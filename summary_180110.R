# churn/from/to format
formatMatrix<-read.table("/home/dongjin86/r_home/2018_MA/dashboard/fbData/test/format.txt",sep="\t",header=FALSE,fill=TRUE,stringsAsFactors = FALSE)

# data file list
csvFolderName<-list.files("/home/dongjin86/r_home/2018_MA/dashboard/fbData/test/data", full.names=TRUE)
csvAllData=data.frame()
dateList=c()
regionList=c()

# combine csv data 
for (i in 1:length(csvFolderName)){
  csvData<-read.csv(csvFolderName[i],sep=",",header=TRUE,fill=TRUE,stringsAsFactors = FALSE)
  nameList=unlist(strsplit(csvFolderName[i],'/'))
  filename=nameList[length(nameList)]
  infoList=unlist(strsplit(filename,'_'))
  churn=infoList[1]
  region=infoList[2]
  datetime=infoList[3]
  
  csvData=cbind(csvData,churn,region,datetime)
  csvAllData=rbind(csvAllData, csvData)
  dateList=c(dateList,datetime)
  regionList=c(regionList,region)
}

dateList=unique(dateList)
regionList=unique(regionList)
csvAllData<-csvAllData[,c(3,4,5,6,7,8)]

flagshipModels=c('Galaxy S7','Galaxy S7 Edge','Galaxy S8','Galaxy S8+','Galaxy Note 8')

s7N_1Models=c('Galaxy S6','Galaxy S6 Edge','Galaxy S6 Edge Plus','Galaxy Note 5')
s7N_23Models=c('Galaxy S5','Galaxy S4','Galaxy Note 4','Galaxy Note Edge','Galaxy Note 3')
s8N_1Models=c('Galaxy S7','Galaxy S7 Edge')
s8N_23Models=c('Galaxy S6','Galaxy S6 Edge','Galaxy S6 Edge Plus','Galaxy Note 5','Galaxy S5','Galaxy Note 4','Galaxy Note Edge')
n8N_1Models=c('Galaxy S7','Galaxy S7 Edge','Galaxy S8','Galaxy S8+')
n8N_23Models=c('Galaxy S6','Galaxy S6 Edge','Galaxy S6 Edge Plus','Galaxy Note 5','Galaxy S5','Galaxy Note 4','Galaxy Note Edge')

for (j in 1:length(regionList)) {
  currentRegion=regionList[j]
  newFormat=cbind(formatMatrix,currentRegion)
  colnames(newFormat)<-c('churn','from','to','region')
  
  for (k in 1:length(dateList)) {
    currentDate=dateList[k]
    
    tCsvAllData<-csvAllData[csvAllData$datetime==currentDate & csvAllData$region==currentRegion,]
    colnames(tCsvAllData)<-c('from','to',currentDate,'churn','region')
    tCsvAllData<-tCsvAllData[,c(1,2,3,4,5)]
    
    newFormat<-merge(newFormat,tCsvAllData,by= c('churn','from','to','region'),all.x=TRUE)
    
    for ( l in 1:length(flagshipModels)){
      currentModel<-flagshipModels[l]
      
      samsung_to_flagship<-tCsvAllData[tCsvAllData$churn=='joined' & tCsvAllData$from=='Samsung' &  tCsvAllData$to==flagshipModels[l],]
      if (nrow(samsung_to_flagship) == 0){
        next
      }
      
      if (currentModel=='Galaxy S7' | currentModel=='Galaxy S7 Edge'){
        exModels<-unique(c(s7N_23Models, s7N_1Models))
      } else if (currentModel=='Galaxy S8' | currentModel=='Galaxy S8+'){
        exModels<-unique(c(s8N_23Models, s8N_1Models))
      } else 
        exModels<-unique(c(n8N_23Models, n8N_1Models))
      
      flagship_to_flagship_t<-tCsvAllData[tCsvAllData$churn=='joined' &  tCsvAllData$to==flagshipModels[l],]
      flagship_to_flagship<-flagship_to_flagship_t[which(flagship_to_flagship_t$from %in% exModels),]
      exModelsSum<-sum(flagship_to_flagship[flagship_to_flagship$to==currentModel,currentDate])
      
      otherSamsungPortion=samsung_to_flagship[,currentDate]-exModelsSum
      applePortion=tCsvAllData[tCsvAllData$churn=='joined' & tCsvAllData$from=='Apple' &  tCsvAllData$to==flagshipModels[l],]
      otherAndroidPortion=1-applePortion[,currentDate]-samsung_to_flagship[,currentDate]
      
      newFormat[newFormat$churn=='joined' & newFormat$from=='Other Android' & newFormat$to==currentModel & newFormat$region==currentRegion, currentDate] = otherAndroidPortion
      newFormat[newFormat$churn=='joined' & newFormat$from=='Other Samsung' & newFormat$to==currentModel & newFormat$region==currentRegion, currentDate] = otherSamsungPortion
    }
  }
  newFormat[is.na(newFormat)]<-0
  write.csv(newFormat,paste("/home/dongjin86/r_home/2018_MA/dashboard/fbData/test/result/",currentRegion,".csv", sep=""), row.names = FALSE)
}
