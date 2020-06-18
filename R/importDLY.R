#' Import Canadian Snow Data from .dly file
#' @param fileLoc File path to .dly data
#' @param progress boolean spesifing if you want progress of code to be printed out
#' @return nicely organized dataframe of snow data
#' @export
importDLY<-function(fileLoc,progress=FALSE){
  SnowDataUpdated <- read.delim(file = fileLoc, header=FALSE, stringsAsFactors=FALSE)
  SnowDataUpdated<-SnowDataUpdated$V1
  monthlymat<-matrix("",nrow = 31,ncol = 6)
  accumulatedmat<-c()
  FinalOutput<-c()
  monthlymat[,6]=as.character(1:31)
  len<-length(SnowDataUpdated)
  for(i in 1:len){
    curstr<-SnowDataUpdated[i]
    if(str_length(curstr)==77){
      if(i>1){
        FinalOutput<-rbind(FinalOutput,cbind(id,Name,Lat,Lon,Elev,Sdate,Edate,Nobs,accumulatedmat))
        accumulatedmat<-c()
        if(progress){
          print(paste(i,"of",len,"is complete."))
        }
      }
      id=.Internal(substr(curstr,1L,7L))
      Name=.Internal(substr(curstr,9L,38L))
      Lat=.Internal(substr(curstr,40L,45L))
      Lon=.Internal(substr(curstr,47L,53L))
      Elev=.Internal(substr(curstr,55L,58L))
      Sdate=.Internal(substr(curstr,60L,65L))
      Edate=.Internal(substr(curstr,67L,72L))
      Nobs=.Internal(substr(curstr,74L,77L))
    } else{
      monthlymat[,1]=.Internal(substr(curstr,9L,12L))
      monthlymat[,2]=.Internal(substr(curstr,13L,14L))

      for(j in 1:31){
        cur<-as.integer((j-1)*10)
        monthlymat[j,3]<-.Internal(substr(curstr,cur+16L,cur+18L))
        monthlymat[j,4]<-.Internal(substr(curstr,cur+20L,cur+22L))
        monthlymat[j,5]<-.Internal(substr(curstr,cur+24L,cur+24L))
      }
      accumulatedmat<-rbind(accumulatedmat,monthlymat)
    }
  }
  data.frame(id=FinalOutput[,1],Name=FinalOutput[,2],Lat=as.numeric(FinalOutput[,3]),Lon=as.numeric(FinalOutput[,4]),Elev=as.numeric(FinalOutput[,5]),Sdate=as.numeric(FinalOutput[,6]),Edate=as.numeric(FinalOutput[,7]),Nobs=as.numeric(FinalOutput[,8]),Year=as.numeric(FinalOutput[,9]),Month=as.numeric(FinalOutput[,10]),SnowDepth=as.numeric(FinalOutput[,11]),QualityFlag=as.numeric(FinalOutput[,12]),ClimateFlag=FinalOutput[,13])
}
