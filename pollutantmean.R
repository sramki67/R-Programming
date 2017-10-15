
## Week 2 Programming Assignment - R Programming
pollutantmean <-function(directory,pollutant,id = 1:332)
    {
     x <- length(id)
     filelist <- c()
     
  ## Form the filename
  fileextn <-".csv"
  currentDirectory <-getwd()
  ##dataDirectory <-paste(currentDirectory,"/",directory,"/",sep = "",collapse = NULL)
  for (i in 1:length(id)){
      # Check the length and form the file name. If length < 10 , add "00" and ".csv"
      if(id[i]<10){
          f<-paste("00",as.character(id[i]),fileextn,sep="",collapse=NULL)
          filelist <- c(filelist,f)
      }else if(id[i]>=10 && id[i]<100){
          f<-paste("0",as.character(id[i]),fileextn,sep="",collapse=NULL)
          filelist <- c(filelist,f)
      }else if(id[i]>=100){
          f<-paste(as.character(id[i]),fileextn,sep="",collapse=NULL)
          filelist <- c(filelist,f)
      }
  }
 
  filenames <-list(filelist)
  ## Create a data frame of the data from input files to one data set
  newdf <-data.frame(do.call("rbind",lapply(filelist,read.csv,header=TRUE)))
  newdf
  #extract the values of the required pollutant ( ignore NA values) by using the input pollutant
  goodVal <- complete.cases(newdf)
  goodVal
  mean(newdf$nitrate[goodVal])
  
  if (pollutant == "sulfate"){
      mean(newdf$sulfate[goodVal])
      
  }else if (pollutant == "nitrate"){
      mean(newdf$nitrate[goodVal])
  }
  
  
  
  
 
    }
  ##filelist
    
  
  
 

