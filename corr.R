## Corr.r - Program to find Correlation of Air Quality Data

corr <- function(directory,threshhold = 0){
    ## Get the directory and threshold
    ## IF the threshold is default value of 0 then we need to process all the files
    ## in the directory. If the value = 150 then we only need to process those files with
    ## nobs = 150 by calling complete function.
    ## First call the complete.r to capture all the complete observations
    ## to a data frame. Parse the data frame for the file-id's equal to the input
    ## threshold value.
    ## Create a data frame of these input files only
    ## Run the coreelation function on the combined dataframe.
    
    source("complete.r")
    c <- complete("specdata",1:332)
    t <- threshhold
    lst <- c()
    for (i in 1:nrow(c)){
        if (c$nobs[i] > t){
            id <- c$id[i]
            lst <- c(lst,c$id[i])
         }
        
    }
    
    fileextn <- ".csv"
    filelist <- c()
    workingDir <- as.character(directory)
    cDir <- getwd()
    D <- paste(cDir,"/",workingDir,sep="",collapse=NULL)
    setwd(D)
    cr <- vector(mode="numeric",length=0)
    
    for (i in 1:length(lst)){
        # Check the length and form the file name. If length < 10 , add "00" and ".csv"
        if(length(lst)== 0){
            cr
        }
        else if(lst[i]<10){
            f<-paste("00",as.character(lst[i]),fileextn,sep="",collapse=NULL)
            filelist <- c(filelist,f)
            newdf <- data.frame(read.csv(f),header=TRUE)
            cv <- cor(x=newdf$sulfate,y=newdf$nitrate,use="complete.obs")
            cr <- c(cr,cv)
            
            
            
        }else if(lst[i]>=10 && lst[i]<100){
            f<-paste("0",as.character(lst[i]),fileextn,sep="",collapse=NULL)
            filelist <- c(filelist,f)
            newdf <- data.frame(read.csv(f),header=TRUE)
            cv <- cor(x=newdf$sulfate,y=newdf$nitrate,use="complete.obs")
            cr <- c(cr,cv)
        }else if(lst[i]>=100){
            f<-paste(as.character(lst[i]),fileextn,sep="",collapse=NULL)
            filelist <- c(filelist,f)
            newdf <- data.frame(read.csv(f),header=TRUE)
            cv <- cor(x=newdf$sulfate,y=newdf$nitrate,use="complete.obs")
            cr <- c(cr,cv)
        }
    }
    ##print(filelist)
    setwd("..")
    cr
    ##newdf <-data.frame(do.call("rbind",lapply(filelist,read.csv,header=TRUE)))
    ##setwd("..")
    ##cor(x=newdf$sulfate,y=newdf$nitrate,use="complete.obs")
    

}