# Second Function Complete.R
# Takes an input of DIrectory and a vector of file id(s)
# Processes the complete observations (count of the input file)
complete <- function(directory,id=1:332)
{
   # First get the directory of the files
    # get the current directory and set the directory to the required directory
    workingDir <- as.character(directory)
    workingDir
    cDir <- getwd()
    cDir
    D <- paste(cDir,"/",workingDir,sep="",collapse=NULL)
    idList <- c() ## Empty list to store the file id. 
    obsList <- c() ## Empty lists to store Good observations
    df1 <- data.frame() ## Create an empty data frame to add the data in the final step
    setwd(D)
    # Now we are in the right directory, we need to process the files
    # The input is a integer vector and we need to form the required file names
    fileextn <-".csv"
    # Check how many files to process and get this number
    numfiles <- length(id)
    numfiles
    filelist <- c()
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
    
    ## All the required files are stored in a list.
    ## Now we need to process the complete records ofnobs< each of the file into a vector
    
    for (i in 1:length(filelist)){
        df <- data.frame(read.csv(as.character(filelist[i]),header=TRUE))
        good <- complete.cases(df)
        nobs<- sum(good)
       ##print (filelist[i])
       # First we create a separate list of ids and nobs
       idList <-c(idList,id[i])
       obsList <- c(obsList,nobs)
    }
    setwd("..")
    df1 <-data.frame(id = idList,nobs=obsList)
    
}