# Project-Programming-Assignment-3-Coursera-Part1
#Getting the best hospital with least mortality rate in the 30 days 
best<-function(state,outcome)
{
  #Reading the outcomes of the file 
  data<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
  fd<-as.data.frame(cbind(data[,2],#Column with hospital data 
                          data[,7],#Column with state data
                          data[,11],#Column with heart attack data
                          data[,17],#Column with heart failure data     
                          data[,23]),#Column with Pneumonia data)
                          stringsAsFactors = FALSE)
  colnames(fd)<-c("hospital","state","heart attack","heart failure","pneumonia")
  #Now putting conditions on state and hospital and validating to get the result
  if (!state %in% fd[,state])
  {
    stop('Invalid Input')
  }else if(!outcome %in% c("heart attack","heart failure","pneumonia"))
  {
    stop('Invalid Input')
  }else 
  {
    state_value<-which(fd[,"state"]==state)
    #Extracting all the data values as per condition on called state
    data2<-fd[state_value,]
    evaluated<-as.numeric(ts[,eval(outcome)])
    min_value<-data2[evaluated,na.rm=TRUE]
    result<-data2[,"hospital"][which(evaluated==min_value)]
    output<-result[order(result)]
  }
return(output)
}
# example output:
best("SC", "heart attack") 
