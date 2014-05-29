remove(list=ls())
?vector
u1 <- c(1,3,4)
u2 <- c(5,5)

str(append(u1,u2))
?vapply
data_hospital <- read.csv("hospital-data.csv",header=TRUE,stringsAsFactors=FALSE)
data_outcome <- read.csv("outcome-of-care-measures.csv",header=TRUE,stringsAsFactors=FALSE)
y<-colnames(data_outcome)
str(y)
grep
?which
?
k<-unlist(strsplit("heart attack", " "))
k[1]
?subset
grep(paste(k[1],k[2],sep="."),y,ignore.case=TRUE)
&& grep("hospital.30.day",y,ignore.case=TRUE)
paste(k, sep ="/",collapse =".")
?grep
?paste
?colNames
?tapply
data_mortality_rate <- as.numeric(data_outcome[,11])
?hist
?strsplit
hist(data_mortality_rate)
?pmatch
g<-data_outcome$State=="IL"
library(sqldf)
?grep
?min
?sort
?stop
library(stats)
?append
z<- nrow(c("c","a","b"))

best <- function(state=character(),disease = character()){
  outcome_by_state <- subset(data_outcome, State == state)
  
  #str(outcome_by_state)
  if(nrow(outcome_by_state)!=0){
  disease_name <- paste(unlist(strsplit(disease, " ")),collapse =".*")
  outcome_name <- paste("^Hospital.30.Day.Death..Mortality..Rates.from",disease_name,"$",sep=".*")
  columnNames <- colnames(outcome_by_state)
  ii<- grep(outcome_name,columnNames,ignore.case=TRUE,value = TRUE)
  #print(ii)
  print(length(ii))
  if(length(ii)==1 ){
  outcome_by_state[,ii]<- as.numeric(outcome_by_state[,ii])
  minm_rate <- min(outcome_by_state[,ii],na.rm=TRUE)
  best_hospital_names <- sort(subset(outcome_by_state,outcome_by_state[,ii]==minm_rate)$Hospital.Name)
  print(best_hospital_names[1])
  }

  else{
    stop("invalid outcome")
  }
  
  }
  else {
    stop("invalid state")
  }
}

best("MD","pneumo")


str(as.numeric(data_outcome[,11],rm.na))

s<-order(data_outcome, as.numeric(data_outcome[,11]))

rankhospital <- function(state=character(),disease = character(),rank ){
  outcome_by_state <- subset(data_outcome, State == state)
  
  #str(outcome_by_state)
  if(nrow(outcome_by_state)!=0){
    disease_name <- paste(unlist(strsplit(disease, " ")),collapse =".*")
    outcome_name <- paste("^Hospital.30.Day.Death..Mortality..Rates.from",disease_name,"$",sep=".*")
    columnNames <- colnames(outcome_by_state)
    ii<- grep(outcome_name,columnNames,ignore.case=TRUE,value = TRUE)
    #print(ii)
    #print(length(ii))
    if(length(ii)==1 ){
      outcome_by_state[,ii]<- as.numeric(outcome_by_state[,ii])
      mortality_rates <- unique(sort(outcome_by_state[,ii]))
      ranked_hospital <- character()
      
      
      #arrange_hospital <- function(rate){
       # best_hospital_names <- sort(subset(outcome_by_state,outcome_by_state[,ii]==rate)$Hospital.Name)
        #str(best_hospital_names)
      #  ranked_hospital <- c(ranked_hospital,best_hospital_names)
       # str(ranked_hospital)
      #  return(ranked_hospital)
      #}
      #hospital_ranked <- sapply(mortality_rates, arrange_hospital)
      #str(hospital_ranked)
      for (i in 1:length(mortality_rates)){
        best_hospital_names <- sort(subset(outcome_by_state,outcome_by_state[,ii]==mortality_rates[i])$Hospital.Name)
        ranked_hospital <- c(ranked_hospital,best_hospital_names)
      }
      str(ranked_hospital)
      
      if(rank == "best"){
        rank <-1
      }
      else if (rank == "worst"){
        rank <- length(ranked_hospital)
      }
      else
      {
        rank <-rank
      }
      if(length(ranked_hospital) >= rank)
        {
      print(ranked_hospital[rank])
        
      }
      else{
      print("NA")
      }
    }
    
    else{
      stop("invalid outcome")
    }
    
  }
  else {
    stop("invalid state")
  }
}

rankhospital("MN", "heart attack", 5000)
