remove(list=ls())

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
z<- sort(c("c","a","b"))

best <- function(state=character(),disease = character()){
  outcome_by_state <- subset(data_outcome, State = "IL")
  #summary(outcome_by_state)
  disease_name <- paste(unlist(strsplit(disease, " ")),collapse =".*")
  outcome_name <- paste("^Hospital.30.Day.Death..Mortality..Rates.from",disease_name,"$",sep=".*")
  #print(outcome_name)
  columnNames <- colnames(outcome_by_state)
  ii<- grep(outcome_name,columnNames,ignore.case=TRUE)
  outcome_by_state[,ii]<- as.numeric(outcome_by_state[,ii])
  minm_rate <- min(outcome_by_state[,ii],na.rm=TRUE)
  #print(minm_rate)
  best_hospital_names <- sort(subset(outcome_by_state,outcome_by_state[,ii]==minm_rate)$Hospital.Name)
  #hospital.name <- b
  print(best_hospital_names[1])
  
  #outcome_by_state[,outcome_by_state$outcome_name]<- as.numeric(outcome_by_state[,outcome_by_state$outcome_name])
  #print(min(outcome_by_state$outcome_name))
  #best_hospital_index <- which(outcome_by_state$outcome_name)= min(outcome_by_state[,outcome_by_state$outcome_name])
  #best_hospital_name <- outcome_by_state$Hospital.Name[best_hospital_index]
  #print(best_hospital_name)
  
}

best("IL", "heart attack")
