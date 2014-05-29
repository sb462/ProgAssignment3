remove(list = ls())
?read.csv 
?unique
data_outcome <- read.csv("outcome-of-care-measures.csv",header=TRUE,stringsAsFactors = FALSE)


rankall <- function(outcome=character(),num= "best"){
  data_outcome <- read.csv("outcome-of-care-measures.csv",header=TRUE,stringsAsFactors = FALSE)
  state_vector <- unique(data_outcome$State)
  disease_name <- paste(unlist(strsplit(outcome, " ")),collapse =".*")
  outcome_name <- paste("^Hospital.30.Day.Death..Mortality..Rates.from",disease_name,"$",sep=".*")
  columnNames <- colnames(data_outcome)
  ii<- grep(outcome_name,columnNames,ignore.case=TRUE,value = TRUE)
  rank_state <- function(outcome= character(),state=character(),num){
    state_mortality_vec <- 
  }
  
  return(state_vector)
}

