# create a function rankhospital that takes three arguments: 
# the 2-character abbreviated name of a state (state), 
# an outcome (outcome), 
# and the ranking of a hospital in that state for that outcome (rank). 
# The function reads the outcome-of-care-measures.csv ﬁle and returns
# a character vector with the name of the hospital that has the ranking 
# speciﬁed by the rank argument

# get data
all_outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
# get states
require(datasets)
data(state)

# function to check whether state is wrong
wrong.state <- function(abb){
  count <- sapply(state.abb, function(x) x == abb)
  ifelse(sum(count) < 1, return(T), return(F))
}

# function to check whether outcome is wrong
wrong.outcome <- function(outc){
  count <- sapply(c("heart attack", "heart failure", "pneumonia"),
                  function(x) x == outc)
  ifelse(sum(count) < 1, return(T), return(F))
}

#function to check whether the rank is wrong
wrong.rank <- function(r){
  if(r == "best" | r == "worst" | (class(r) == "numeric" & length(r) == 1)){
    return(F)
  }
  else{
    return(T)
  }
}

# function to find the best hospital for a given state and outcome
rankhospital <- function(state, outcome, rank = "best"){
  if((wrong.state(state) & wrong.outcome(outcome) & wrong.rank(rank))){
    stop("invalid state, outcome and rank")
  }
  else if(wrong.state(state) & wrong.outcome(outcome)){
    stop("invalid state and outcome")
  }
  else if(wrong.state(state) & wrong.rank(rank)){
    stop("invalid state and rank")
  }
  else if(wrong.outcome(outcome) & wrong.rank(rank)){
    stop("invalid rank and outcome")
  }
  else if(wrong.rank(rank)){
    stop("invalid rank")
  }
  else if(wrong.state(state)){
    stop("invalid state")
  }
  else if(wrong.outcome(outcome)){
    stop("invalid outcome")
  }
  
  if(outcome == "heart attack") column.number <- 11
  if(outcome == "heart failure") column.number <- 17
  if(outcome == "pneumonia") column.number <- 23
  
  hosp.name <- 2
  # get all hospitals from that state AND only the name and outcome column
  state.hospitals <- all_outcomes[all_outcomes$State == state,c(hosp.name, column.number)] 
  names(state.hospitals) <- c("name", "mortality.rate")
  
  # coerce values to numeric (and suppress warnings)
  state.hospitals$mortality.rate <- suppressWarnings(as.numeric(state.hospitals$mortality.rate))
  # remove hospitals with NAs
  state.hospitals <- state.hospitals[!is.na(state.hospitals$mortality.rate), ]
  # sort by hospital name and by mortality rate and only save names
  ranked.hospitals <- state.hospitals[order(state.hospitals$mortality.rate, state.hospitals$name),]
  hospital.names <- ranked.hospitals$name
  
  # return according to the specified rank options - "best", "worst" or rank#
  if(rank == "best"){
    return(ranked.hospitals$name[1])
  }
  else if(rank == "worst"){
    return(ranked.hospitals$name[length(ranked.hospitals$name)])
  }
  else {
    return(ranked.hospitals$name[rank])
  }
    
}
# testing results - command followed by expected result

# rankhospital("TX", "heart attack")
# [1] "CYPRESS FAIRBANKS MEDICAL CENTER"

# rankhospital("TX", "heart failure")
# [1] "FORT DUNCAN MEDICAL CENTER"


# rankhospital("MD", "heart attack")
# [1] "JOHNS HOPKINS HOSPITAL, THE"

# rankhospital("MD", "pneumonia")
# [1] "GREATER BALTIMORE MEDICAL CENTER"

# rankhospital("BB", "heart attack")
# Error in best("BB", "heart attack") : invalid state

# rankhospital("NY", "hert attack") 
# Error in best("NY", "hert attack") : invalid outcome

# rankhospital("TX", "heart failure", 4)
# [1] "DETAR HOSPITAL NAVARRO"

# rankhospital("MD", "heart attack", "worst")
# [1] "HARFORD MEMORIAL HOSPITAL"

# rankhospital("MN", "heart attack", 5000)
# [1] NA
