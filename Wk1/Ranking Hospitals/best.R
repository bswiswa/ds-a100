# create a function that returns the best hospital in a given state 
# for a given condition/outcome
# make sure the csv files are saved in the same directory as best.R

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

# function to find the best hospital for a given state and outcome
best <- function(state, outcome){
  if((wrong.state(state) & wrong.outcome(outcome))){
    stop("invalid state and outcome")
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

# sort by mortality rate and hospital name
  min.mortality.rate <- min(state.hospitals$mortality.rate)
  best.hospitals <- state.hospitals$name[state.hospitals$mortality.rate == min.mortality.rate]
  sort(best.hospitals)
# return the first, best hospital
  return(best.hospitals[1])
}


