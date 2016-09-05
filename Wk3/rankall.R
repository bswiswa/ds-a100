# Write a function called rankall that takes two arguments: 
#   an outcome name (outcome) and a hospital ranking (rank). 
# The function reads the outcome-of-care-measures.csv ﬁle and returns a 2-column data frame
# containing the hospital in each state that has the ranking speciﬁed in num. 

# get data
hosp.data <- read.csv("../Wk2/Ranking Hospitals - cont/outcome-of-care-measures.csv", colClasses = "character")

# function to check whether outcome is wrong
wrong.outcome <- function(outc){
  return(!outc %in% c("heart attack", "heart failure", "pneumonia"))
}

#function to check whether the rank is wrong
wrong.rank <- function(r){
  if(length(r) != 1) return(T)
  else if(r == "best" | r == "worst" | class(r) == "numeric" ) return(F)
  else return(T)
}

rankall <- function(outcome, rank="best"){
  # data validation
  if(wrong.outcome(outcome) & wrong.rank(rank)) stop("invalid outcome and rank")
  else if(wrong.outcome(outcome)) stop("invalid outcome")
  else if(wrong.rank(rank)) stop("invalid rank")
  
  # get column names
  if(outcome == "heart attack") outcome.column <- 11
  if(outcome == "heart failure") outcome.column <- 17
  if(outcome == "pneumonia") outcome.column <- 23
  
  states <- unique(hosp.data$State)
  best.hosp.matrix <- sapply(states, 
                      function(x){
                      # get all hospitals in a state and exclude those with NAs
                      # add an outcome column to the frame
                      hosp.data$outcome <- suppressWarnings(as.numeric(as.character(hosp.data[,outcome.column])))
                      state.hosp <- subset(hosp.data[!is.na(hosp.data$outcome), ],
                                                State == x, 
                                                select = c(Hospital.Name, State, outcome))

                      # rank them, order and only return name and state columns
                      ranked.state.hosp <- subset(state.hosp[order(state.hosp$outcome, 
                                                                   state.hosp$Hospital.Name),],
                                                  select = c(Hospital.Name, State))
                      
                      # handling the rank argument
                      index <- rank
                      if(rank == "best") index <- 1
                      else if(rank == "worst") index <- dim(ranked.state.hosp)[1]
                      
                      return(ranked.state.hosp[index,])
                      })
         
  # rearrange and name it
    best.hosp.matrix <- t(best.hosp.matrix)
  # make state column have all the state values, which are now in the row names
    best.hosp.matrix[,2] <- row.names(best.hosp.matrix)
  # create a data frame from matrix 
    best.hosp.frame <- as.data.frame(best.hosp.matrix)
  # make the columns of the frame vectors
    best.hosp.frame <- as.data.frame(lapply(best.hosp.frame, unlist), stringsAsFactors = FALSE)
  # change the column names
    names(best.hosp.frame) <- c("hospital", "state")
  # sort columns
  best.hosp.frame <- best.hosp.frame[order(best.hosp.frame$state),]
  
  return(best.hosp.frame)
}
