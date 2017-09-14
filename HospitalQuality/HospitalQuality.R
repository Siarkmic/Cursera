

setwd("C:/Users/siarkmi2/Documents/GitHub/Cursera/HospitalQuality")

# Loading data
Hosp.raw <- read.csv("hospital-data.csv", stringsAsFactors = FALSE)
View(Hosp.raw)

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack<-as.numeric(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack) 
outcome$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
head(outcome)
#View(outcome)

# outcome[, 11] <- as.numeric(outcome[, 11])
## You may get a warning about NAs being introduced; that is okay
# hist(outcome[, 11])


best <- function(state, Outcome) {
        
        # IF for choosing columns
        myOutcome <-Outcome
        MyState <-state
        colSel <- ""
        if(myOutcome == "heart attack")         {colSel <- 11
        } else if(myOutcome == "heart failure") {colSel <- 17
        } else if(myOutcome == "pneumonia")     {colSel <- 23
        }
        
        ## Read outcome data
        Hosp <- subset(outcome, State == MyState, select = c(Hospital.Name, colSel)) 
        ## Check that state and outcome are valid
        
        ## Return hospital name in that state with lowest 30-day death rate
        Hosp.sort <- Hosp[order(Hosp[2], Hosp[1]),]
        Hosp.ret <- Hosp.sort[1,1]
        return(Hosp.ret)
}

best("TX", "heart attack")
#"CYPRESS FAIRBANKS MEDICAL CENTER"
 best("TX", "heart failure")
#[1] "FORT DUNCAN MEDICAL CENTER"
 best("MD", "heart attack")
#[1] "JOHNS HOPKINS HOSPITAL, THE"
 best("MD", "pneumonia")
#[1] "GREATER BALTIMORE MEDICAL CENTER"
 
 
 
 rankhospital <- function(state, Outcome, num = "best") {
         # IF for choosing columns
         myOutcome <-Outcome
         MyState <-state
         colSel <- ""
         
         if(myOutcome == "heart attack")         {colSel <- 11
         } else if(myOutcome == "heart failure") {colSel <- 17
         } else if(myOutcome == "pneumonia")     {colSel <- 23
         }
        
         ## Read outcome data
         Hosp <- subset(outcome, State == MyState, select = c(Hospital.Name, colSel)) 
         
         ## Check that state and outcome are valid
         
         ## Return hospital name in that state with the given rank
         Hosp <- Hosp[complete.cases(Hosp),]
         Hosp.sort <- Hosp[order(Hosp[2], Hosp[1]),]
         
         if(num == "worst") {myNum <- NROW(Hosp.sort) #last
         } else if (num=="best") {myNum <- 1          #first
         } else {myNum <- num                         #declared
         }
         
         Hosp.ret <- Hosp.sort[myNum,1]
         return(Hosp.ret)
         ## 30-day death rate
 }
 
 rankhospital("MD", "heart failure", 5)
 
 rankhospital("TX", "heart failure", 4)
  "DETAR HOSPITAL NAVARRO"
 rankhospital("MD", "heart attack", "worst")
 
  "HARFORD MEMORIAL HOSPITAL"
 rankhospital("MN", "heart attack", 5000)
 NA
 
 ###########3
 
 rankall <- function(Outcome, num = "best") {
         # IF for choosing columns
         myOutcome <-Outcome
         colSel <- ""
         
         if(myOutcome == "heart attack")         {colSel <- 11
         } else if(myOutcome == "heart failure") {colSel <- 17
         } else if(myOutcome == "pneumonia")     {colSel <- 23
         }
         require(dplyr)
         
         ## Read outcome data
         Hosp <- subset(outcome, select = c(State, Hospital.Name, colSel)) 
         
         ## Check that state and outcome are valid
         Hosp.sort <- Hosp %>% 
                 arrange(State, Hosp[3], Hosp[2] ) %>%
                 group_by(State) %>%
                 mutate(rank=row_number())
         print.data.frame(sorted)
         
         if(num == "worst") {myNum <- NROW(Hosp.sort) #last
         } else if (num=="best") {myNum <- 1          #first
         } else {myNum <- num                         #declared
         }
         
         Hosp.ret <- Hosp.sort[ which(Hosp.sort$rank==myNum), ]
         return(Hosp.ret)
         
         ## For each state, find the hospital of the given rank
         ## Return a data frame with the hospital names and the
         ## (abbreviated) state name
 }
 
 head(rankall("heart attack", 20), 10)
 
 
 
 
 
 