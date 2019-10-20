## Write a function called rankhospital that takes three arguments: 
## the 2-character abbreviated name of a state (state), 
## an outcome (outcome), and the ranking of a hospital in that state for that outcome (num). 
## The function reads the outcome-of-care-measures.csv file and returns a 
## character vector with the name of the hospital that has the ranking specified by 
## the num argument

rankhospital <- function(state, outcome, num="best"){
        ## Read Outcome Data
        outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that state and outcome are valid
        validstates<-as.factor(outcomedata$State)
        if (!(state%in%validstates)){
                stop("invalid state")
        }
        
        if (!((outcome=="heart attack")||(outcome=="heart failure")||(outcome=="pneumonia"))){
                stop("invalid outcome")
        }
        ## Return hospital name in that state with given rank in 30 day death rate
        ## subset the dataframe by input state
        statedata<-subset(outcomedata,State==state)
        ## if heart attack
        if(outcome=="heart attack"){
                ##remove NAs and order the hospital list by heart attack death stats & hospital name alphabetically
                removeNAsfromlist<-statedata[(!(is.na(as.numeric(statedata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)))),]
                orderedhospitallist<-removeNAsfromlist[order(as.numeric(removeNAsfromlist$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),
                                                             removeNAsfromlist$Hospital.Name),]
                ##based on input rank, return appropriate hospital name
                if(num=="best"){
                        orderedhospitallist[1,2]
                }
                else if(num=="worst"){
                        orderedhospitallist[nrow(orderedhospitallist),2]
                }
                else{
                        rank <- as.numeric(num)
                        if (rank>nrow(orderedhospitallist)){
                                NA
                        }
                        else{
                                orderedhospitallist[rank,2]
                        }
                }
        }
        else if(outcome=="heart failure"){
                ##remove NAs and order the hospital list by heart failure death stats & hospital name alphabetically
                removeNAsfromlist<-statedata[(!(is.na(as.numeric(statedata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)))),]
                orderedhospitallist<-removeNAsfromlist[order(as.numeric(removeNAsfromlist$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),
                                                             removeNAsfromlist$Hospital.Name),]
                ##based on input rank, return appropriate hospital name
                if(num=="best"){
                        orderedhospitallist[1,2]
                }
                else if(num=="worst"){
                        orderedhospitallist[nrow(orderedhospitallist),2]
                }
                else{
                        rank <- as.numeric(num)
                        if (rank>nrow(orderedhospitallist)){
                                NA
                        }
                        else{
                                orderedhospitallist[rank,2]
                        }
                }
        }
        else if(outcome=="pneumonia"){
                ##remove NAs and order the hospital list by pneumonia death stats & hospital name alphabetically
                removeNAsfromlist<-statedata[(!(is.na(as.numeric(statedata$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)))),]
                orderedhospitallist<-removeNAsfromlist[order(as.numeric(removeNAsfromlist$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),
                                                             removeNAsfromlist$Hospital.Name),]
                
                ##based on input rank, return appropriate hospital name
                if(num=="best"){
                        orderedhospitallist[1,2]
                }
                else if(num=="worst"){
                        orderedhospitallist[nrow(orderedhospitallist),2]
                }
                else{
                        rank <- as.numeric(num)
                        if (rank>nrow(orderedhospitallist)){
                                NA
                        }
                        else{
                                orderedhospitallist[rank,2]
                        }
                }
        }
}