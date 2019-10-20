## Write a function called best that take two arguments: the 2-character abbreviated 
## name of a state and an outcome name. The function reads the outcome-of-care-measures.csv 
## file and returns a character vector with the name of the hospital that has the 
## best (i.e. lowest) 30-day mortality for the specified outcome in that state. 
## The hospital name is the name provided in the Hospital.Name variable. 
## The outcomes can be one of “heart attack”, “heart failure”, or “pneumonia”. 
## Hospitals that do not have data on a particular outcome should be excluded 
## from the set of hospitals when deciding the rankings.

## Handling ties. If there is a tie for the best hospital for a given outcome, 
## then the hospital names should be sorted in alphabetical order and the first 
## hospital in that set should be chosen (i.e. if hospitals “b”, “c”, and “f” are tied 
## for best, then hospital “b” should be returned).

best <- function(state, outcome){
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
        ## Return hospital name in that state with lowest 30 day death rate
        ## subset the data by input state
        statedata<-subset(outcomedata,State==state)
        ## if outcomes is heart attack
        if(outcome=="heart attack"){
                ##find the lowest heart attack death rate
                lowestdeathrate<-min(as.numeric(statedata[,11]), na.rm=TRUE)
                ##subset the list of hospitals that might share the lowest death rate
                lowesthospitallist<-subset(statedata,(as.numeric(statedata[,11])==lowestdeathrate))
                ##alphabetically order the hospitals with the lowest death rate
                alphalowesthospitallist <- lowesthospitallist[order(11),]
                ##return the first hospital in the list
                alphalowesthospitallist[1,2]
        }
        ## if outcome is heart failure
        else if(outcome=="heart failure"){
                ##find the lowest heart failure death rate
                lowestdeathrate<-min(as.numeric(statedata[,17]), na.rm=TRUE)
                ##subset the list of hospitals that might share the lowest death rate
                lowesthospitallist<-subset(statedata,(as.numeric(statedata[,17])==lowestdeathrate))
                ##alphabetically order the hospitals with the lowest death rate
                alphalowesthospitallist <- lowesthospitallist[order(17),]
                ##return the first hospital in the list
                alphalowesthospitallist[1,2]
        }
        ## if outcome is pneumonia
        else if(outcome=="pneumonia"){
                ##find the lowest pneumonia death rate
                lowestdeathrate<-min(as.numeric(statedata[,23]), na.rm=TRUE)
                ##subset the list of hospitals that might share the lowest death rate
                lowesthospitallist<-subset(statedata,(as.numeric(statedata[,23])==lowestdeathrate))
                ##alphabetically order the hospitals with the lowest death rate
                alphalowesthospitallist <- lowesthospitallist[order(23),]
                ##return the first hospital in the list
                alphalowesthospitallist[1,2]
        }
}