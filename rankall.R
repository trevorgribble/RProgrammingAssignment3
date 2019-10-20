## Write a function called rankall that takes two arguments: an outcome name (outcome)
## and a hospital rank- ing (num). The function reads the outcome-of-care-measures.csv file 
## and returns a 2-column data frame containing the hospital in each state that has the 
## ranking specified in num. For example the function call rankall("heart attack", "best") would 
## return a data frame containing the names of the hospitals that are the best in their 
## respective states for 30-day heart attack death rates. The function should return a value 
## for every state (some may be NA). The first column in the data frame is named hospital, 
## which contains the hospital name, and the second column is named state, which contains
## the 2-character abbreviation for the state name. Hospitals that do not have data on a 
##particular outcome should be excluded from the set of hospitals when deciding the rankings.

rankall <- function(outcome, num="best"){
        ## Read Outcome Data
        outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that outcome is valid
        if (!((outcome=="heart attack")||(outcome=="heart failure")||(outcome=="pneumonia"))){
                stop("invalid outcome")
        }
        ## Create output dataframe by checking how many states there are to set dimensions
        validstates<-as.factor(outcomedata$State)
        outputdata <- data.frame(matrix(NA,ncol=2,nrow=length(levels(validstates))))
        colnames(outputdata) <- c("hospital","state")
        
        ## For each state, find the hospital of the given rank
        for (i in 1:length(levels(validstates))){
                ##set the state abbreviation in output dataframe
                outputdata[i,2] <- as.character(levels(validstates)[i])
                #subset the data by state 
                statedata<-subset(outcomedata,State==(levels(validstates)[i]))
                #if outcome is heart attack
                if(outcome=="heart attack"){
                        ## Remove NAs and sort by heart attack Mortality Rates & Alphabetically Hospital Name
                        removeNAsfromlist<-statedata[(!(is.na(as.numeric(statedata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)))),]
                        orderedhospitallist<-removeNAsfromlist[order(as.numeric(removeNAsfromlist$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),
                                                                     removeNAsfromlist$Hospital.Name),]
                        ## Figure out the rank and put the correct hospital in the output dataframe
                        if(num=="best"){
                                outputdata[i,1] <- orderedhospitallist[1,2]
                        }
                        else if(num=="worst"){
                                outputdata[i,1] <- orderedhospitallist[nrow(orderedhospitallist),2]
                        }
                        else{
                                rank <- as.numeric(num)
                                if (rank>nrow(orderedhospitallist)){
                                        outputdata[i,1] <- NA
                                }
                                else{
                                        outputdata[i,1] <- orderedhospitallist[rank,2]
                                }
                        }
                }
                ## If outcome is heart failure
                if(outcome=="heart failure"){
                        ## Remove NAs and sort by heart failure Mortality Rates & Alphabetically Hospital Name
                        removeNAsfromlist<-statedata[(!(is.na(as.numeric(statedata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)))),]
                        orderedhospitallist<-removeNAsfromlist[order(as.numeric(removeNAsfromlist$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),
                                                                     removeNAsfromlist$Hospital.Name),]
                        ## Figure out the rank and put the correct hospital in the output dataframe
                        if(num=="best"){
                                outputdata[i,1] <- orderedhospitallist[1,2]
                        }
                        else if(num=="worst"){
                                outputdata[i,1] <- orderedhospitallist[nrow(orderedhospitallist),2]
                        }
                        else{
                                rank <- as.numeric(num)
                                if (rank>nrow(orderedhospitallist)){
                                        outputdata[i,1] <- NA
                                }
                                else{
                                        outputdata[i,1] <- orderedhospitallist[rank,2]
                                }
                        }
                }
                ## If outcome is pneumonia
                if(outcome=="pneumonia"){
                        ## Remove NAs and sort by pneumonia Mortality Rates & Alphabetically Hospital Name
                        removeNAsfromlist<-statedata[(!(is.na(as.numeric(statedata$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)))),]
                        orderedhospitallist<-removeNAsfromlist[order(as.numeric(removeNAsfromlist$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),
                                                                     removeNAsfromlist$Hospital.Name),]
                        ## Figure out the rank and put the correct hospital in the output dataframe
                        if(num=="best"){
                                outputdata[i,1] <- orderedhospitallist[1,2]
                        }
                        else if(num=="worst"){
                                outputdata[i,1] <- orderedhospitallist[nrow(orderedhospitallist),2]
                        }
                        else{
                                rank <- as.numeric(num)
                                if (rank>nrow(orderedhospitallist)){
                                        outputdata[i,1] <- NA
                                }
                                else{
                                        outputdata[i,1] <- orderedhospitallist[rank,2]
                                }
                        }
                }
        }
        ## Return a data frame with the hospital names and the (abbreviated) state name
        outputdata
}

