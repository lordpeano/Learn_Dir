rankhospital <- function(state, outcome, num = "best") 
{
      #
      # two letter postal code for state (state)
      # an outcome (outcome) from "heart attack","heart failure", "pneumonia"
      # num is to return the hospital in that state which ranks the number for that outcome
      # ties are broken based on hospital alphabetical order
      #
      
      ## Read outcome data
      
      # Read Data
      my_outcome<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
      
      ## Check that state and outcome are valid
      if(sum(state==factor(my_outcome[,7], exclude="NA"))==0)
            {
                  stop("invalid state")
                  
            }# end state check
      #set up means to determine outcome column
      
      my_out<-rbind(type_prob=c("heart attack","heart failure", "pneumonia"),org_col=c(11,17,23))
      colnames(my_out)<-c("heart attack","heart failure","pneumonia")
      #check to see if valid 
      
      if(sum(outcome==factor(my_out[1,]))==0)
            {
                  stop ("invalid outcome")
                  
            }# end outcome check
            
      ## Return hospital name in that state with the given rank
      ## 30-day death rate
      
      # Reduce to the state value
      my_state_temp <-subset(my_outcome,my_outcome$State==state)
      
      #get the correct column
      outcome_col<-as.numeric(my_out[2,outcome])
      
      #if num > number of hosptial return NA
      
      if(num > tmp_chk)
            {
                  stop("NA")
            }
      
      #if num = "best"" then num = 1
      
      if (num == "best")
            {
                  answer = best(state, outcome)
                  stop( answer )
            }# end best
      if(num == "worst")
            {
                  #find the maximum value
                  my_out_val_min <- max(as.numeric(my_state_temp[,outcome_col]),na.rm=TRUE)     
                  
                  #find the list of hospital that meet the value
                  #sort alphabetically
                  
                  temp_st <- rbind(c(1:length(colnames(my_outcome))),colnames(my_outcome))
                  target_out <- temp_st[2,outcome_col]
                  
                  temp_answer<-list(" ",0)
                  temp_index =1
                  for( i in 1:nrow(my_state_temp))
                  {     
                        if(my_state_temp[i,outcome_col]==my_out_val_min)
                        {
                              if(temp_index==1)
                              {
                                    temp_answer<-c(my_state_temp[i,2],my_state_temp[i,target_out])
                                    temp_index =2
                              } #set first value
                              else {
                                    if(temp_index>=1)
                                    { 
                                          temp_answer<-rbind(temp_answer,c(my_state_temp[i,2],my_state_temp[i,target_out]))
                                    }
                              } #set anymore values
                        }# loop to find next value
                  }# end for to find maximum value
                  
                  result = temp_answer[1]
                  stop(result)
            }#end worst
      # Now to find the the nth one
      
      
}