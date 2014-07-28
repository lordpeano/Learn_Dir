best<-function(state,outcome)
      {
           
            # Read Data
            my_outcome<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
            
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
            
            # Reduce to the state value
            my_state_temp <-subset(my_outcome,my_outcome$State==state)
            
            #get the correct column
            outcome_col<-as.numeric(my_out[2,outcome])
            
            
            #find the minimum value
            my_out_val_min <- min(as.numeric(my_state_temp[,outcome_col]),na.rm=TRUE)     
            
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
            }# end for to find minimum value
            
            result = temp_answer[1]
            result
      }# end function