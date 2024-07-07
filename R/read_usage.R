read_usage <- function(date) {
  
  #create a filename based on params$date for user data
  user_filename <- str_c("usage-data/uw-madison-usage", params$date, ".csv")
  
  #read the usage file
  usage <- read.csv(user_filename)%>%
    mutate(Email=str_to_lower(Email))%>%  #make all emails lower case for easier deduplication
    rename(Notebooks = "Notebooks.Owned", #rename the column headers 
           tot_act="Total.Activities", 
           act_7="Activities.Last.7.days",        
           act_30="Activities.Last.30.days",
           act_60="Activities.Last.60.days", 
           last_act="Last.Activity",  
           tot_log="Total.Logins", 
           log_7="Logins.Last.7.days", 
           log_30="Logins.Last.30.days", 
           log_60="Logins.Last.60.days", 
           last_log="Last.Login", 
           MB_Used="MB.Used")
  
  return(usage)
}