# usage_setup 
# This function 

usage_setup <- function(usage, accounts, output = "active") {
  
  formatted_usage<-usage%>%
    rename(Email="email",                   #rename the column headers
           Full_Name="full_name",
           First_Name="first_name",
           Last_Name="last_name",
           SSO_ID="sso_id",
           Notebooks="notebooks_owned",
           CE_Type="ce_type",
           Last_Course="last_course",
           Created="created",
           tot_act="total_activities", 
           act_7="activities_last_7_days",        
           act_30="activities_last_30_days",
           act_60="activities_last_60_days", 
           last_act="last_activity",
           tot_log="total_logins",
           log_7="logins_last_7_days", 
           log_30="logins_last_30_days", 
           log_60="logins_last_60_days", 
           last_log="last_login", 
           MB_Used="mb_used")%>%
    mutate(Email=str_to_lower(Email))
  
  active_usage <- left_join(formatted_usage, accounts, by = "Email")       #add role and lab info
  
  active_usage<-active_usage%>% # CHANGED from usage to active_usage
    mutate(PI = Lab)%>%
    mutate(Creation_Year = as.character(str_split_fixed(Created, " ", n = 2)[,1]))%>%   # create new column with just the year notebook was created
    mutate(Last_Act_Year = as.character(str_split_fixed(last_act, "-", n = 2)[,1]))%>%  # create new column with just the year the last notebook activity occurred
    mutate(Last_Act_Month = as.character(str_split_fixed(last_act, "-", n = 3)[,2]))    # create new column with just the month the last notebook activity occurred
  
  # The code below is not as relevant anymore, since accounts are filtered during the users step now - but it does not harm anything
  active_usage <- active_usage%>%
    filter(!is.na(Lab),
           Lab != "NA")                    # keep only those in research Lab

if(output == "inactive") {
  
  return(inactive_users)
  
} else {
  
  return(active_usage)
  
}

}