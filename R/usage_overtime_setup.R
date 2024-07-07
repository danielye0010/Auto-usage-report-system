# usage_overtime_setup
# This function

usage_overtime_setup <- function(accounts) {
  
  all_usage <- dbGetQuery(con, "SELECT * FROM uw_madison_usage_view")
  
  all_usage_over_time <- all_usage%>%
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
           MB_Used="mb_used",
           Date="date")%>%
    mutate(Email=str_to_lower(Email))%>%
    mutate(Date = as_date(Date))
  
  # The code below is not as relevant anymore, since accounts are filtered during the users step now - but it does not harm anything
  usage_over_time <- left_join(all_usage_over_time, accounts, by = "Email")%>%
    filter(!is.na(Lab),
           Lab != "NA")%>% # Used to filter on Role == Researcher - now filtering on whether they are part of a Lab
    select(Email, Unit, Department, LabID, Lab, tot_log, log_30, log_60, tot_act, act_30, act_60, Date, Onboarding)

  
  return(usage_over_time)
  
}