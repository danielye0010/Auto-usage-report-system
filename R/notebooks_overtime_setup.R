# notebooks_overtime_setup
# This function

notebooks_overtime_setup <- function(accounts, output = "regular") {
  
  # pull all notebook data from Athena/s3
  all_notebooks <- dbGetQuery(con, "SELECT * FROM uw_madison_notebook_view")
  
  
  # process notebooks over time to find notebooks that ARE associated with eln-users
  notebooks_over_time <- all_notebooks%>%
    rename(Notebook_Name = "notebook_name", #rename the column headers
           Created="created",
           Course_Notebook="course_notebook",
           Owner_Fullname="owner_full_name",
           Email="owner_email",
           SSO_ID="sso_id",
           Num_Users="num_users",
           tot_act="total_activities", 
           act_7="activities_last_7_days",        
           act_30="activities_last_30_days",
           act_60="activities_last_60_days", 
           last_act="last_activity",  
           Notebook_Unique_ID="notebook_unique_id",
           Date="date")%>%
    mutate(Email=str_to_lower(Email))%>%
    inner_join(accounts)%>% # merge lab_notebooks with lab names, units, and departments (simultaneously filters out non-lab notebooks) - returns rows from lab-notebooks that are in accounts dataframe
    mutate(last_act = (as.Date(last_act, "%Y-%m-%d")))%>%
    mutate(Created = (as.Date(Created, "%Y-%m-%d")))%>%
    mutate(PI = Lab)%>%
    filter(Date != "2016-11-01")%>%
    filter(!is.na(Lab),
           Lab != "NA")
  
  # process notebooks over time to find notebooks that ARE NOT associated with eln-users
  anti_notebooks_over_time <- all_notebooks%>%
    rename(Notebook_Name = "notebook_name", #rename the column headers
           Created="created",
           Course_Notebook="course_notebook",
           Owner_Fullname="owner_full_name",
           Email="owner_email",
           SSO_ID="sso_id",
           Num_Users="num_users",
           tot_act="total_activities", 
           act_7="activities_last_7_days",        
           act_30="activities_last_30_days",
           act_60="activities_last_60_days", 
           last_act="last_activity",  
           Notebook_Unique_ID="notebook_unique_id",
           Date="date")%>%
    mutate(Email=str_to_lower(Email))%>%
    anti_join(accounts)%>% # return rows from lab_notebooks that aren't in accounts dataframe    mutate(last_act = (as.Date(last_act, "%Y-%m-%d")))%>%
    mutate(Created = (as.Date(Created, "%Y-%m-%d")))%>%
    filter(Date != "2016-11-01") # This month of data is not formatted correctly
  
  Date="date" # notebooks_over_time<- notebooks_over_time%>%
    # filter(!is.na(Lab), Lab != "NA")
  #   #filter(Course_Notebook == "no")%>%
  #   filter(!is.na(Lab))%>%
  #   select(Notebook_Name, Created, Email, Owner_Fullname, Num_Users, tot_act, act_30, last_act, Unit, Department, Lab, PI, Date)
  # 
  # anti_notebook_over_time<- anti_notebook_over_time%>%
  #   filter(Course_Notebook == "no")
  
  if(output == "anti") {
    
    return(anti_notebooks_over_time)
    
  } else {
    
    return(notebooks_over_time)
    
  }
  
}