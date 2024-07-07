# notebook_setup 
# This function 

notebook_setup <- function(date,accounts) {

# pull notebook data from Athena/s3
notebooks <- dbGetQuery(con, str_c("SELECT * FROM uw_madison_notebook_view WHERE date = '", date, "'"))

notebooks <- notebooks%>%
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
         Notebook_Unique_ID="notebook_unique_id")%>%
  mutate(Email=str_to_lower(Email))     #make all emails lower case for easier merging

# merge lab_notebooks with lab names and schools - simultaneously filters out non-lab notebooks
lab_notebooks <- left_join(notebooks, accounts, by = "Email")

# filter out course notebooks  
lab_notebooks<-lab_notebooks%>%
  # filter(Course_Notebook == "no")%>%
  filter(!is.na(Lab),
         Lab != "NA")

lab_notebooks <- lab_notebooks%>%
  mutate(PI = Lab)%>% # create new column with just PI of notebook (taken from Lab field)
  mutate(Creation_Year = as.character(str_split_fixed(Created, " ", n = 2)[,1]))%>%     # create new column with just the year notebook was created
  mutate(Last_Act_Year = as.character(str_split_fixed(last_act, "-", n = 2)[,1]))%>%    # create new column with just the year the last notebook activity occurred
  mutate(Last_Act_Month = as.character(str_split_fixed(last_act, "-", n = 3)[,2]))      # create new column with just the month the last notebook activity occurred

return(lab_notebooks)

}