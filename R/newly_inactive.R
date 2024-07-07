newly_inactive <- function(active_usage, lab_notebooks, trimester, tri_year) {
  # code to find the labs that became inactive since the last trimester check - check_activity function is called to divide months data into trimesters
  
  # determine labs that became inactive in the past three months
  inactive_notebooks <- check_activity(lab_notebooks, trimester, tri_year)
  
  # determine inactive labs this month that were active in the last trimester in usage data
  inactive_users <- check_activity(active_usage, trimester, tri_year)
  
  # merge list of labs with inactive notebooks this month that were active last trimester with labs with inactive users this month that were active last trimester
  all_inactive <- full_join(inactive_notebooks, inactive_users)
  
  inactive_PI_accounts <- accounts # add PI emails to table (Do we have a designated contact for each lab, or should we send out emails to PI?)
  
  all_inactive <- all_inactive%>%
    mutate(Email = NA)
  
  for (i in 1: nrow(all_inactive)) {
    curr_row <- all_inactive[i, ]
    curr_PI <- curr_row$PI
    # print(curr_PI)
    
    # filter for just users
    active_usage_PI <- active_usage%>%
      filter(Lab == curr_PI)
    
    # THIS FOR LOOP IS NOT UPDATED TO USE LABID versus the EMAIL COMPARISON CURRENTLY - In the future using LabID would be ideal
    for(j in 1:nrow(active_usage_PI)) {
      row <- active_usage_PI[j, ]                                    # obtain the user row
      
      # if the PI name is contained within the owner email add email to row of all_inactive table
      if(!is.na(curr_PI) & grepl(curr_PI, row$Email, ignore.case = TRUE)) {
        all_inactive[i, "Email"] = row$Email
        # print(curr_PI)
        # print(row$Email)
        
        
      } else {
        
        # check to see if the PI name matches the owner's name if it does not match the email -> if it does match the full name, add email to all_inactive table
        if(!is.na(curr_PI) & grepl(curr_PI, row$Full_Name, ignore.case = TRUE)) {
          all_inactive[i, "Email"] = row$Email
          # print(curr_PI)
          
        }
      }
    }
  }
  
  return(all_inactive)
}