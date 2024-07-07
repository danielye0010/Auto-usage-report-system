downloads_setup <- function(date, lab_notebooks) {
  
  # create a filename based on params$date for downloads data
  downloads <- dbGetQuery(con, str_c("SELECT * FROM uw_madison_nb_download_view WHERE date = '", date, "'"))
  
  # read and setup the downloads file data -> filter for just month of interest
  formatted_downloads <- downloads %>%
    rename(User_Name = "user_name",              # rename column headers
           SSO_ID = "sso_id",
           User_Email = "user_email",
           Notebook_Name = "notebook_name",
           Date = "occurred_at",
           IP = "ip_address",
           Type = "full_partial_notebook",
           SHA2 = "sha2") %>%
    mutate(User_Email = str_to_lower(User_Email))  # make all emails lower case
  
  # rename Email to Owner_Email
  lab_notebooks_d <- lab_notebooks %>%
    rename(Owner_Email = "Email")
  
  # merge downloads with lab notebooks -> simultaneously filters out course notebooks
  downloads <- left_join(formatted_downloads, lab_notebooks_d, by = "Notebook_Name") # can't merge on notebook unique ID, since notebook data does not include this
  
  # Filter out course notebooks...
  downloads <- downloads %>%
    filter(!is.na(Lab),
           Lab != "NA")  # keep only those in research Lab
  
  # filter unspecific "My Notebook" entries, only keep "My Notebook" downloads that have matching owner and user emails (works for now, but there could be exceptions in the future)
  downloads <- downloads %>%
    filter((Notebook_Name != "My Notebook") | ((Notebook_Name == "My Notebook") & (User_Email == Owner_Email)))
  
  # select only relevant columns to work with
  downloads <- downloads %>%
    select(netid, Date, Type, Notebook_Name, User_Name, User_Email, Owner_Fullname, Owner_Email, IP, SHA2, Num_Users, LabID, Lab, PI, Unit, Created, last_act, tot_act, act_7, act_30, act_60, Notebook_Unique_ID)
  
  return(downloads)
}

graph_onboarding_comp <- function(usage_over_time, facet = TRUE) {
  
  # generate vector of dates
  dates <- c(unique(as.character(usage_over_time$Date)))
  
  # subset the usage over time dataset by labs that have been onboarded vs. labs that have not
  usage_over_time_onboardings <- usage_over_time %>% filter(Onboarding != "NA", Onboarding != "NULL")
  usage_over_time_NO_onboardings <- usage_over_time %>% filter(Onboarding == "NA" | Onboarding == "NULL")
  
  # initalize a data frame to store 
  activity_data = data.frame()
  
  for (date in dates) {
    print(date)
    
    curr_usage_O <- usage_over_time_onboardings %>% filter(Date == date)
    curr_usage_NO <- usage_over_time_NO_onboardings %>% filter(Date == date)
    
    print(nrow(curr_usage_O))
    print(nrow(curr_usage_NO))
    
    # get number of active users in onboardings and non onboardings group
    active_O_users <- nrow(curr_usage_O %>% filter(act_60 > 0))
    active_NO_users <- nrow(curr_usage_NO %>% filter(act_60 > 0))
    
    # count the number of users over time that have 1 login or more in the last 60 days of the download date
    one_act_O <- curr_usage_O %>%
      filter(tot_act >= 1) %>%
      count(Date) %>%
      mutate(freq = "one_or_more", Onboarding = TRUE, Date = date, n = n / active_O_users)
    
    # count the number of users over time that have 3 logins or more in the last 60 days of the download date
    act_3_60_O <- curr_usage_O %>%
      filter(act_60 >= 3) %>%
      count(Date) %>%
      mutate(freq = "3_in_last_60", Onboarding = TRUE, Date = date, n = n / active_O_users)
    
    # count the number of users over time that have 10 logins or more in the last 60 days of the download date
    act_10_60_O <- curr_usage_O %>%
      filter(act_60 >= 10) %>%
      count(Date) %>%
      mutate(freq = "10_in_last_60", Onboarding = TRUE, Date = date, n = n / active_O_users)
    
    # count the number of users over time that have 1 login or more in the last 60 days of the download date
    one_act_NO <- curr_usage_NO %>%
      filter(tot_act >= 1) %>%
      count(Date) %>%
      mutate(freq = "one_or_more", Onboarding = FALSE, Date = date, n = n / active_NO_users)
    
    # count the number of users over time that have 3 logins or more in the last 60 days of the download date
    act_3_60_NO <- curr_usage_NO %>%
      filter(act_60 >= 3) %>%
      count(Date) %>%
      mutate(freq = "3_in_last_60", Onboarding = FALSE, Date = date, n = n / active_NO_users)
    
    # count the number of users over time that have 10 logins or more in the last 60 days of the download date
    act_10_60_NO <- curr_usage_NO %>%
      filter(act_60 >= 10) %>%
      count(Date) %>%
      mutate(freq = "10_in_last_60", Onboarding = FALSE, Date = date, n = n / active_NO_users)
    
    # bind the activity data frames together
    curr_activity_data <- rbind(one_act_O, act_10_60_O, act_3_60_O, one_act_NO, act_10_60_NO, act_3_60_NO) %>%
      ungroup()
    activity_data <- rbind(activity_data, curr_activity_data)
  }
  
  # plot frequency of activities overtime, with date on x asis and n (frequency) on y-axis
  plot <- ggplot(activity_data, aes(x = as.Date(Date), y = n, color = freq, linetype = Onboarding)) +
    ggtitle("ELN User Activity Frequency for Labs") +
    theme(plot.title = element_text(hjust = 0.5)) +
    ylab("Number of Activities") +
    geom_line(size = 0.5) +
    scale_color_discrete(name = "Frequency", breaks = c("10_in_last_60", "3_in_last_60", "one_or_more"), labels = c("10 or more activities in the last 60 days", "3 or more activities in the last 60 days", "1 or more activities since 9/14"))
  
  # Split in horizontal direction
  plot + facet_grid(. ~ freq, scales = 'free_y')
}

graphOT_logins <- function(usage_over_time) {
  
  # count the number of users over time that have 1 login or more in the last 60 days of the download date
  one_login <- usage_over_time %>% filter(tot_log >= 1) %>% count(Date) %>% mutate(freq = "one_or_more")
  
  # count the number of users over time that have 3 logins or more in the last 60 days of the download date
  login_3_60 <- usage_over_time %>% filter(log_60 >= 3) %>% count(Date) %>% mutate(freq = "3_in_last_60")
  
  # count the number of users over time that have 10 logins or more in the last 60 days of the download date
  login_10_60 <- usage_over_time %>% filter(log_60 >= 10) %>% count(Date) %>% mutate(freq = "10_in_last_60")
  
  # bind the login data frames together
  logins_data <- rbind(one_login, login_10_60, login_3_60) %>% ungroup()
  
  # plot frequency of logins overtime, with date on x asis and n (frequency) on y-axis
  ggplot(logins_data, aes(x = Date, y = n, color = freq)) +
    ggtitle("ELN User Login Frequency") +
    theme(plot.title = element_text(hjust = 0.5)) +
    ylab("Number of Logins") +
    geom_line(size = 1) +
    scale_color_discrete(name = "Frequency", breaks = c("10_in_last_60", "3_in_last_60", "one_or_more"), labels = c("10 or more logins in the last 60 days", "3 or more logins in the last 60 days", "1 or more logins since 9/14"))
}

graphOT_notebook_activities <- function(notebooks_over_time) {
  
  units_data <- data.frame() # intialize a data frame to store activity data for each unit
  units <- c("All", "School of Medicine & Public Health", "College of Agriculture & Life Sciences", "College of Letters & Science", "College of Engineering",
             "School of Veterinary Medicine", "Graduate School", "School of Pharmacy", "School of Education", "LaFollette School of Public Affairs",
             "School of Nursing", "School of Human Ecology", "School of Social Work", "WSLH", "WISCIENCE") # create a vector that contains all units
  
  for (unit in units) {
    
    if (unit == "All") {
      activities <- notebooks_over_time %>%
        group_by(Date) %>%
        summarise(date_act30 = sum(act_30), .groups = "drop_last") %>%
        mutate(freq = unit)
      
    } else {
      activities <- notebooks_over_time %>%
        filter(Unit == unit) %>%
        group_by(Date) %>%
        summarise(date_act30 = sum(act_30), .groups = "drop_last") %>%
        mutate(freq = unit)
    }
    
    data <- data.frame(activities)
    units_data <- rbind(units_data, data)
  }
  
  # convert date
  units_data <- units_data %>%
    mutate(Date = as_date(Date))
  
  ggplot(units_data, aes(x = Date, y = as.integer(date_act30), color = freq)) +
    ylab("Notebook Activities in Last 30 Days") +
    ggtitle("Notebook Activities Over Time") +
    geom_line() +
    scale_color_discrete(name = "Unit")
}

graphOT_notebooks_created <- function(notebooks_over_time) {
  
  units_data <- data.frame() # intialize a data frame to store activity data for each unit
  units <- c("All", "School of Medicine & Public Health", "College of Agriculture & Life Sciences", "College of Letters & Science", "College of Engineering",
             "School of Veterinary Medicine", "Graduate School", "School of Pharmacy", "School of Education", "LaFollette School of Public Affairs",
             "School of Nursing", "School of Human Ecology", "School of Social Work", "WSLH", "WISCIENCE") # create a vector that contains all units
  
  for (unit in units) {
    
    if (unit == "All") {
      notebooks <- notebooks_over_time %>%
        filter(tot_act >= 1) %>%
        count(Date) %>%
        mutate(freq = unit)
      
    } else {
      notebooks <- notebooks_over_time %>%
        filter(tot_act >= 1) %>%
        filter(Unit == unit) %>%
        count(Date) %>%
        mutate(freq = unit)
    }
    
    data <- data.frame(notebooks)
    units_data <- rbind(units_data, data)
  }
  
  # convert date
  units_data <- units_data %>%
    mutate(Date = as_date(Date))
  
  ggplot(units_data, aes(x = Date, y = n, color = freq)) +
    ylab("Number of Notebooks") +
    ggtitle("Number of Notebooks Created Over Time") +
    geom_line() +
    scale_color_discrete(name = "Unit")
}

abbreviate_unit_name <- function(curr_unit) {
  
  abbrev_unit <- case_when(
    curr_unit == "School of Medicine & Public Health" ~ "SMPH",
    curr_unit == "College of Agriculture & Life Sciences" ~ "CALS",
    curr_unit == "College of Letters & Science" ~ "LS",
    curr_unit == "College of Engineering" ~ "Engr",
    curr_unit == "School of Veterinary Medicine" ~ "SVM",
    curr_unit == "Graduate School" ~ "Grad", 
    curr_unit == "School of Pharmacy" ~ "Pharmacy", 
    curr_unit == "School of Education" ~ "Ed", 
    curr_unit == "LaFollette School of Public Affairs" ~ "LaFollette",
    curr_unit == "School of Nursing" ~ "Nursing", 
    curr_unit == "School of Human Ecology" ~ "SoHE", 
    curr_unit == "School of Social Work" ~ "SW", 
    curr_unit == "Wisconsin State Laboratory of Hygiene" ~ "WSLH")
  
  return(abbrev_unit)
}

check_activity <- function(dataframe, trimester, tri_year, output = "inactive") {
  
  # create new formatted "last_activity_date in lab_notebook and usage
  dataframe <- dataframe %>%
    mutate(Last_Act_Date = str_split_fixed(last_act, " ", n = 3)[,1])
  
  dataframe  <- dataframe %>%
    mutate(Last_Act_Date_formatted = as.Date(Last_Act_Date))
  
  # initialize cutoff variables
  curr_start_cutoff = ""
  curr_stop_cutoff = ""
  
  prev_start_cutoff = ""
  prev_stop_cutoff = ""
  
  # current = Trimester 1, previous = Trimester 3
  if (trimester == 1) {
    curr_start_cutoff = as.Date(str_c(tri_year, "/01/01"), format = "%Y/%m/%d")
    curr_stop_cutoff = as.Date(str_c(tri_year, "/05/01"), format = "%Y/%m/%d")
    
    prev_start_cutoff = as.Date(str_c(as.numeric(tri_year) - 1, "/09/01"), format = "%Y/%m/%d")
    prev_stop_cutoff = as.Date(str_c(tri_year, "/01/01"), format = "%Y/%m/%d")
    
  # current = Trimester 2, previous = Trimester 1
  } else if (trimester == 2) {
    curr_start_cutoff = as.Date(str_c(tri_year, "/05/01"), format = "%Y/%m/%d")
    curr_stop_cutoff = as.Date(str_c(tri_year, "/09/01"), format = "%Y/%m/%d")
    
    prev_start_cutoff = as.Date(str_c(tri_year, "/01/01"), format = "%Y/%m/%d")
    prev_stop_cutoff = as.Date(str_c(tri_year, "/05/01"), format = "%Y/%m/%d")
    
  # current = Trimester 3, previous = Trimester 2
  } else if (trimester == 3) {
    curr_start_cutoff = as.Date(str_c(tri_year, "/09/01"), format = "%Y/%m/%d")
    curr_stop_cutoff = as.Date(str_c(as.numeric(tri_year) + 1, "/01/01"), format = "%Y/%m/%d")
    
    prev_start_cutoff = as.Date(str_c(tri_year, "/05/01"), format = "%Y/%m/%d")
    prev_stop_cutoff = as.Date(str_c(tri_year, "/09/01"), format = "%Y/%m/%d")
  }
  
  # determine all labs with active notebooks in the previous trimester
  prev_active <- unique(dataframe %>%
                          filter(Last_Act_Date_formatted >= prev_start_cutoff) %>%
                          filter(Last_Act_Date_formatted < prev_stop_cutoff) %>%
                          select("Unit", "PI") %>%
                          arrange(Unit))
  
  # determine all labs with active notebooks in the current trimester
  curr_active <- unique(dataframe %>%
                          filter(Last_Act_Date_formatted >= curr_start_cutoff) %>%
                          filter(Last_Act_Date_formatted < curr_stop_cutoff) %>%
                          select("Unit", "PI") %>%
                          arrange(Unit))
  
  still_active <- semi_join(curr_active, prev_active)   # return all rows from currently active that are in previously active
  active <- anti_join(curr_active, prev_active)         # return all rows from currently active that are not in previously active
  inactive <- anti_join(prev_active, curr_active)       # return all rows from previously active that are not in currently active
  
  if (output == "inactive") {
    return(inactive)
  } else if (output == "active") {
    return(active)
  } else {
    return(still_active)
  }
}

check_moreThan1User <- function(active_usage, lab_notebooks) {
  ## PIs with 1 notebook and more than 1 user
  
  # find all users with only one notebook
  one_notebook <- active_usage %>%
    filter(Notebooks == 1) %>%
    select(Email, Notebooks, MB_Used)
  
  # standardize the names and choose only relevant columns from lab notebooks data frame
  clean_lab_books <- lab_notebooks %>%
    rename(Email = Owner_Email) %>%
    select(Email, Owner_Fullname, Notebook_Name, Created, Num_Users, tot_act, act_7, act_30, act_60, last_act, Notebook_Unique_ID, Lab, Unit, PI)
  
  one_notebook_combo <- left_join(one_notebook, clean_lab_books) # return all rows from one_notebook dataframe with columns from both one_notebook and clean_lab_books
  
  # exclude notebooks with one user or less
  moreThan1User <- one_notebook_combo %>%
    filter(Num_Users > 1)
  
  PI_moreThan1User1Book <- data.frame() # initialize a data frame
  
  # loop through data frame of notebooks with more than one user
  for (i in 1:nrow(moreThan1User)) {
    row <- moreThan1User[i, ] # obtain row
    
    # if the PI name is contained within the owner email bind the row to the data frame
    if (grepl(row$PI, row$Email, ignore.case = TRUE)) {
      PI_moreThan1User1Book <- rbind(PI_moreThan1User1Book, row)
      
    } else {
      # check to see if the PI name matches the owner's name if it does not match the email -> if it does match full name, bind the row to the data frame
      if (grepl(row$PI, row$Owner_Fullname, ignore.case = TRUE)) {
        PI_moreThan1User1Book <- rbind(PI_moreThan1User1Book, row)
      }
    }
  }
  
  return(PI_moreThan1User1Book$PI)
}

check_trimester <- function(date_string, trimester) {
  
  # return that current date is not trimester cutoff as default
  trimester_cutoff = "no"
  
  # Trimester 1 Cutoff
  if (grepl("May", date_string, fixed = TRUE)) {
    trimester_cutoff = "yes"
  }
  
  # Trimester 2 Cutoff
  if (grepl("September", date_string, fixed = TRUE)) {
    trimester_cutoff = "yes"
  }
  
  # Trimester 3 Cutoff
  if (grepl("January", date_string, fixed = TRUE)) {
    trimester_cutoff = "yes"
  }
  
  return(trimester_cutoff)
}

# Combined Report Functions

usage_setup <- function(date) {
  
  # create a filename based on params$date for user data
  user_filename <- str_c("usage-data/uw-madison-usage", params$date, ".xlsx")
  
  # read the usage file
  usage <- read_xlsx(user_filename) %>%
    mutate(Email = str_to_lower(Email)) %>%  # make all emails lower case for easier deduplication
    rename(Notebooks = "Notebooks Owned",  # rename the column headers 
           tot_act = "Total Activities", 
           act_7 = "Activities Last 7 days",        
           act_30 = "Activities Last 30 days",
           act_60 = "Activities Last 60 days", 
           last_act = "Last Activity",  
           tot_log = "Total Logins", 
           log_7 = "Logins Last 7 days", 
           log_30 = "Logins Last 30 days", 
           log_60 = "Logins Last 60 days", 
           last_log = "Last Login", 
           MB_Used = "MB Used")
  
  # Read in the file with known LA users
  accounts <- read_csv("usage-data/all-vlookup.csv") %>%
    mutate(Email = str_to_lower(Email)) %>%  # make all email lower case
    select(Email, Role) %>%  # keep only the Email and role columns
    filter(!duplicated(.))  # remove duplicates
  
  # Read in the file with what labs users are in
  labs <- read_csv("usage-data/lab-vlookup.csv") %>%
    mutate(Email = str_to_lower(Email)) %>%  # make all email lower case
    select(Email, Lab) %>%  # Keep only email and lab
    filter(!duplicated(.))  # remove duplicates
  
  # combine the information about labs and users roles to the usage data
  usage <- usage %>%
    full_join(accounts) %>%  # add role info
    full_join(labs) %>%  # add lab info
    filter(Role == "researcher")  # keep only those in the researcher role
  
  usage <- usage %>%
    mutate(Unit = as.factor(str_split_fixed(Lab, "-", n = 2)[,1])) %>%  # split lab and college info
    mutate(Unit = fct_recode(Unit,  # correct unit spellings
                             LS = "Chem",  
                             SLH = "LSH",
                             SLH = "SHL", 
                             Pharmacy = "Pharm",
                             Pharmacy = "PHarmacy"))
  
  # create data frame of users with accounts that have not been created/initialized, i.e have NA in "Created" column
  inactiveUsers <- usage[is.na(usage$Created), ]
  
  # filter users that have accounts that have not been created/initialized out of usage data frame
  usage <- usage[!is.na(usage$Created), ]
  
  active_usage <- usage %>%
    filter(!is.na(Notebooks))
  
  return(active_usage)
}

usage_overtime_setup <- function() {
  
  directory = "usage-data"
  usage_over_time <- tibble()  # initializes the data frame
  usage_files <- list.files(directory, pattern = ".xlsx")
  
  file_date <- mdy(params$date)
  
  for (file in usage_files) {
    
    date <- as.character(gsub(".*?([0-9]+).*$", "\\1", file), sep = "")
    month <- str_sub(date, 1L, 2L)
    day <- str_sub(date, 3L, 4L)
    yr <- paste("20", str_sub(date, 5L, 6L), sep = "")
    date <- paste(yr, month, day, sep = "-")
    
    if (date <= file_date) {
      data_sheet <- read_xlsx(file.path(directory, file, fsep = "/")) %>%
        mutate(Email = str_to_lower(Email)) %>%
        full_join(accounts) %>%
        filter(Role == "researcher") %>%
        select(Email, `Total Logins`, `Logins Last 60 days`) %>%
        mutate(Date = ymd(date))
      
      usage_over_time <- bind_rows(usage_over_time, data_sheet)
    }
  }
  
  return(as.vector(accounts, active_usage))
}

notebook_setup <- function(date) {
  
  # create a filename based on params$date for notebook data
  notebook_filename <- str_c("../notebook-data/uw-madison-notebook-usage", params$date, ".xlsx")
  
  # read the usage file
  notebooks <- read_xlsx(notebook_filename) %>%
    rename(Notebook_Name = "Notebook Name",  # rename the column headers 
           Course_Notebook = "Course Notebook?",
           Owner_Fullname = "Owner Fullname",
           Owner_Email = "Owner Email",
           SSO_ID = "SSO ID",
           Num_Users = "# of Users",
           tot_act = "Total Activities", 
           act_7 = "Activities Last 7 days",        
           act_30 = "Activities Last 30 days",
           act_60 = "Activities Last 60 days", 
           last_act = "Last Activity",  
           Notebook_Unique_ID = "Notebook Unique ID") %>%
    mutate(Owner_Email = str_to_lower(Owner_Email))  # make all emails lower case for easier merging
  
  # filter out course notebooks  
  lab_notebooks <- notebooks %>%
    filter(Course_Notebook == "no")
  
  course_notebooks <- notebooks %>%
    filter(Course_Notebook == "yes")
  
  # Read in the file with known LA users
  accounts <- read_csv("usage-data/all-vlookup.csv") %>%
    mutate(Email = str_to_lower(Email)) %>%  # make all email lower case
    select(Email, Role) %>%  # keep only the Email and role columns
    filter(!duplicated(.))  # remove duplicates
  
  # Read in the file with what labs users are in
  labs <- read_csv("usage-data/lab-vlookup.csv") %>%
    mutate(Email = str_to_lower(Email)) %>%  # make all email lower case
    select(Email, Lab) %>%  # Keep only email and lab
    filter(!duplicated(.))
  
  # merge lab_notebooks with lab names and schools - simultaneously filters out non-lab notebooks
  lab_notebooks2 <- merge(lab_notebooks, labs, by.x = "Owner_Email", by.y = "Email")
  
  # split lab and units, and relabel
  lab_notebooks3 <- lab_notebooks2 %>%
    mutate(Unit = as.factor(str_split_fixed(Lab, "-", n = 2)[,1])) %>%  # split lab and unit info
    mutate(Unit = fct_recode(Unit,  # correct unit spellings
                             LS = "Chem",  
                             SLH = "LSH",
                             SLH = "SHL", 
                             Pharmacy = "Pharm",
                             Pharmacy = "PHarmacy"))
  
  lab_notebooks4 <- lab_notebooks3 %>%
    mutate(PI = as.factor(str_split(lab_notebooks3$Lab, "-", 2, simplify = TRUE)[,2]))
  
  return(lab_notebooks4)
}

notebooks_overtime_setup <- function() {
  
  directory = "../notebook-data"
  notebook_over_time <- tibble()  # initializes the data frame
  anti_notebook_over_time <- tibble()
  notebook_files <- list.files(directory, pattern = ".xlsx")
  
  file_date <- mdy(params$date)
  
  for (file in notebook_files) {
    
    date <- as.character(gsub(".*?([0-9]+).*$", "\\1", file), sep = "")
    month <- str_sub(date, 1L, 2L)
    day <- str_sub(date, 3L, 4L)
    yr <- paste("20", str_sub(date, 5L, 6L), sep = "")
    date <- paste(yr, month, day, sep = "-")
    
    if (date <= file_date & (date != "2016-11-01")) {  # removed November 2016 download data because it is not accurate
      
      notebook_data_sheet <- read_xlsx(file.path(directory, file, fsep = "/")) %>%
        filter(`Course Notebook?` == "no") %>%
        mutate(`Owner Email` = str_to_lower(`Owner Email`)) %>%
        rename(Email = "Owner Email") %>%
        inner_join(labs) %>%
        mutate(`Last Activity` = as.Date(`Last Activity`, "%Y-%m-%d")) %>%
        mutate(`Created` = as.Date(`Created`, "%Y-%m-%d")) %>%
        mutate(Unit = as.factor(str_split_fixed(Lab, "-", n = 2)[,1])) %>%
        mutate(Unit = fct_recode(Unit,  # correct unit spellings
                                 LS = "Chem",  
                                 SLH = "LSH",
                                 SLH = "SHL", 
                                 Pharmacy = "Pharm",
                                 Pharmacy = "PHarmacy")) %>%
        mutate(PI = as.factor(str_split(Lab, "-", 2, simplify = TRUE)[,2])) %>%
        select(`Notebook Name`, `Created`, `Email`, `Owner Fullname`, `# of Users`, `Total Activities`, `Activities Last 30 days`, `Last Activity`, `Unit`, `PI`) %>%
        mutate(Date = ymd(date)) %>%
        rename(Notebook_Name = "Notebook Name",  # rename the column headers
               Owner_Fullname = "Owner Fullname",
               num_users = "# of Users",
               tot_act = "Total Activities",
               act_30 = "Activities Last 30 days",
               last_act = `Last Activity`)
      
      anti_notebook_data_sheet <- read_xlsx(file.path(directory, file, fsep = "/")) %>%
        filter(`Course Notebook?` == "no") %>%
        mutate(`Owner Email` = str_to_lower(`Owner Email`)) %>%
        rename(Email = "Owner Email") %>%
        anti_join(labs) %>%
        mutate(`Last Activity` = as.Date(`Last Activity`, "%Y-%m-%d")) %>%
        mutate(`Created` = as.Date(`Created`, "%Y-%m-%d")) %>%
        select(`Notebook Name`, `Created`, `Email`, `Owner Fullname`, `# of Users`, `Total Activities`, `Activities Last 30 days`, `Last Activity`) %>%
        mutate(Date = ymd(date)) %>%
        rename(Notebook_Name = "Notebook Name",  # rename the column headers
               Owner_Fullname = "Owner Fullname",
               num_users = "# of Users",
               tot_act = "Total Activities",
               act_30 = "Activities Last 30 days",
               last_act = `Last Activity`) %>%
        filter(Owner_Fullname != "Jan Cheetham" & Owner_Fullname != "Tobin Magle" & Owner_Fullname != "Shannon Stiles" & Owner_Fullname != "John Puccinelli")
      
      notebook_over_time <- bind_rows(notebook_over_time, notebook_data_sheet)
      anti_notebook_over_time <- bind_rows(anti_notebook_over_time, anti_notebook_data_sheet)
    }
  }
  
  return(notebook_over_time)
}
