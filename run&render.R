## ---------------------------
## Statement of Data Security

## This repository contains demonstration code for our internal projects.   
## All sensitive information, including API keys, passwords, usernames, and any other confidential data, has been carefully removed or replaced with placeholders.   
## The code presented here is intended solely for illustrating the project's logic and process.
## ---------------------------

##################################################
##  libraries needed 
##################################################

# Load the necessary packages/load libraries
library(rmarkdown)
library(tidyverse) #for dplyr and ggplot
library(readxl)    # to read in input
library(knitr)     #for the kable function
library(lubridate) #to manipulate dates
library(miceadds)  #for source.all function
library(googlesheets4)  # for connecting to ELN google drive
library(googledrive)
library(gargle)         # for retrieving oauth2 token (wrapper)
library(stringr)
library(dplyr, warn.conflicts = FALSE)
library(glue)

# Define source for R functions
source.all("R/", grepstring=".R")

##################################################
##  PARAMETERS - update before running each month
##  enter download date here just once assign to single_date
##  the date needs to match the date used in the renamed tsv files. 
##################################################
single_date <- "2023-01-06"             
download_date <- as_date(single_date)

trimester_param <- case_when(month(download_date) %in% c(1,2,3,4) ~ 1,
                             month(download_date) %in% c(5,6,7,8) ~ 2,
                             month(download_date) %in% c(9,10,11,12) ~ 3)                
# update every trimester with the current trimester

tri_year_param <- year(download_date)                    
# update every year with the correct trimester year
full_date_param <- glue("{month(single_date, label = TRUE, abbr = FALSE)}, {day(single_date)}{case_when(!day(single_date) %in% c(1,2,3) ~ 'th', day(single_date) == 1 ~ 'st', day(single_date) == 2 ~ 'nd', day(single_date) == 3 ~ 'rd')} {year(single_date)}")
#string containing the download date
date_param <- single_date

month_param <- month(download_date - months(1))                
# update every month with the written month the data belongs to
# (i.e., the month before the download date)

year_param <-  year(download_date - months(1))
# update when necessary with the year of the month the data 
# belongs to (i.e., the year that corresponds to the month before the download date)

key_param <- "your-key-file.json"  # replaced with a placeholder
users_gsheet_param <- "https://docs.google.com/spreadsheets/your-users-sheet"  # replaced with a placeholder
labs_gsheet_param <- "https://docs.google.com/spreadsheets/your-labs-sheet"  # replaced with a placeholder
automated_status <- TRUE             # change to FALSE if you don't want the Rmarkdown reports to run automatically (not used in render_reports.R, but useful to change parameters within Rmd file if testing new code)
append_status <- TRUE                # change to FALSE if you don't want the high level summary data written to Jan's gsheet that pulls into the RCI dashboard

##################################################
##  PROCESS DATA
##################################################

library(DBI) # DBI defines an interface for communication between R and relational database management systems
con <- dbConnect(odbc::odbc(),
                 Driver = "athena",
                 UID = Sys.getenv("uid"),
                 PWD = Sys.getenv("password"),
                 AuthenticationType = "IAM Credentials",
                 AwsRegion = "us-east-2",
                 S3OutputLocation = "s3://your-s3-bucket/output/",  # replaced with a placeholder
                 Schema = "eln-usage-data",
                 timeout = 10)

# Read in the usage data for specific month
curr_usage <- dbGetQuery(con, str_c("SELECT * FROM uw_madison_usage_view WHERE date = '", date_param, "'"))

# Read in the manifest meta data file with known LA users from google drive
gs4_auth(path = key_param)

# read in users metadata from users sheet in ELN-Users google drive sheet
users <- read_sheet(users_gsheet_param, sheet = "users")
# remove all users with no LabID
clean_users <- users %>% filter(!is.na(LabID))
# remove all users with NA LabIDs (i.e. the consultants and IT personnel)
clean_users <- users %>% filter(LabID != "NA")

# Read in labs meta data from labs sheet in google drive
labs <- read_sheet(labs_gsheet_param, sheet = "labs")

# combine metadata info from labs into users data frame
curr_accounts <- inner_join(clean_users, labs, by="LabID")

# set up current month's usage data
curr_active_usage <- usage_setup(curr_usage, curr_accounts)

# set up usage over time
curr_usage_over_time <- usage_overtime_setup(curr_accounts)

# set up current month's notebook data
curr_lab_notebooks <- notebook_setup(date_param, curr_accounts)

# set up notebooks over time
curr_notebooks_over_time <- notebooks_overtime_setup(curr_accounts)

# set up download data - USE THIS IF THE TRIMESTER REPORT IS ADDED TO THE AUTOMATED RENDER REPORT SCRIPT
# added the pipe to change the column NetID in the curr_lab_notebooks to be "netid"
curr_downloads <- downloads_setup(date_param, curr_lab_notebooks %>% rename(netid = NetID))

##################################################
##  RENDER REPORTS
##################################################

# remove all previous report files from ~/eln-usage/reports
# list.files("~/eln-usage/reports", full.names = TRUE)
do.call(file.remove, list(list.files("./reports", full.names = TRUE)))

##### RENDER ELN MONTHLY USAGE COMBINED REPORT #####

# create output_file report name
eln_report_name <- str_c(date_param, "_ELN_monthly_report")

# set parameters for reports
curr_params <- list(full_date = full_date_param,
                    date = date_param,
                    month = month_param,
                    year = year_param,
                    key = key_param,
                    users_gsheet = users_gsheet_param,
                    labs_gsheet = labs_gsheet_param,
                    usage = curr_usage,
                    accounts = curr_accounts,
                    active_usage = curr_active_usage,
                    usage_over_time = curr_usage_over_time,
                    lab_notebooks = curr_lab_notebooks,
                    notebooks_over_time = curr_notebooks_over_time,
                    automated = automated_status,
                    append = append_status)

#getwd()

# render ELN MONTHLY REPORT
render(input = "./scripts/ELN_usage_combined_report.Rmd", 
       output_format = "pdf_document",
       output_file = eln_report_name,
       output_dir = "~/eln_usage/reports",
       params = curr_params,
       knit_root_dir = "~/eln_usage")

#getwd()

##### RENDER UNIT MONTHLY REPORTS #####

# create vector of units
units <- c("School of Medicine & Public Health", 
          "College of Agriculture & Life Sciences",
          "College of Letters & Science", 
          "College of Engineering",
          "School of Veterinary Medicine",
          "Graduate School",
          "School of Pharmacy",
          "School of Education",
          "LaFollette School of Public Affairs",
          "School of Nursing",
          "School of Human Ecology", 
          "School of Social Work")

#"Wisconsin State Laboratory of Hygiene" is giving an error ...

# loop through units
for(curr_unit in units) {
    
  # convert full unit name to abbreviated unit name
  abbrev_unit <- abbreviate_unit_name(curr_unit)
  
  # create file name
  report_name <- str_c(date_param, "_", abbrev_unit, "_monthly_report")
  
  # filter data by unit
  current_accounts <-  curr_accounts %>%
    filter(Unit == curr_unit)

  current_active_usage <- curr_active_usage %>%
    filter(Unit == curr_unit)

  current_usage_over_time <- curr_usage_over_time %>%
    filter(Unit == curr_unit)

  current_lab_notebooks <- curr_lab_notebooks %>%
    filter(Unit == curr_unit)

  current_notebooks_over_time <- curr_notebooks_over_time %>%
    filter(Unit == curr_unit)
  
  # add unit to curr_params
  # compile parameters for current unit of interest
  curr_params <- list(full_date = full_date_param,
                      date = date_param,
                      month = month_param,
                      year = year_param,
                      key = key_param,
                      users_gsheet = users_gsheet_param,
                      labs_gsheet = labs_gsheet_param,
                      unit = curr_unit,
                      usage = curr_usage,
                      accounts = current_accounts,
                      active_usage = current_active_usage,
                      usage_over_time = current_usage_over_time,
                      lab_notebooks = current_lab_notebooks,
                      notebooks_over_time = current_notebooks_over_time,
                      automated = automated_status)
  
#  print(curr_params)
  # render function for unit report
  render(input = "./scripts/ELN_usage_by_college.Rmd", 
         output_format = "pdf_document",
         output_file = report_name,
         output_dir = "./reports",
         params = curr_params,
         knit_root_dir = ".")

#taking the "Wisconsin State Laboratory of Hygiene" prevents the error from happening
  
}

##### RENDER ELN TRIMESTER REPORT #####

# check to see if download date matches the trimester cutoff (January, May, September) and the trimester report should be run
trimester_cutoff <-  check_trimester(full_date_param)
#print(trimester_cutoff)

if (trimester_cutoff == "yes") {

  # create output_file report name
  eln_report_name <- str_c(date_param, "_ELN_trimester_", trimester_param, "_", tri_year_param, "_report")
  
  # set parameters for reports
  curr_params <- list(trimester = trimester_param,
                      tri_year = tri_year_param,
                      full_date = full_date_param,
                      date = date_param,
                      month = month_param,
                      year = year_param,
                      key = key_param,
                      users_gsheet = users_gsheet_param,
                      labs_gsheet = labs_gsheet_param,
                      accounts = curr_accounts,
                      active_usage = curr_active_usage,
                      usage_over_time = curr_usage_over_time,
                      lab_notebooks = curr_lab_notebooks,
                      notebooks_over_time = curr_notebooks_over_time,
                      downloads = curr_downloads,
                      automated = automated_status,
                      append = append_status)
  
  # render ELN TRIMESTER REPORT
  render(input = "./scripts/ELN_trimester_report.Rmd", 
         output_format = "pdf_document",
         output_file = eln_report_name,
         output_dir = "./reports",
         params = curr_params,
         knit_root_dir = ".")
  
}

##################################################
##  TRANSFER FILES
##################################################

# Transfer files to ELN-team Google Drive: /ELN-team/ELN-usage/reports/(date_param)_reports

# authorize googledrive
library(googledrive)

# If you are interacting with R from a web-based platform, like RStudio Server or Cloud,
# you need to use a variant of this flow, known as out-of-band auth ("oob").
# If this does not happen automatically, you can request it yourself with use_oob = TRUE or,
# more persistently, by setting an option via options(gargle_oob_default = TRUE).
print(key_param)
drive_auth(path = key_param, use_oob = TRUE)

# create name of reports folder
folder_name <- str_c(date_param, "_eln_reports")
reports_path <- as_id("https://drive.google.com/drive/folders/your-reports-folder-id")  # replaced with a placeholder
# create new reports folder for current date
drive_mkdir(name = folder_name,
            path = reports_path)

# loop through reports folder and upload current reports to new folder in ELN-team drive
drive_path <- as_id("https://drive.google.com/drive/folders/your-team-drive-id")  # replaced with a placeholder
folder_path <- drive_find(pattern = folder_name,
                          type = "folder",
                          shared_drive = drive_path)  # updated to use `shared_drive`

directory <- "~/eln_usage/reports"
report_files <- list.files(directory, pattern = ".pdf")

for (file in report_files) {

  # create curr_path
  curr_path <- str_c(directory, "/", file)

  # upload file to the current reports folder in the ELN-team google drive
  drive_upload(curr_path, path = folder_path, type = "pdf")
}
