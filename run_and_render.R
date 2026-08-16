## Organization-neutral ELN usage-report orchestration example
##
## This script demonstrates how the original workflow connected Athena/S3,
## Google Sheets metadata, modular R transformations, RMarkdown rendering,
## and Google Drive delivery. Organization-specific credentials, identifiers,
## schemas, and report templates are intentionally externalized.

library(rmarkdown)
library(tidyverse)
library(readxl)
library(knitr)
library(lubridate)
library(miceadds)
library(googlesheets4)
library(googledrive)
library(gargle)
library(stringr)
library(glue)
library(DBI)

source.all("R/", grepstring = ".R")

required_env <- function(name) {
  value <- Sys.getenv(name, "")
  if (!nzchar(value)) stop(glue("Missing required environment variable: {name}"))
  value
}

# -----------------------------------------------------------------------------
# Reporting period
# -----------------------------------------------------------------------------
report_date <- as_date(Sys.getenv("REPORT_DATE", as.character(Sys.Date())))
date_param <- as.character(report_date)
full_date_param <- format(report_date, "%B %d, %Y")
month_param <- month(report_date - months(1))
year_param <- year(report_date - months(1))
trimester_param <- case_when(
  month(report_date) %in% 1:4 ~ 1,
  month(report_date) %in% 5:8 ~ 2,
  TRUE ~ 3
)
tri_year_param <- year(report_date)

report_dir <- Sys.getenv("ELN_REPORT_DIR", "reports")
dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)

template_dir <- Sys.getenv("ELN_TEMPLATE_DIR", "templates")
key_param <- required_env("GOOGLE_SERVICE_ACCOUNT_KEY")
users_gsheet_param <- required_env("ELN_USERS_SHEET")
labs_gsheet_param <- required_env("ELN_LABS_SHEET")

# -----------------------------------------------------------------------------
# Athena connection
# -----------------------------------------------------------------------------
con <- dbConnect(
  odbc::odbc(),
  Driver = Sys.getenv("ATHENA_ODBC_DRIVER", "athena"),
  UID = required_env("ATHENA_UID"),
  PWD = required_env("ATHENA_PASSWORD"),
  AuthenticationType = Sys.getenv("ATHENA_AUTH_TYPE", "IAM Credentials"),
  AwsRegion = Sys.getenv("AWS_REGION", "us-east-2"),
  S3OutputLocation = required_env("ATHENA_S3_OUTPUT"),
  Schema = Sys.getenv("ELN_ATHENA_SCHEMA", "eln_usage_data"),
  timeout = 10
)

usage_view <- Sys.getenv("ELN_USAGE_VIEW", "eln_usage_view")
curr_usage <- dbGetQuery(
  con,
  glue("SELECT * FROM {usage_view} WHERE date = '{date_param}'")
)

# -----------------------------------------------------------------------------
# Metadata
# -----------------------------------------------------------------------------
gs4_auth(path = key_param)
users <- read_sheet(users_gsheet_param, sheet = Sys.getenv("ELN_USERS_TAB", "users"))
labs <- read_sheet(labs_gsheet_param, sheet = Sys.getenv("ELN_LABS_TAB", "labs"))

clean_users <- users %>%
  filter(!is.na(LabID), LabID != "NA")
curr_accounts <- inner_join(clean_users, labs, by = "LabID")

# -----------------------------------------------------------------------------
# Core transformations
# -----------------------------------------------------------------------------
curr_active_usage <- usage_setup(curr_usage, curr_accounts)
curr_usage_over_time <- usage_overtime_setup(curr_accounts)
curr_lab_notebooks <- notebook_setup(date_param, curr_accounts)
curr_notebooks_over_time <- notebooks_overtime_setup(curr_accounts)
curr_downloads <- downloads_setup(
  date_param,
  curr_lab_notebooks %>% rename(netid = NetID)
)

dbDisconnect(con)

base_params <- list(
  full_date = full_date_param,
  date = date_param,
  month = month_param,
  year = year_param,
  accounts = curr_accounts,
  active_usage = curr_active_usage,
  usage_over_time = curr_usage_over_time,
  lab_notebooks = curr_lab_notebooks,
  notebooks_over_time = curr_notebooks_over_time,
  downloads = curr_downloads,
  automated = TRUE
)

render_if_available <- function(template_name, output_file, params) {
  template <- file.path(template_dir, template_name)
  if (!file.exists(template)) {
    warning(glue("Skipping {template_name}: public demo does not include this report template."))
    return(invisible(NULL))
  }

  render(
    input = template,
    output_format = "pdf_document",
    output_file = output_file,
    output_dir = report_dir,
    params = params,
    knit_root_dir = "."
  )
}

# -----------------------------------------------------------------------------
# Overall monthly report
# -----------------------------------------------------------------------------
render_if_available(
  "monthly_report.Rmd",
  str_c(date_param, "_eln_monthly_report"),
  base_params
)

# -----------------------------------------------------------------------------
# Unit-level monthly reports; unit names are learned from metadata rather than
# hard-coded to a specific institution.
# -----------------------------------------------------------------------------
units <- sort(unique(na.omit(as.character(curr_accounts$Unit))))
for (curr_unit in units) {
  unit_params <- base_params
  unit_params$unit <- curr_unit
  unit_params$accounts <- curr_accounts %>% filter(Unit == curr_unit)
  unit_params$active_usage <- curr_active_usage %>% filter(Unit == curr_unit)
  unit_params$usage_over_time <- curr_usage_over_time %>% filter(Unit == curr_unit)
  unit_params$lab_notebooks <- curr_lab_notebooks %>% filter(Unit == curr_unit)
  unit_params$notebooks_over_time <- curr_notebooks_over_time %>% filter(Unit == curr_unit)

  render_if_available(
    "unit_report.Rmd",
    str_c(date_param, "_", abbreviate_unit_name(curr_unit), "_monthly_report"),
    unit_params
  )
}

# -----------------------------------------------------------------------------
# Trimester report
# -----------------------------------------------------------------------------
if (check_trimester(full_date_param) == "yes") {
  trimester_params <- base_params
  trimester_params$trimester <- trimester_param
  trimester_params$tri_year <- tri_year_param

  render_if_available(
    "trimester_report.Rmd",
    str_c(date_param, "_eln_trimester_", trimester_param, "_", tri_year_param, "_report"),
    trimester_params
  )
}

# -----------------------------------------------------------------------------
# Optional Google Drive delivery
# -----------------------------------------------------------------------------
drive_parent_id <- Sys.getenv("ELN_DRIVE_REPORTS_FOLDER", "")
if (nzchar(drive_parent_id)) {
  drive_auth(path = key_param)
  folder_name <- str_c(date_param, "_eln_reports")
  parent <- as_id(drive_parent_id)
  created_folder <- drive_mkdir(name = folder_name, path = parent)

  report_files <- list.files(report_dir, pattern = "\\.pdf$", full.names = TRUE)
  for (file in report_files) {
    drive_upload(file, path = created_folder, type = "pdf")
  }
} else {
  message("ELN_DRIVE_REPORTS_FOLDER is not set; skipping Google Drive upload.")
}
