# Prepare usage-over-time data from the configured Athena view.

usage_overtime_setup <- function(accounts) {
  usage_view <- Sys.getenv("ELN_USAGE_VIEW", "eln_usage_view")
  all_usage <- DBI::dbGetQuery(con, glue::glue("SELECT * FROM {usage_view}"))

  all_usage_over_time <- all_usage %>%
    rename(
      Email = "email",
      Full_Name = "full_name",
      First_Name = "first_name",
      Last_Name = "last_name",
      SSO_ID = "sso_id",
      Notebooks = "notebooks_owned",
      CE_Type = "ce_type",
      Last_Course = "last_course",
      Created = "created",
      tot_act = "total_activities",
      act_7 = "activities_last_7_days",
      act_30 = "activities_last_30_days",
      act_60 = "activities_last_60_days",
      last_act = "last_activity",
      tot_log = "total_logins",
      log_7 = "logins_last_7_days",
      log_30 = "logins_last_30_days",
      log_60 = "logins_last_60_days",
      last_log = "last_login",
      MB_Used = "mb_used",
      Date = "date"
    ) %>%
    mutate(Email = str_to_lower(Email), Date = as_date(Date))

  left_join(all_usage_over_time, accounts, by = "Email") %>%
    filter(!is.na(Lab), Lab != "NA") %>%
    select(Email, Unit, Department, LabID, Lab, tot_log, log_30, log_60,
           tot_act, act_30, act_60, Date, Onboarding)
}
