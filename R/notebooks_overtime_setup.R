# Prepare notebook history from the configured Athena view.

notebooks_overtime_setup <- function(accounts, output = "regular") {
  notebook_view <- Sys.getenv("ELN_NOTEBOOK_VIEW", "eln_notebook_view")
  all_notebooks <- DBI::dbGetQuery(con, glue::glue("SELECT * FROM {notebook_view}"))

  exclude_dates_raw <- Sys.getenv("ELN_EXCLUDE_DATES", "")
  exclude_dates <- if (nzchar(exclude_dates_raw)) {
    trimws(strsplit(exclude_dates_raw, ",")[[1]])
  } else {
    character(0)
  }

  formatted <- all_notebooks %>%
    rename(
      Notebook_Name = "notebook_name",
      Created = "created",
      Course_Notebook = "course_notebook",
      Owner_Fullname = "owner_full_name",
      Email = "owner_email",
      SSO_ID = "sso_id",
      Num_Users = "num_users",
      tot_act = "total_activities",
      act_7 = "activities_last_7_days",
      act_30 = "activities_last_30_days",
      act_60 = "activities_last_60_days",
      last_act = "last_activity",
      Notebook_Unique_ID = "notebook_unique_id",
      Date = "date"
    ) %>%
    mutate(
      Email = str_to_lower(Email),
      last_act = as.Date(last_act, "%Y-%m-%d"),
      Created = as.Date(Created, "%Y-%m-%d")
    )

  if (length(exclude_dates)) {
    formatted <- formatted %>% filter(!Date %in% exclude_dates)
  }

  notebooks_over_time <- formatted %>%
    inner_join(accounts, by = "Email") %>%
    mutate(PI = Lab) %>%
    filter(!is.na(Lab), Lab != "NA")

  anti_notebooks_over_time <- formatted %>%
    anti_join(accounts, by = "Email")

  if (output == "anti") anti_notebooks_over_time else notebooks_over_time
}
