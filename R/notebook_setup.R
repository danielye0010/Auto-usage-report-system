# Prepare current notebook-usage data from the configured Athena view.

notebook_setup <- function(date, accounts) {
  notebook_view <- Sys.getenv("ELN_NOTEBOOK_VIEW", "eln_notebook_view")
  query <- glue::glue("SELECT * FROM {notebook_view} WHERE date = '{date}'")
  notebooks <- DBI::dbGetQuery(con, query)

  notebooks <- notebooks %>%
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
      Notebook_Unique_ID = "notebook_unique_id"
    ) %>%
    mutate(Email = str_to_lower(Email))

  lab_notebooks <- left_join(notebooks, accounts, by = "Email") %>%
    filter(!is.na(Lab), Lab != "NA") %>%
    mutate(
      PI = Lab,
      Creation_Year = as.character(str_split_fixed(Created, " ", n = 2)[, 1]),
      Last_Act_Year = as.character(str_split_fixed(last_act, "-", n = 2)[, 1]),
      Last_Act_Month = as.character(str_split_fixed(last_act, "-", n = 3)[, 2])
    )

  lab_notebooks
}
