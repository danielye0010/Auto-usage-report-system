# Build notebook-download data from the configured Athena view.

downloads_setup <- function(date, lab_notebooks) {
  download_view <- Sys.getenv("ELN_DOWNLOAD_VIEW", "eln_download_view")
  query <- glue::glue("SELECT * FROM {download_view} WHERE date = '{date}'")
  downloads <- DBI::dbGetQuery(con, query)

  formatted_downloads <- downloads %>%
    rename(
      User_Name = "user_name",
      SSO_ID = "sso_id",
      User_Email = "user_email",
      Notebook_Name = "notebook_name",
      Date = "occurred_at",
      IP = "ip_address",
      Type = "full_partial_notebook",
      SHA2 = "sha2"
    ) %>%
    mutate(User_Email = str_to_lower(User_Email))

  lab_notebooks_d <- lab_notebooks %>%
    rename(Owner_Email = "Email")

  downloads <- left_join(formatted_downloads, lab_notebooks_d, by = "Notebook_Name") %>%
    filter(!is.na(Lab), Lab != "NA") %>%
    filter(
      (Notebook_Name != "My Notebook") |
        ((Notebook_Name == "My Notebook") & (User_Email == Owner_Email))
    ) %>%
    select(
      netid, Date, Type, Notebook_Name, User_Name, User_Email,
      Owner_Fullname, Owner_Email, IP, SHA2, Num_Users, LabID, Lab,
      PI, Unit, Created, last_act, tot_act, act_7, act_30, act_60,
      Notebook_Unique_ID
    )

  downloads
}
