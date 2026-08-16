read_usage <- function(date) {
  org_slug <- Sys.getenv("ELN_ORG_SLUG", "organization")
  user_filename <- str_c("usage-data/", org_slug, "_usage", date, ".csv")

  read.csv(user_filename) %>%
    mutate(Email = str_to_lower(Email)) %>%
    rename(
      Notebooks = "Notebooks.Owned",
      tot_act = "Total.Activities",
      act_7 = "Activities.Last.7.days",
      act_30 = "Activities.Last.30.days",
      act_60 = "Activities.Last.60.days",
      last_act = "Last.Activity",
      tot_log = "Total.Logins",
      log_7 = "Logins.Last.7.days",
      log_30 = "Logins.Last.30.days",
      log_60 = "Logins.Last.60.days",
      last_log = "Last.Login",
      MB_Used = "MB.Used"
    )
}
