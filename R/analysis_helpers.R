# Organization-neutral analysis helpers retained from the original reporting project.

graph_onboarding_comp <- function(usage_over_time, facet = TRUE) {
  dates <- unique(as.character(usage_over_time$Date))
  onboarded <- usage_over_time %>% filter(Onboarding != "NA", Onboarding != "NULL")
  not_onboarded <- usage_over_time %>% filter(Onboarding == "NA" | Onboarding == "NULL")
  activity_data <- data.frame()

  for (date in dates) {
    curr_o <- onboarded %>% filter(Date == date)
    curr_n <- not_onboarded %>% filter(Date == date)
    active_o <- max(1, nrow(curr_o %>% filter(act_60 > 0)))
    active_n <- max(1, nrow(curr_n %>% filter(act_60 > 0)))

    summarize_freq <- function(df, denominator, onboarding) {
      bind_rows(
        df %>% filter(tot_act >= 1) %>% count(Date) %>%
          mutate(freq = "one_or_more", Onboarding = onboarding, Date = date, n = n / denominator),
        df %>% filter(act_60 >= 3) %>% count(Date) %>%
          mutate(freq = "3_in_last_60", Onboarding = onboarding, Date = date, n = n / denominator),
        df %>% filter(act_60 >= 10) %>% count(Date) %>%
          mutate(freq = "10_in_last_60", Onboarding = onboarding, Date = date, n = n / denominator)
      )
    }

    activity_data <- bind_rows(
      activity_data,
      summarize_freq(curr_o, active_o, TRUE),
      summarize_freq(curr_n, active_n, FALSE)
    )
  }

  plot <- ggplot(activity_data, aes(x = as.Date(Date), y = n, color = freq, linetype = Onboarding)) +
    ggtitle("ELN User Activity Frequency") +
    theme(plot.title = element_text(hjust = 0.5)) +
    ylab("Share of Active Users") +
    geom_line(linewidth = 0.5) +
    scale_color_discrete(name = "Frequency")

  if (facet) plot + facet_grid(. ~ freq, scales = "free_y") else plot
}

graphOT_logins <- function(usage_over_time) {
  logins_data <- bind_rows(
    usage_over_time %>% filter(tot_log >= 1) %>% count(Date) %>% mutate(freq = "one_or_more"),
    usage_over_time %>% filter(log_60 >= 3) %>% count(Date) %>% mutate(freq = "3_in_last_60"),
    usage_over_time %>% filter(log_60 >= 10) %>% count(Date) %>% mutate(freq = "10_in_last_60")
  )

  ggplot(logins_data, aes(x = Date, y = n, color = freq)) +
    ggtitle("ELN User Login Frequency") +
    theme(plot.title = element_text(hjust = 0.5)) +
    ylab("Number of Logins") +
    geom_line(linewidth = 1) +
    scale_color_discrete(name = "Frequency")
}

graphOT_notebook_activities <- function(notebooks_over_time) {
  units <- c("All", sort(unique(na.omit(as.character(notebooks_over_time$Unit)))))
  units_data <- data.frame()

  for (unit in units) {
    activities <- notebooks_over_time
    if (unit != "All") activities <- activities %>% filter(Unit == unit)
    activities <- activities %>%
      group_by(Date) %>%
      summarise(date_act30 = sum(act_30, na.rm = TRUE), .groups = "drop") %>%
      mutate(freq = unit)
    units_data <- bind_rows(units_data, activities)
  }

  units_data %>%
    mutate(Date = as_date(Date)) %>%
    ggplot(aes(x = Date, y = as.integer(date_act30), color = freq)) +
    ylab("Notebook Activities in Last 30 Days") +
    ggtitle("Notebook Activities Over Time") +
    geom_line() +
    scale_color_discrete(name = "Unit")
}

graphOT_notebooks_created <- function(notebooks_over_time) {
  units <- c("All", sort(unique(na.omit(as.character(notebooks_over_time$Unit)))))
  units_data <- data.frame()

  for (unit in units) {
    notebooks <- notebooks_over_time %>% filter(tot_act >= 1)
    if (unit != "All") notebooks <- notebooks %>% filter(Unit == unit)
    notebooks <- notebooks %>% count(Date) %>% mutate(freq = unit)
    units_data <- bind_rows(units_data, notebooks)
  }

  units_data %>%
    mutate(Date = as_date(Date)) %>%
    ggplot(aes(x = Date, y = n, color = freq)) +
    ylab("Number of Notebooks") +
    ggtitle("Number of Notebooks Created Over Time") +
    geom_line() +
    scale_color_discrete(name = "Unit")
}

check_activity <- function(dataframe, trimester, tri_year, output = "inactive") {
  dataframe <- dataframe %>%
    mutate(
      Last_Act_Date = str_split_fixed(last_act, " ", n = 3)[, 1],
      Last_Act_Date_formatted = as.Date(Last_Act_Date)
    )

  if (trimester == 1) {
    curr_start <- as.Date(str_c(tri_year, "/01/01"), format = "%Y/%m/%d")
    curr_stop <- as.Date(str_c(tri_year, "/05/01"), format = "%Y/%m/%d")
    prev_start <- as.Date(str_c(as.numeric(tri_year) - 1, "/09/01"), format = "%Y/%m/%d")
    prev_stop <- as.Date(str_c(tri_year, "/01/01"), format = "%Y/%m/%d")
  } else if (trimester == 2) {
    curr_start <- as.Date(str_c(tri_year, "/05/01"), format = "%Y/%m/%d")
    curr_stop <- as.Date(str_c(tri_year, "/09/01"), format = "%Y/%m/%d")
    prev_start <- as.Date(str_c(tri_year, "/01/01"), format = "%Y/%m/%d")
    prev_stop <- as.Date(str_c(tri_year, "/05/01"), format = "%Y/%m/%d")
  } else {
    curr_start <- as.Date(str_c(tri_year, "/09/01"), format = "%Y/%m/%d")
    curr_stop <- as.Date(str_c(as.numeric(tri_year) + 1, "/01/01"), format = "%Y/%m/%d")
    prev_start <- as.Date(str_c(tri_year, "/05/01"), format = "%Y/%m/%d")
    prev_stop <- as.Date(str_c(tri_year, "/09/01"), format = "%Y/%m/%d")
  }

  prev_active <- dataframe %>%
    filter(Last_Act_Date_formatted >= prev_start, Last_Act_Date_formatted < prev_stop) %>%
    select(Unit, PI) %>% distinct()
  curr_active <- dataframe %>%
    filter(Last_Act_Date_formatted >= curr_start, Last_Act_Date_formatted < curr_stop) %>%
    select(Unit, PI) %>% distinct()

  if (output == "inactive") anti_join(prev_active, curr_active)
  else if (output == "active") anti_join(curr_active, prev_active)
  else semi_join(curr_active, prev_active)
}

check_moreThan1User <- function(active_usage, lab_notebooks) {
  one_notebook <- active_usage %>%
    filter(Notebooks == 1) %>%
    select(Email, Notebooks, MB_Used)

  clean_lab_books <- lab_notebooks %>%
    rename(Email = Owner_Email) %>%
    select(Email, Owner_Fullname, Notebook_Name, Created, Num_Users,
           tot_act, act_7, act_30, act_60, last_act, Notebook_Unique_ID,
           Lab, Unit, PI)

  more_than_one <- left_join(one_notebook, clean_lab_books) %>%
    filter(Num_Users > 1)

  more_than_one %>%
    filter(
      mapply(function(pi, email) grepl(pi, email, ignore.case = TRUE), PI, Email) |
      mapply(function(pi, name) grepl(pi, name, ignore.case = TRUE), PI, Owner_Fullname)
    ) %>%
    pull(PI)
}
