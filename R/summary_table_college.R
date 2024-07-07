
summary_table_college <- function(active_usage, lab_notebooks) {
  
  # summarize usage data for total lab, individuals, and data stored per unit                
  usage_table<-active_usage%>% # CHANGED from usage to active_usage
    group_by(Department)%>%
    summarise(Labs = length(unique(Lab)), 
              Individuals = n(), 
              `Data Stored (GB)` = (sum(MB_Used, na.rm = TRUE))/1000,
              .groups = "drop_last")%>%
    arrange(desc(Labs)) # unit with the most labs will be at top
  
  # summarize notebook data for notebooks, total actitivites, and activities last 30 days per unit
  notebook_table<-lab_notebooks%>% # CHANGED from lab_notebooks3 to lab_notebooks
    group_by(Department)%>%
    summarise(Notebooks = n(),
              `Total Activities` = sum(tot_act),
              `Activities Last 30 Days` = as.integer(sum(act_30)),
              .groups = "drop_last")
  
  # combine usage summary data frame with notebook summary data frame
  usage_table <- merge(usage_table, notebook_table, by.x = "Department", by.y = "Department")
  
  usage_table <- usage_table[, c("Department", "Labs", "Notebooks", "Individuals", "Activities Last 30 Days", "Data Stored (GB)")]%>%
    arrange(desc(Labs)) # table order will have highest values at top 
  
  return(usage_table)
}
