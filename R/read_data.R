read_data <- function(filename) {
  
  # Read in the file with known LA users
  data <-read_csv(str_c("usage-data/", filename, ".csv"))%>%
    mutate(Email=str_to_lower(Email))%>%            # make all email lower case
    filter(!duplicated(.))                          # remove duplicates
  
  return(data)
}