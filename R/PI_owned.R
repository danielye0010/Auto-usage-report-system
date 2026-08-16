PI_owned <- function(lab_notebooks) {
  # code to find the number of notebooks which are owned by PIs
  PI_owned_notebooks = 0
  
  # loop through lab notebooks to determine if notebook is owned by the PI in the lab field
  for(i in 1:nrow(lab_notebooks)) {
    row <- lab_notebooks[i, ]                                    # obtain the row
    PI_name <- row$PI                                             # obtain the PI name
    
    # if the PI name contains a "-", split the name once more and save the last segment of the split string
    if(!is.na(PI_name) & grepl("-", PI_name, ignore.case = TRUE)) {               
      PI_name<- str_split(PI_name, "-", 2, simplify = TRUE)[,2]
      
    }
    
    # if the PI name is contained within the owner email increment the counter for the number of PI owned lab notebooks
    if(!is.na(PI_name) & grepl(PI_name, row$Email, ignore.case = TRUE)) {
      PI_owned_notebooks = PI_owned_notebooks + 1
      
    } else {
      
      # check to see if the PI name matches the owner's name if it does not match the email -> if it does match the full name, increment the counter for the number of PI owned lab notebooks
      if(!is.na(PI_name) & grepl(PI_name, row$Owner_Fullname, ignore.case = TRUE)) {
        PI_owned_notebooks = PI_owned_notebooks + 1
        
      }
    }
  }
  return(PI_owned_notebooks)
}