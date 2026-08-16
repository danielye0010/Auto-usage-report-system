# Utilities shared by report rendering.

abbreviate_unit_name <- function(curr_unit) {
  # Generate a filesystem-safe, organization-neutral label instead of maintaining
  # a hard-coded mapping of institution-specific unit names.
  label <- stringr::str_replace_all(curr_unit, "[^A-Za-z0-9]+", "_")
  label <- stringr::str_replace_all(label, "^_+|_+$", "")
  stringr::str_sub(label, 1, 50)
}

check_trimester <- function(date_string) {
  month_name <- stringr::str_extract(date_string, "^[A-Za-z]+")
  if (month_name %in% c("January", "May", "September")) "yes" else "no"
}
