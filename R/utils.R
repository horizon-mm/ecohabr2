check_required_columns <- function(dt, required) {
  if (!data.table::is.data.table(dt)) {
    stop("Input must be a data.table")
  }
  
  missing <- setdiff(required, names(dt))
  if (length(missing) > 0) {
    stop(
      sprintf(
        "data.table is missing required columns: %s",
        paste(missing, collapse = ", ")
      )
    )
  }
  invisible(TRUE)
}