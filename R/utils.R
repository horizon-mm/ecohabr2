check_required_columns <- function(dt, required) {
  if (!data.table::is.data.table(dt)) {
    warning("Input must be a data.table")
    invisible(NULL)
  }
  
  missing <- setdiff(required, names(dt))
  if (length(missing) > 0) {
    warning(
      sprintf(
        "data.table is missing required columns: %s",
        paste(missing, collapse = ", ")
      )
    )
    invisible(FALSE)
  }
  invisible(TRUE)
}
