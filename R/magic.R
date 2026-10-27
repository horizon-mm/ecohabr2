#' Migrate data
#' @description
#' Migrate data from an old version of EcoHAB instance to current version
#' @param ehd_old An instance of EcoHAB containing previous data
#' @return An instance of current EcoHAB version with previous data
#' @export
migrate_data <- function(ehd_old = NULL) {
  if (is.null(ehd_old) || !inherits(ehd_old, "EcoHAB")) {
    warning("Old instance must be provided.")
    return()
  }
  suppressWarnings(ehd <- EcoHAB$new())
  ehd$.__enclos_env__$private$.raw <- ehd_old$raw
  ehd$.__enclos_env__$private$.idlist <- ehd_old$idlist
  ehd$.__enclos_env__$private$.timeline <- ehd_old$timeline
  ehd$.__enclos_env__$private$.timeline_bin <- ehd_old$timeline_bin
  ehd$.__enclos_env__$private$.events <- copy(ehd_old$.__enclos_env__$private$.events)
  for (result in ehd_old$all_results()) {
    ehd$.__enclos_env__$private$results[[result]] <- ehd_old$get_result(result)
  }
  ehd$.__enclos_env__$private$timezone <- ehd_old$.__enclos_env__$private$timezone
  ehd$.__enclos_env__$private$loc_threshold <- ehd_old$.__enclos_env__$private$loc_threshold
  ehd$.__enclos_env__$private$follow_threshold <- ehd_old$.__enclos_env__$private$follow_threshold
  ehd$.__enclos_env__$private$n_partial <- ehd_old$.__enclos_env__$private$n_partial
  ehd$.__enclos_env__$private$n_fixed <- ehd_old$.__enclos_env__$private$n_fixed
  ehd
}
