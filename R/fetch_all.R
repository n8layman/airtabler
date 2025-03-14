fetch_all <- function(base, table_name, ...) {
  out <- list()
  out[[1]] <- airtabler::air_get(base, table_name, combined_result = FALSE,...)
  if(length(out[[1]]) == 0){
    emptyTableMessage <- glue::glue("The queried view for {table_name} in {base} is empty")
    warning(emptyTableMessage)
    return(emptyTableMessage)
  } else {
    offset <- airtabler::get_offset(out[[1]])
    while (!is.null(offset)) {
      out <- c(out, list(airtabler::air_get(base, table_name, combined_result = FALSE, offset = offset, ...)))
      offset <- airtabler::get_offset(out[[length(out)]])
    }
    
    # Normalize data types before binding
    # First, extract all records into a flat list
    all_records <- unlist(lapply(out, function(x) x$records), recursive = FALSE)
    
    # Process fields to ensure consistent types
    for (i in seq_along(all_records)) {
      for (field_name in names(all_records[[i]]$fields)) {
        # Handle date fields specifically (they can be lists or characters)
        if (grepl("Date", field_name, ignore.case = TRUE)) {
          if (is.list(all_records[[i]]$fields[[field_name]])) {
            # If it's a list, convert to character
            all_records[[i]]$fields[[field_name]] <- as.character(unlist(all_records[[i]]$fields[[field_name]]))
          }
        }
      }
    }
    
    # Create a new structure with normalized records
    normalized_out <- list(records = all_records)
    
    # Now bind the rows with normalized data
    result <- dplyr::bind_rows(normalized_out)
    cbind(id = result$id, result$fields, createdTime = result$createdTime,
          stringsAsFactors = FALSE)
  }
}
