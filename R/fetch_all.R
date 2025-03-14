fetch_all <- function(base, table_name, separator = "|||", ...) {
  out <- list()
  out[[1]] <- airtabler::air_get(base, table_name, combined_result = FALSE, ...)
  
  if(length(out[[1]]) == 0) {
    emptyTableMessage <- glue::glue("The queried view for {table_name} in {base} is empty")
    warning(emptyTableMessage)
    return(emptyTableMessage)
  } else {
    offset <- airtabler::get_offset(out[[1]])
    while (!is.null(offset)) {
      out <- c(out, list(airtabler::air_get(base, table_name, combined_result = FALSE, offset = offset, ...)))
      offset <- airtabler::get_offset(out[[length(out)]])
    }
    
    out <- dplyr::bind_rows(out)
    result_df <- cbind(id = out$id, out$fields, createdTime = out$createdTime, stringsAsFactors = FALSE)
    
    # Flatten list columns
    for (col in names(result_df)) {
      if (any(sapply(result_df[[col]], is.list))) {
        result_df[[col]] <- sapply(result_df[[col]], function(x) {
          if (is.list(x) && length(x) > 0) {
            # Filter out NULL elements
            non_null <- x[!sapply(x, is.null)]
            if (length(non_null) > 0) {
              # Convert all elements to character and join with separator
              return(paste(sapply(non_null, as.character), collapse = separator))
            }
          } else if (!is.null(x) && !is.na(x)) {
            # Handle single values
            return(as.character(x))
          }
          return(NA_character_)
        })
      }
    }
    
    return(result_df)
  }
}
