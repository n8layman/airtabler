#' Process a table for Excel export
#'
#' Helper function to prepare a dataframe for Excel export in a way that's compatible with Airtable import.
#' Converts list columns to character strings, handles attachments, and optionally preserves ID columns
#' to maintain relationships between tables.
#'
#' @author Nathan C. Layman
#'
#' @param df Data frame. The table from air_dump() to process
#' @param preserve_ids Logical. If TRUE (default), keeps ID columns which are necessary for maintaining
#'        relationships between tables. Set to FALSE if you want to create fresh records without preserving links.
#' @param clean_attachment_cols Logical. If TRUE (default), converts attachment columns
#'        to a format that Airtable can understand during import. Attachments will be represented
#'        by their IDs as comma-separated strings.
#'
#' @return Data frame processed for Excel export with appropriate column types for Airtable import
#'
#' @details
#' This function handles several types of Airtable data:
#' - List columns (such as linked records, attachments, and multi-select fields) are converted to comma-separated strings
#' - Attachment columns are specially processed to extract IDs
#' - NULL values are converted to empty strings
#'
#' @keywords internal
process_table_for_excel <- function(df, preserve_ids = TRUE, clean_attachment_cols = TRUE) {
  # If empty dataframe, return as is
  if (nrow(df) == 0) {
    return(df)
  }

  # Process each column
  for (col in names(df)) {
    # Identify list-type columns (linked records, attachments, etc.)
    if (is.list(df[[col]])) {
      # Convert list columns to character for Excel
      df[[col]] <- sapply(df[[col]], function(x) {
        if (is.null(x)) {
          return("")
        } else if (is.list(x)) {
          # For attachments or complex lists
          if (length(x) > 0 && "id" %in% names(x[[1]])) {
            # This is likely an attachment or linked record with IDs
            ids <- sapply(x, function(item) item$id)
            return(paste(ids, collapse = ","))
          } else {
            # Generic list handling
            return(paste(unlist(x), collapse = ","))
          }
        } else {
          return(paste(x, collapse = ","))
        }
      })
    }
  }

  # Optionally remove system columns that shouldn't be imported
  if (!preserve_ids) {
    # Remove Airtable system columns
    system_cols <- c("id", "createdTime")
    df <- df[, !names(df) %in% system_cols, drop = FALSE]
  }

  return(df)
}
