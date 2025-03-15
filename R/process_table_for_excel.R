#' Process Table for Excel Export
#' 
#' This function processes a data frame to prepare it for export to Excel. 
#' It unnests data frame columns, appends the original column names as a prefix, 
#' and concatenates list columns into single string entries separated by a specified delimiter.
#' 
#' @author Nathan C. Layman
#' 
#' @param table_data A data frame containing columns of various types (data frames or lists).
#' @param concat_sep A string used as the separator for concatenating list column values. Defaults to a comma.
#' 
#' @return A data frame where:
#' - Data frame columns are unnested, and their original column names are used as a prefix.
#' - List columns are concatenated into a single string, with elements separated by `concat_sep`.
#' 
#' @examples
#' # Example usage:
#' table_data <- data.frame(
#'   id = 1:3,
#'   author = list(
#'     data.frame(id = 101:103, email = c("a@example.com", "b@example.com", "c@example.com"), name = c("Alice", "Bob", "Charlie"))
#'   ),
#'   tags = list(c("tag1", "tag2"), c("tag3", "tag4"), c("tag5"))
#' )
#' 
#' result <- process_table_for_excel(table_data)
#' print(result)
#' 
#' @export
process_table_for_excel <- function(
    table_data,
    concat_sep = ",") {

  # If empty dataframe, return as is
  if (nrow(table_data) == 0) {
    return(table_data)
  }

  # Identify which columns are data frames
   possibly_bind_rows <- possibly(
    .f = \(x) dplyr::bind_rows(x),
    otherwise = NA,  # Generic function to return whatever input was provided
    quiet = TRUE
  )

  # Woot!
  df_cols <- names(table_data)[map_lgl(table_data, ~possibly_bind_rows(.x) |> names() |> length() > 1)]
  table_data <- table_data |> unnest_wider(all_of(df_cols), names_sep = ".")

  # Identify which columns are lists
  list_cols <- names(table_data)[map_lgl(table_data, ~ is.list(.x))]
  
  # Concatenate list columns
  table_data <- table_data |>
    mutate(across(all_of(list_cols), ~ paste(unlist(.x), collapse = concat_sep)))

  max_char_lengths <- sapply(table_data, function(col) {
  if(is.character(col)) {
    max(nchar(col, type = "chars"), na.rm = TRUE)
  } else {
    max(nchar(as.character(col), type = "chars"), na.rm = TRUE)
  }
})

print(max_char_lengths)

  return(table_data)
}
