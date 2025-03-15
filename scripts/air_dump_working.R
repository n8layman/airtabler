air_dump <- function(base, metadata = NULL, description = NULL,
                     add_missing_fields = TRUE,
                     download_attachments = TRUE,
                     attachment_fields = NULL,
                     polite_downloads = TRUE,
                     field_names_to_snakecase = TRUE,
                     ...) {
  
  # If no metadata is provided, attempt to retrieve it from the base
  if(is.null(metadata)){
    message("No metadata provided. Metadata will be retrieved from the metadata table or generated via API")
    
    # Get the schema structure of the Airtable base
    base_schema <- air_get_schema(base)
    
    # Extract all table names from the schema
    all_table_names <- base_schema$tables$name
    
    # Look for a metadata table in the base (case insensitive)
    metadata_table_exists <- grepl("meta data", all_table_names, ignore.case = TRUE)
    
    if(any(metadata_table_exists)){
      # If metadata table exists, fetch it from the base
      message("Retrieving metadata from table")
      metadata <- fetch_all(base = base, table_name = all_table_names[metadata_table_exists])
    } else {
      # If no metadata table exists, generate metadata using the API
      message("Generating metadata from api with air_generate_metadata_from_api")
      metadata <- air_generate_metadata_from_api(base = base)
    }
  }
  
  # Convert all metadata column names to snake_case for consistency
  names(metadata) <- snakecase::to_snake_case(names(metadata))
  if(field_names_to_snakecase) metadata$field_name <- snakecase::to_snake_case(metadata$field_name)
  
  # Define and check for required fields in the metadata
  required_metadata_fields <- c("table_name", "field_name")
  
  if(!all(required_metadata_fields %in% names(metadata))){
    stop(glue::glue("metadata table must contain the 
                    following fields: {required_metadata_fields}. Note
                    that field names are converted to snakecase
                    before check."))
  }
  
  # Get unique table names from the metadata
  base_table_names <- unique(metadata$table_name)
  
  # Display the tables that will be processed
  print(base_table_names)
  
  # Process each table in the base and create a named list of tables
  table_list <- base_table_names |>
    purrr::set_names() |>
    purrr::map(function(table_name) {
      
      # Fetch all records from the current table
      current_table <- airtabler::fetch_all(base, table_name)

      # Get expected field names for this table from metadata
      expected_fields <- metadata[metadata$table_name == table_name, "field_name"]

      # Convert field names to snake_case if option is enabled
      if(field_names_to_snakecase) {
        names(current_table) <- snakecase::to_snake_case(names(current_table))
        expected_fields <- snakecase::to_snake_case(expected_fields)
      }
      
      # If table is empty or doesn't exist, create an empty dataframe with an id column
      if(!is.data.frame(current_table)){
        current_table <- data.frame(id = character())
      }
      
      # Identify observed fields (column names) in the fetched table
      observed_fields <- names(current_table)
      
      # Find differences between expected and observed fields
      fields_difference <- setdiff(expected_fields, observed_fields)
      
      # Handle field discrepancies if any exist
      if(!is.null(fields_difference)){
        # Check for fields in table but not in metadata (observed but not expected)
        fields_in_table_not_in_metadata <- setdiff(observed_fields, expected_fields)
        
        # Define system fields that are allowed to exist without being in metadata
        system_fields <- c("id", "createdTime", "created_time")
        system_fields_pattern <- paste(system_fields, collapse = "|")
        
        # If there are non-system fields in the table that aren't in metadata, throw an error
        if(length(fields_in_table_not_in_metadata) != 0 && 
           !all(fields_in_table_not_in_metadata %in% system_fields)){
          
          # Filter out system fields from the list of missing metadata fields
          undocumented_fields <- fields_in_table_not_in_metadata[
            !grepl(system_fields_pattern, fields_in_table_not_in_metadata, ignore.case = FALSE)
          ]
          
          # Format the list of undocumented fields for the error message
          undocumented_fields_formatted <- paste(undocumented_fields, collapse = "\n")
          
          # Stop execution with informative error message
          stop(glue::glue('The metadata table is missing the following fields from table {table_name}:

                          {undocumented_fields_formatted}

                          Please update the metadata table via R with air_update_metadata_table
                          or manually at https://airtable.com/{base}'))
        }
        
        # Add fields that exist in metadata but not in table, if requested
        if(add_missing_fields){
          fields_in_metadata_not_in_table <- setdiff(expected_fields, observed_fields)
          
          # Add empty columns for fields that are missing from the table
          current_table[fields_in_metadata_not_in_table] <- list(character(0))
        }
      }
      
      # Process attachments if enabled
      if(download_attachments){
        
         #Create a local copy of attachment_fields for this table iteration
        table_attachment_fields <- attachment_fields

          # Verify we can identify which fields contain attachments
        if(rlang::is_empty(table_attachment_fields) && !"field_type" %in% names(metadata)){
          rlang::abort("Unclear which fields contain attachments.
                      Either use the attachment_fields argument or
                      supply a metadata dataframe with field_types == 'multipleAttachments' for
                      fields that should be downloaded")
        } 

        # Determine which fields contain attachments
        if(is.null(table_attachment_fields)) {
        
          table_metadata <- metadata[metadata$table_name == table_name, c("field_name", "field_type")]
          
          # Extract fields of type 'multipleAttachments'
          table_attachment_fields <- table_metadata |>
            dplyr::filter(field_type == "multipleAttachments") |>
            dplyr::pull(field_name)
          
          # Inform user if no attachment fields are found
          if(rlang::is_empty(table_attachment_fields)) {
            rlang::inform("No fields of type multipleAttachment. No files to download")
            return(current_table)
          }
        }
        
        # Verify the attachment fields exist in the table
        if(is.character(table_attachment_fields)){
          if(!any(table_attachment_fields %in% names(current_table))) {
            return(current_table)
          }
        }
        
        # Configure download delay for polite API usage
        download_delay <- 0
        if(polite_downloads){
          download_delay <- 0.01  # 10ms delay between downloads
        }
        
        # Process each attachment field
        for(attachment_field in table_attachment_fields){
          Sys.sleep(download_delay)
          # Download attachments for the current field
          current_table <- air_download_attachments(current_table, field = attachment_field, ...)
        }
      }
      
      return(current_table)
    })
  
  # Add metadata to the result list
  table_list$metadata <- metadata
  
  # Check if there's a description table in the results
  has_description_table <- grepl(pattern = "description", x = names(table_list), ignore.case = TRUE)
  
  # Handle description data based on parameters and existing data
  if(!is.null(description)){
    if(any(has_description_table)){
      warning("Base has a description table and a description data.frame was supplied to
              this function. Inserting description data.frame at $description. Table
              extract may be overwritten.")
    }
    table_list$description <- description
  } else {
    # If no description provided and none exists in the results, generate a default one
    if(all(!has_description_table)){
      table_list$description <- air_generate_base_description()
    }
  }
  
  # Return the complete list of tables, metadata, and description
  return(table_list)
}
