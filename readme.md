# airtabler Documentation

## Overview

The `airtabler` package provides tools for interacting with Airtable bases through the Airtable API. This package allows you to list bases, export data, and save exports in various formats including Excel workbooks, which can later be re-imported into Airtable.

## Installation

```r
# Install from GitHub
devtools::install_github("n8layman/airtabler")

# Load required packages
library(airtabler)
library(tidyverse)
```

## Authentication

Before using the package, you'll need to set up your Airtable API token. The token can be set using:

```r
# Set your Airtable API token (recommended to use .Renviron)
Sys.setenv(AIRTABLE_API_KEY="your_api_token")
```

For security, it's recommended to store your API token in your `.Renviron` file rather than hardcoding it in scripts.

## Basic Usage

### Listing Available Bases

```r
# Get a list of all bases your token has access to
bases <- air_list_bases() |> pluck("bases")
```

This returns a data frame with base information including:

- `id`: The base ID
- `name`: The base name
- `permissionLevel`: Your access level to the base

### Exporting Base Data

The package provides functions for exporting Airtable base data:

```r
# Generate metadata for a specific base
base_metadata <- air_generate_metadata_from_api(base_id)

# Export all data from a base to an R object
base_dump <- air_dump(
  base_id,
  metadata = base_metadata,
  base_path = "path/to/output/directory",
  attachment_folder = "attachments",
  overwrite = FALSE,
  remove_original_field = TRUE,
  organize_by_table_field = TRUE
)
```

### Exporting to Excel

You can export the base data to Excel format:

```r
# Export to Excel workbook
air_dump_to_xlsx(
  base_dump,
  output_file = "path/to/output.xlsx",
  base_name = "Base Name"
)
```

## Batch Processing Example

The following example demonstrates how to process and export multiple Airtable bases:

```r
# Get list of bases your token has access to
eha_bases <- air_list_bases() |> pluck("bases")

# Process each base
pwalk(eha_bases, function(id, name, permissionLevel) {
  # Display the current base name
  print(stringr::str_squish(name))

  # Create output directory
  output_dir <- paste0("/path/to/export/directory/",
                      gsub(" ", "_", stringr::str_squish(name)))

  # Generate metadata and export data
  base_metadata <- air_generate_metadata_from_api(id)
  base_dump <- air_dump(id,
                        metadata = base_metadata,
                        base_path = output_dir,
                        attachment_folder = "attachments",
                        overwrite = FALSE,
                        remove_original_field = TRUE,
                        organize_by_table_field = TRUE)

  # Export to Excel
  output_file <- paste0(output_dir, "/", basename(output_dir), "_", id, ".xlsx")
  air_dump_to_xlsx(base_dump, output_file = output_file, base_name = name)
})
```

## Re-importing to Airtable

The Excel files generated by `air_dump_to_xlsx()` are formatted for easy re-import into Airtable. To import:

1. Open your Airtable base in a web browser
2. Select the table where you want to import data
3. Click on the "+" button in the view bar
4. Select "Import data"
5. Choose "Upload a file" and select your Excel file
6. Follow the Airtable import wizard to map fields
7. Confirm and complete the import

Notes on re-importing:

- The Excel workbook contains separate sheets for each table in the base
- Linked records may need to be re-established during import
- Attachments are not included in the Excel file but are stored in the attachment folder
- For large bases, consider importing one table at a time

## Function Reference

### `air_list_bases()`

Returns a list of all Airtable bases your API token can access.

### `air_generate_metadata_from_api(base_id)`

Generates metadata for a specific base, which is required for export operations.

### `air_dump(base_id, metadata, ...)`

Exports all data from a base to an R object (not CSV files).

Parameters:

- `base_id`: The ID of the Airtable base to export
- `metadata`: Base metadata from `air_generate_metadata_from_api()`
- `base_path`: Directory used for storing attachments and other supplementary files
- `attachment_folder`: Subfolder name for attachments
- `overwrite`: Whether to overwrite existing files
- `remove_original_field`: Remove original field IDs after export
- `organize_by_table_field`: Organize exported files by table and field

This function returns an R object containing all the table data from the Airtable base. The data is stored in memory and can be further processed before writing to disk.

### `air_dump_to_xlsx(base_dump, output_file, base_name)`

Converts a base dump to an Excel workbook.

Parameters:

- `base_dump`: The output from `air_dump()`
- `output_file`: Path to save the Excel file
- `base_name`: Name to use in the workbook metadata

## Error Handling

Most functions will return errors with informative messages if:

- Your API token is invalid or expired
- You don't have permission to access a base
- A base ID doesn't exist
- There are connection issues with the Airtable API

## Dependencies

- tidyverse
- httr
- jsonlite
- openxlsx
- fs
