devtools::install_github("n8layman/airtabler")
library(airtabler)
library(tidyverse)

# Get list of bases your token has access to
eha_bases <- air_list_bases() |> pluck("bases")
name <- eha_bases[11,]$name
id <- eha_bases[11,]$id

pwalk(eha_bases, function(id, name, permissionLevel) {
  print(stringr::str_squish(name))
  output_dir <- paste0("/Users/nathanlayman/EHA Dropbox/Nathan Layman/airtable_export/", gsub(" ", "_", stringr::str_squish(name)))

  base_metadata <- air_generate_metadata_from_api(id)
  base_dump <- air_dump(id,
                        metadata = base_metadata,
                        base_path = output_dir,
                        attachment_folder = "attachments",
                        overwrite = TRUE,
                        remove_original_field = TRUE,
                        organize_by_table_field = TRUE)

  output_file <- paste0(output_dir, "/", basename(output_dir), "_", id, ".xlsx")
  air_dump_to_xlsx(base_dump, output_file = output_file, base_name = name)
})
