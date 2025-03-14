devtools::install_github("n8layman/airtabler")
library(airtabler)
library(tidyverse)

# Get list of bases your token has access to
eha_bases <- air_list_bases() |> pluck("bases")

pwalk(eha_bases, function(id, name, permissionLevel) {
  print(name)
  output_dir <- paste0("/Users/nathanlayman/EHA Dropbox/Nathan Layman/airtable_export/", gsub(" ", "_", name))

  base_metadata <- air_generate_metadata_from_api(id)
  base_dump <- air_dump(id, metadata = base_metadata)

  tryCatch(
    {
      air_dump_to_csv(base_dump, overwrite = TRUE, output_dir = output_dir)
    },
    error = function(e) {
      message("First attempt failed. Trying again with snake_case = FALSE...")
      air_dump_to_csv(base_dump, overwrite = TRUE, output_dir = output_dir, names_to_snake_case = FALSE)
    }
  )  

air_dump_to_json(id, base_metadata, overwrite = TRUE, output_dir = output_dir)
})

name <- eha_bases[9,]$name
id <- eha_bases[9,]$id
