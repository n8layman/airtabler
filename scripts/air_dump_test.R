devtools::install_github("n8layman/airtabler")
library(airtabler)
library(tidyverse)

# Get list of bases your token has access to
eha_bases <- air_list_bases()

# Choose a base e.g., NY city food recommendations
base <- "appCM0UwGXlCID8Se"

base_schema <- air_get_schema(base)

table_name <- base_schema$tables$name

table_json <- air_get_json(base, table_name)

base_dump <- air_dump(base)

base_metadata <- air_generate_metadata_from_api(base)
air_dump_to_json(base, metadata, overwrite = T)

?air_dump_to_json
