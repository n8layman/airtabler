#' Get an ID from a URL
#'
#' General function for parsing airtable URLs to find base, table, view, or record id's
#'
#' @param url String. A url generated by airtable
#' @param pattern  String. A regex pattern for identifying the type of id you would like to get
#' @param id_type String. One of base_id, table_id, view_id, or record_id.
#' @param split_pattern String. Where should the URL be split? default is forward slashes "/" and questionmarks "?"
#'
#' @return String
#' @export air_get_id_from_url
#'
#' @examples
#'
#' url <- "https://airtable.com/apphDEokVZ9gvPNFk/tblaKC1ADBafoHVXN/viwteUgD7vaMBruHR/recMzdoM43RVRWybD?blocks=hide"
#'
#' # General function for parsing url components
#' air_get_id_from_url(url, '^app',id_type = "base_id")
#' # Get different components
#' air_get_base_id_from_url(url)
#' air_get_table_id_from_url(url)
#' air_get_view_id_from_url(url)
#' air_get_record_id_from_url(url)
#'
air_get_id_from_url <- function(url, pattern, id_type, split_pattern = "/|\\?" ){

  url_split <- stringr::str_split_1(string = url,pattern =  split_pattern)

  id_filter <- stringr::str_detect(url_split,pattern = pattern)

  if(all(!id_filter)){

    err_msg <- glue::glue("{id_type} should conform to the pattern {pattern}.\nNo {id_type} found in {url}.")

    rlang::abort(err_msg)
  }

  id <- url_split[id_filter]

  return(id)
}



#' @describeIn air_get_id_from_url Get the base id
#' @export air_get_base_id_from_url
#'
air_get_base_id_from_url <- function(url,pattern = "^app\\w{13}"){

  id <- air_get_id_from_url(url = url, pattern = pattern,id_type = "base_id")

  return(id)
}

#' @describeIn air_get_id_from_url Get the table id
#' @export air_get_table_id_from_url
#'
air_get_table_id_from_url<- function(url, pattern = "^tbl\\w{13}"){
  id <- air_get_id_from_url(url = url,  pattern = pattern,id_type = "table_id")
  return(id)
}

#' @describeIn air_get_id_from_url Get the view id
#' @export
#'
air_get_view_id_from_url<- function(url,  pattern = "^viw\\w{13}"){
  id <- air_get_id_from_url(url = url,  pattern = pattern,id_type = "view_id")
  return(id)
}

#' @describeIn air_get_id_from_url Get the record id
#' @export air_get_record_id_from_url
#'
air_get_record_id_from_url<- function(url,  pattern = "^rec\\w{13}"){
  id <- air_get_id_from_url(url = url,  pattern = pattern,id_type = "record_id")
  return(id)
}
