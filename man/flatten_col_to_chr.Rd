% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/air_dump.R
\name{flatten_col_to_chr}
\alias{flatten_col_to_chr}
\title{Flatten list columns to character}
\usage{
flatten_col_to_chr(data_frame)
}
\arguments{
\item{data_frame}{a data frame, tibble or other data frame like object}
}
\value{
data_frame with list columns converted to character vectors.
}
\description{
Similar in spirit to purrr::flatten_chr except
that it can handle NULL values in lists and returns outputs
that can be written to csv.
}
\details{
Because the outputs are intended for use in CSV files, we must use
double quotes to indicate that the commas separating list values do
not delimit cells. This conforms to RFC 4180 standard for CSVs.
\url{https://datatracker.ietf.org/doc/html/rfc4180}
}
\examples{

data_frame <- data.frame(a = I(list(list("Hello"),
list("Aloha"),
NULL,
list("Hola","Bonjour","Merhaba")
)),
b = 1:4,
c = letters[1:4],
d = I(data.frame(id = 1:4, name = "bob", email = "bob@example.com"))
)

test_df <- flatten_col_to_chr(data_frame)

str(test_df)

}
