% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/climate_data.R
\name{temperature_data}
\alias{temperature_data}
\title{temperature_data}
\usage{
temperature_data(query, scenarios, query_dir = QUERY_FOLDER)
}
\arguments{
\item{query}{GCAM query containing temperature data of one or multiple scenarios}

\item{scenario}{GCAM scenarios to include in processed data}
}
\description{
Process temperature query
}
\examples{
temperature_data("queryA.csv", c("Reference1,date=2017-9-6T13:43:53-07:00", "Reference2,date=2017-9-6T13:43:53-07:00"))
}
\keyword{temperature}
