% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/resource.R
\name{ridl_resource_search}
\alias{ridl_resource_search}
\alias{rr_search}
\title{Search for RIDL resources}
\usage{
ridl_resource_search(query = "*:*", configuration = NULL, ...)

rr_search(query = "*:*", configuration = NULL, ...)
}
\arguments{
\item{query}{character, a query}

\item{configuration}{RIDLConfig, a configuration}

\item{...}{extra params}
}
\value{
A list of RIDLResource
}
\description{
Search for RIDL resources
}
\details{
Search and find datasets on RIDL
}
\examples{
\dontrun{
 # Setting the config to use RIDL default server
 search_ridl_resource("format:xlsx")
}

}
