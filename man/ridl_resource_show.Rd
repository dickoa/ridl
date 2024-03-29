% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/resource.R
\name{ridl_resource_show}
\alias{ridl_resource_show}
\alias{rr_show}
\title{Pull a RIDL resource}
\usage{
ridl_resource_show(identifier, configuration = NULL)

rr_show(identifier, configuration = NULL)
}
\arguments{
\item{identifier}{character, a RIDLResource id}

\item{configuration}{RIDLConfig, the configuration used}
}
\value{
RIDLResource
}
\description{
Pull a RIDL resource
}
\examples{
\dontrun{
 # Setting the config to use RIDL default server
 ridl_config_set()
 res <- ridl_resource_show("98aa1742-b5d3-40c3-94c6-01e31ded6e84")
 res
}
}
