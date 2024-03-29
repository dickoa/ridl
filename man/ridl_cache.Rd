% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/caching.R
\name{ridl_cache}
\alias{ridl_cache}
\alias{ridl_cache_set_dir}
\alias{ridl_cache_get_dir}
\alias{ridl_cache_list}
\alias{ridl_cache_delete}
\alias{ridl_cache_clear}
\title{Caching RIDL downloaded files}
\usage{
ridl_cache_set_dir(path)

ridl_cache_get_dir()

ridl_cache_list()

ridl_cache_delete(file)

ridl_cache_clear()
}
\arguments{
\item{path}{character, directory to set}

\item{file}{Character, the file to delete}
}
\value{
the cache directory

the cache directory

list of files in the cache
}
\description{
Manage cached RIDL downloaded files
}
\details{
The default cache directory is
\verb{~/.cache/R/ridl_cache}, but you can set
your own path using \code{ridl_cache_set_dir()}
}
\examples{
\dontrun{
ridl_cache
## change the default cache directory
tmp <- tempdir()
ridl_cache_set_dir(tmp)

## print current cache directory
ridl_cache_get_dir()

## List available files in the current cache directory
ridl_cache_list()

l <- ridl_cache_list()[1] ## get the first file
ridl_cache_delete(l) ## delete it

ridl_cache_clear() ## delete all cached files
}
}
