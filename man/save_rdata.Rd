% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/imsbasics.R
\name{save_rdata}
\alias{save_rdata}
\title{Save a specific variable in environmet to file in specific folder
Call as cacheR::varToFile(data, filename, path)}
\usage{
save_rdata(
  data,
  filename,
  path = stop("Data, filename and path arguments are required"),
  force = F,
  warn = T
)
}
\arguments{
\item{filename, }{can be without ".RData". Required input.}

\item{path}{in "../.." style ending with "/".}

\item{data:}{An object in the environment. Required input.}
}
\description{
Save a specific variable in environmet to file in specific folder
Call as cacheR::varToFile(data, filename, path)
}
