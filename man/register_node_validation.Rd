% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-links.R
\name{register_node_validation}
\alias{register_node_validation}
\title{Register block validation observer}
\usage{
register_node_validation(vals, rv, session)
}
\arguments{
\item{vals}{Local reactive values.}

\item{rv}{Global reactive values.}

\item{session}{Shiny session object.}
}
\description{
For each block we register an observer that
captures only the messages related to this block validation
status. The observer is destroyed when the node is cleaned
by cleanup_node.
}
