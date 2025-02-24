% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-grid.R
\name{register_links_grid_callbacks}
\alias{register_links_grid_callbacks}
\title{Register observer to receive callback from links module}
\usage{
register_links_grid_callbacks(ids, in_vals, obs, vals)
}
\arguments{
\item{ids}{Blocks ids.}

\item{in_vals}{Incoming values containint grid information.}

\item{obs}{Observers list.}

\item{vals}{Local reactive values.}
}
\description{
This allows to maintain the state of the sidebar in grid switch
while the node switch is updated.
}
\keyword{internal}
