% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-links.R
\name{remove_node_from_stack}
\alias{remove_node_from_stack}
\title{Remove node from group}
\usage{
remove_node_from_stack(id, parent, standalone = FALSE, session)
}
\arguments{
\item{id}{Node id.}

\item{parent}{Global reactive values to update data.}

\item{standalone}{Whether this function is called directly or from
\link{unstack_nodes}.}

\item{session}{Shiny session object.}
}
\description{
Given a node, update its data to remove it from a group (color, group, ...)
}
\keyword{internal}
