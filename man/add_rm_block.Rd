% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plugin-add_rm_blocks.R
\name{add_rm_block_server}
\alias{add_rm_block_server}
\alias{add_rm_block_ui}
\title{Add/remove block module}
\usage{
add_rm_block_server(id, rv, ...)

add_rm_block_ui(id, board)
}
\arguments{
\item{id}{Namespace ID.}

\item{rv}{Reactive values object.}

\item{...}{Extra arguments passed from parent scope}

\item{board}{The initial \code{board} object}
}
\value{
A \code{\link[shiny:reactiveValues]{shiny::reactiveValues()}} object with components \code{add} and \code{rm},
where \code{add} may be \code{NULL} or a \code{block} object and \code{rm} be \code{NULL} or a string
(block ID).
}
\description{
Customizable logic for adding/removing blocks to the board.
}
