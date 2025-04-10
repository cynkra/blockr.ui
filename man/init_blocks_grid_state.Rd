% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-grid.R
\name{init_blocks_grid_state}
\alias{init_blocks_grid_state}
\title{Whether blocks should be in grid or not}
\usage{
init_blocks_grid_state(blocks, vals)
}
\arguments{
\item{blocks}{Board blocks.}

\item{vals}{Local reactive values.}
}
\description{
Whenever a new block is created or some blocks
are removed, we update the mapping to know which
block should be in the grid, so that \link{manage_board_grid}
knows what to do.
}
\keyword{internal}
