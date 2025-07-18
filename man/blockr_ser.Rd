% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-serialize.R
\name{blockr_ser.dag_board}
\alias{blockr_ser.dag_board}
\alias{blockr_ser.dock}
\alias{blockr_ser.network}
\alias{blockr_ser.data.frame}
\alias{blockr_deser.dag_board}
\alias{blockr_deser.dock}
\alias{blockr_deser.network}
\alias{blockr_deser.data.frame}
\title{Ser/deser board}
\usage{
\method{blockr_ser}{dag_board}(
  x,
  blocks = NULL,
  network = NULL,
  grid = NULL,
  selected = NULL,
  mode = NULL,
  options = NULL,
  ...
)

\method{blockr_ser}{dock}(x, ...)

\method{blockr_ser}{network}(x, ...)

\method{blockr_ser}{data.frame}(x, ...)

\method{blockr_deser}{dag_board}(x, data, ...)

\method{blockr_deser}{dock}(x, data, ...)

\method{blockr_deser}{network}(x, data, ...)

\method{blockr_deser}{data.frame}(x, data, ...)
}
\arguments{
\item{x}{Board.}

\item{blocks}{Board blocks.}

\item{network}{visNetwork data.}

\item{grid}{gridstack data.}

\item{selected}{Selected node.}

\item{mode}{App mode.}

\item{options}{Board options.}

\item{...}{Generic consistency.}

\item{data}{Data to restore.}
}
\description{
Ser/deser board
}
