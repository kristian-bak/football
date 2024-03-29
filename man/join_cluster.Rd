% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cluster.R
\name{join_cluster}
\alias{join_cluster}
\title{Join cluster information}
\usage{
join_cluster(data, var_names, k)
}
\arguments{
\item{data}{tibble with modeling data (see get_modeling_data_*)}

\item{var_names}{character vector with variable names used to create the cluster (by default Shots, ShotsOnTarget, Fouls, Corners, YellowCard and RedCard wil be used to create the cluster as these columns cannot be used to predict)}

\item{k}{number of clusters}
}
\value{
tibble with Cluster and ClusterOpponent joined onto data input
}
\description{
Join cluster information
}
