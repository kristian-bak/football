% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/probalize_goals_before_minute_x.R
\name{probalize_goals_before_minute_x}
\alias{probalize_goals_before_minute_x}
\title{Probalize goals before minute x}
\usage{
probalize_goals_before_minute_x(list_goals, x)
}
\arguments{
\item{list_goals}{list obtained from `vectorized_minutes_goals_scored`}

\item{x}{integer specifying minute of interest}
}
\value{
named list with:
* n_games: number of games used
* n_games_with_goals_before_minute_x
* p: probability of the event
* o: the odds of the event
}
\description{
Probalize goals before minute x
}
