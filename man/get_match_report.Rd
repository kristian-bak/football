% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_match_report.R
\name{get_match_report}
\alias{get_match_report}
\title{Get match report}
\usage{
get_match_report(league, season, home_team, away_team)
}
\arguments{
\item{league}{league name (for instance "premier-league")}

\item{season}{season (for insance "2021-2022")}

\item{home_team}{name of home team (for instance "arsenal-fc")}

\item{away_team}{name of away team (for instance "manchester-united")}
}
\value{
named list with objects
* df_goals: tibble with columns X1 and X2 providing goal scoring and goal scorer information
* home_team: tibble with columns X1 (shirt number), X2 (name of player) and X3 (minute of substituion)
* away_team: tibble similar to that of home_team
* managers: tibble with column X1 and X2 providing information on managers for home and away team
* info: tibble with stadium name, number of participants and referees
}
\description{
Get match report
}
