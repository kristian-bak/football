devtools::load_all()

url <- "https://global.espn.com/football/matchstats?gameId=605668"

webpage <-  rvest::read_html(url)

out <- webpage %>%
  rvest::html_table()

## Home team abbrev home team
## Away team abbrev away team
webpage %>%
  rvest::html_nodes("div.team-info") %>%
  rvest::html_text2()


## Saves
out[[1]]

## Last 5 LFC games
out[[2]]

## Last 5 Wolves games
out[[3]]

## Last 5 H2H
out[[4]]

webpage <- rvest::read_html(url)

## Possesion
webpage %>%
  rvest::html_nodes("div.stat-graph") %>%
  rvest::html_text2()

## Getting all game ids:

url <- "https://www.espn.com/soccer/team/results/_/id/364/season/2021"

webpage <- rvest::read_html(url)

webpage %>%
  rvest::html_nodes("a.AnchorLink") %>%
  rvest::html_attr("href")
