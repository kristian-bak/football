

data <- load_seasons(from = 21, to = 22)

get_team_names(data)

data %>%
  dplyr::group_by(AwayTeam) %>%
  dplyr::summarise(ZeroHT = sum(HTAG == 0) / dplyr::n()) %>%
  dplyr::arrange(ZeroHT)

data %>%
  dplyr::filter(AwayTeam == "Tottenham") %>%
  dplyr::summarise(Games = dplyr::n(),
                   ZeroHT = sum(HTAG == 0) / Games)

data %>%
  dplyr::filter(HomeTeam == "Brentford") %>%
  dplyr::summarise(Games = dplyr::n(),
                   ZeroHT = sum(HTHG == 0) / Games)

data %>%
  dplyr::filter(AwayTeam == "Tottenham" & !HomeTeam %in% c("Liverpool", "Man City", "Chelsea", "Arsenal", "West Ham",
                                                           "Leicester", "Man United")) %>%
  dplyr::summarise(ZeroHT = sum(HTAG == 0) / dplyr::n())


data_summary %>%
  as.data.frame()

data <- data %>%
  dplyr::mutate(Yellow = HY + AY)

data_mod <- data %>% dplyr::filter(Season %in% "2122")
m <- glm(Yellow ~ HomeTeam - 1, data = data_mod, family = poisson())

summary(m)

data_mod$PredYellow <- predict(m)

data_mod %>%
  dplyr::mutate(Diff = abs(Yellow - PredYellow)) %>%
  dplyr::group_by(HomeTeam) %>%
  dplyr::summarise(Error = mean(Diff))

data %>%
  dplyr::filter(HomeTeam == "Liverpool") %>%
  dplyr::pull(HY) %>%
  mean() %>%
  log()



summarise_h2h <- function(data) {

  data %>%
    dplyr::mutate(
      Goals = FTHG + FTAG,
      Corners = HC + AC,
      Yellow = HY + AY,
      Red = HR + AR
    ) %>%
    dplyr::summarise(
      Goals = median(Goals),
      Corners = median(Corners),
      Yellow = median(Yellow),
      Red = median(Red)
    )

}

summarise_h2h(data)

both_teams_scores <- function(data) {

  data %>%
    dplyr::mutate(BothScores = 1 * (FTHG + FTAG) > 0) %>%
    dplyr::summarise(N = dplyr::n(),
                     Probability = sum(BothScores) / N)

}

more_than_n_yellow_cards <- function(data, n_cards) {

  data %>%
    dplyr::mutate(ManyCards = 1 * (HY + AY) > n_cards) %>%
    dplyr::summarise(Event = "More than n yellow cards",
                     n_cards = n_cards,
                     NumberOfGames = dplyr::n(),
                     NumberOfEvents = sum(ManyCards),
                     Probability = NumberOfEvents / NumberOfGames)

}

more_than_n_yellow_cards(data = data, n_cards = 2.5)

less_than_n_yellow_cards <- function(data, n_cards) {

  data %>%
    dplyr::mutate(ManyCards = 1 * (HY + AY) < n_cards) %>%
    dplyr::summarise(Event = "Less than n yellow cards",
                     n_cards = n_cards,
                     NumberOfGames = dplyr::n(),
                     NumberOfEvents = sum(ManyCards),
                     Probability = NumberOfEvents / NumberOfGames)

}

less_than_n_yellow_cards(data = data, n_cards = 4.5)

red_card <- function(data) {

  data %>%
    dplyr::mutate(Red = 1 * (HR + AR) > 0) %>%
    dplyr::summarise(Event = "Red card",
                     NumberOfGames = dplyr::n(),
                     NumberOfEvents = sum(Red),
                     Probability = NumberOfEvents / NumberOfGames)

}

red_card(data = data)

goals <- function(data, n_goals) {

  data %>%
    dplyr::mutate(ManyGoals = 1 * (FTHG + FTAG) > n_goals) %>%
    dplyr::summarise(NumberOfGames = dplyr::n(),
                     NumberOfEvents = sum(ManyGoals),
                     Probability = NumberOfEvents / NumberOfGames)

}

goals(data = data, n_goals = 3.5)

both_teams_scores(data %>% dplyr::filter(HomeTeam == "Liverpool"))

odds_ratio <- function(odds, probability) {

  ratio <- as.numeric(odds / probability)

  return(ratio)

}

odds_ratio(odds = 2, probability = both_teams_scores(data %>% dplyr::filter(HomeTeam == "Liverpool")))
