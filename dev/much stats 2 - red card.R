data <- football::load_seasons(from = 19, to = 22)

data %>%
  dplyr::filter(HomeTeam == "Liverpool") %>%
  dplyr::mutate(Red = 1 * (HR > 0)) %>%
  dplyr::summarise(RedProb = sum(Red) / dplyr::n())

data %>%
  dplyr::filter(AwayTeam == "Liverpool") %>%
  dplyr::mutate(Red = 1 * (AR > 0)) %>%
  dplyr::summarise(RedProb = sum(Red) / dplyr::n())


1 / (1 - 0.0182 - 0.0727)

data_spain <- football::load_seasons(league = "La Liga", from = 19, to = 22)

get_team_names(data_spain)

data_spain %>%
  dplyr::filter(HomeTeam == "Villarreal") %>%
  dplyr::mutate(Red = 1 * (HR + AR) > 0) %>%
  dplyr::summarise(RedProb = sum(Red) / dplyr::n())

data_spain %>%
  dplyr::filter(HomeTeam == "Villarreal") %>%
  dplyr::mutate(Red = 1 * (HR > 0)) %>%
  dplyr::summarise(RedProb = sum(Red) / dplyr::n())

data_spain %>%
  dplyr::filter(AwayTeam == "Villarreal") %>%
  dplyr::mutate(Red = 1 * (AR > 0)) %>%
  dplyr::summarise(RedProb = sum(Red) / dplyr::n())




1 / 1.15

o = 1 / p

1 / (1 - 0.0946)
