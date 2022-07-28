#' Get minutes goals scored
#' @param df data.frame with columns X1 (= the score) and X2 (= string with goal scorer, minute, how the goal was scored and assist)
#' @return integer vector with minutes
#'
get_minutes_goals_scored <- function(df) {

  df %>%
    dplyr::filter(X2 != "goals") %>%
    dplyr::pull(X2) %>%
    kb.utils::remove_everything_after(pattern = "\\.") %>%
    kb.utils::remove_everything_but_digits() %>%
    as.integer()

}
