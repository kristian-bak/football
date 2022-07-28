#' Get assists from match report
#' @param df data.frame with goals scored (df_goals obtained from `get_match_report`)
#' @return character vector with assist names
#'
get_assists <- function(df) {

  df %>%
    dplyr::filter(X2 != "goals") %>%
    dplyr::pull(X2) %>%
    kb.utils::remove_everything_before(pattern = "\\(", x = .) %>%
    kb.utils::remove_everything_after(pattern = ")", x = .)

}
