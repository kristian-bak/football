#' Get goalscoring type
#' @param df tibble with goal scored (df_goals obtained from get_match_report)
#'
get_goalscoring_type <- function(df) {

  df %>%
    dplyr::filter(X2 != "goals") %>%
    dplyr::pull(X2) %>%
    kb.utils::remove_everything_before(pattern = "/", x = .) %>%
    kb.utils::remove_everything_after(pattern = "\\(", x = .) %>%
    trimws()

}
