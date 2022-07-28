#' Get abbreviated team names
#' @param x character vector with team names
get_abbrev_team_names <- function(x) {

  ## Abbreviations are the first 3 characters
  abbrev <- substring(x, 1, 3)

  ## Expect those names containing spaces, then pasting the first letter after the space. Ex: ManC
  id_with_spaces <- grep(" ", x)

  names_with_spaces <- x[id_with_spaces]

  prefix <- substring(text = names_with_spaces, first = 1, last = 3)

  suffix <- kb.utils::remove_everything_before(pattern = " ", x = names_with_spaces) %>%
    substring(text = ., first = 1, last = 1)

  abbrev[id_with_spaces] <- paste0(prefix, suffix)

  if (length(abbrev) != length(unique(abbrev)) & length(x) == length(unique(x))) {

    warning("get_abbrev_team_names does not produce unique team names")

  }

  return(abbrev)

}
