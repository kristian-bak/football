#' Join attack and defence flags based on team names
#' @param data tibble with modeling data (see get_modeling_data_*)
#' @param verbose logical indicating if loop counter should be on (default is FALSE)
#'
join_att_and_def_team_flags <- function(data, verbose = FALSE) {

  n_teams <- data %>%
    dplyr::pull(Team) %>%
    unique() %>%
    length()

  abbrev_team_names <- data %>%
    dplyr::pull(Team) %>%
    unique() %>%
    get_abbrev_team_names() %>%
    sort()

  mat <- matrix(data = 0, nrow = nrow(data), ncol = 2 * n_teams)

  colnames(mat) <- c(paste0("Att", abbrev_team_names),
                     paste0("Def", abbrev_team_names))

  data_tmp <- dplyr::bind_cols(
    data %>%
      dplyr::mutate(TeamAbbrev = get_abbrev_team_names(x = Team),
                    OpponentAbbrev = get_abbrev_team_names(x = Opponent)),
    mat
  )

  n <- nrow(data)

  for (i in 1:n) {

    str_att <- paste0("Att", data_tmp$TeamAbbrev[i])
    str_def <- paste0("Def", data_tmp$OpponentAbbrev[i])

    data_tmp[[str_att]][i] <- 1
    data_tmp[[str_def]][i] <- 1

    if (verbose) {

      kb.utils::loop_counter(i = i, n = n)

    }

  }

  return(data_tmp)

}

#' Join attack and defence flags based on a cluster
#' @param data tibble with modeling data (see get_modeling_data_*)
#' @param verbose logical indicating if loop counter should be on (default is FALSE)
#'
join_att_and_def_cluster_flags <- function(data, verbose = FALSE) {

  n_clusters <- data %>%
    dplyr::pull(Cluster) %>%
    unique() %>%
    length()

  cluster_names <- data %>%
    dplyr::pull(Cluster) %>%
    unique() %>%
    sort()

  mat <- matrix(data = 0, nrow = nrow(data), ncol = 2 * n_clusters)

  colnames(mat) <- c(paste0("Att", cluster_names),
                     paste0("Def", cluster_names))

  data_tmp <- dplyr::bind_cols(
    data,
    mat
  )

  n <- nrow(data)

  for (i in 1:n) {

    str_att <- paste0("Att", data_tmp$Cluster[i])
    str_def <- paste0("Def", data_tmp$ClusterOpponent[i])

    data_tmp[[str_att]][i] <- 1
    data_tmp[[str_def]][i] <- 1

    if (verbose) {

      kb.utils::loop_counter(i = i, n = n)

    }

  }

  return(data_tmp)

}
