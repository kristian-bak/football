#' Join cluster information
#' @param data tibble with modeling data (see get_modeling_data_*)
#' @param var_names character vector with variable names used to create the cluster (by default Shots, ShotsOnTarget, Fouls, Corners, YellowCard and RedCard wil be used to create the cluster as these columns cannot be used to predict)
#' @param k number of clusters
#' @return tibble with Cluster and ClusterOpponent joined onto data input
#'
join_cluster <- function(data, var_names, k) {

  if (missing(var_names)) {

    var_names <- c("Shots", "ShotsOnTarget", "Fouls",
                   "Corners", "YellowCard", "RedCard")

  }

  data_mean <- data %>%
    dplyr::group_by(Team) %>%
    dplyr::summarise(dplyr::across(
      .cols = var_names,
      .fns = mean
    ), .groups = "drop") %>%
    as.data.frame()

  rownames(data_mean) <- data_mean$Team

  data_mean <- data_mean %>%
    dplyr::select(-Team)

  obj_kmeans <- stats::kmeans(data_mean, centers = 5)

  data_cluster <- dplyr::tibble(
    Team = names(obj_kmeans$cluster),
    Cluster = obj_kmeans$cluster %>% as.factor()
  )

  data_cluster_opponent <- data_cluster %>%
    dplyr::rename(Opponent = Team, ClusterOpponent = Cluster)

  data_out <- data %>%
    dplyr::left_join(data_cluster, by = "Team") %>%
    dplyr::left_join(data_cluster_opponent, by = "Opponent")

  return(data_out)

}
