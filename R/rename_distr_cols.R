#' Rename distribution columns
#' @param data tibble (typically modeling data, see get_modeling_data_*)
rename_distr_cols <- function(data) {

  list(min, q25, mean, median, q75, max, sum, count_na, count_zero, count_one)

  data %>%
    dplyr::rename_with(.cols = dplyr::ends_with("_1"), .fn = ~gsub("_.*", "Min", .x)) %>%
    dplyr::rename_with(.cols = dplyr::ends_with("_2"), .fn = ~gsub("_.*", "Q25", .x)) %>%
    dplyr::rename_with(.cols = dplyr::ends_with("_3"), .fn = ~gsub("_.*", "Mean", .x)) %>%
    dplyr::rename_with(.cols = dplyr::ends_with("_4"), .fn = ~gsub("_.*", "Median", .x)) %>%
    dplyr::rename_with(.cols = dplyr::ends_with("_5"), .fn = ~gsub("_.*", "Q75", .x)) %>%
    dplyr::rename_with(.cols = dplyr::ends_with("_6"), .fn = ~gsub("_.*", "Max", .x)) %>%
    dplyr::rename_with(.cols = dplyr::ends_with("_7"), .fn = ~gsub("_.*", "Sum", .x)) %>%
    dplyr::rename_with(.cols = dplyr::ends_with("_8"), .fn = ~gsub("_.*", "CountNA", .x)) %>%
    dplyr::rename_with(.cols = dplyr::ends_with("_9"), .fn = ~gsub("_.*", "CountZero", .x)) %>%
    dplyr::rename_with(.cols = dplyr::ends_with("_10"), .fn = ~gsub("_.*", "CountOne", .x))

}
