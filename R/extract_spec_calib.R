#' Extract the calibration data from raw spec file.
#'
#' @param spec_raw The output from `read_raw_spec_txt`
#'
#' @return A tibble with the standards' IDs, absorbances, and concentrations.
#' @export
# @examples # TODO
extract_spec_calib <- function(spec_raw){

  spec_calib <- spec_raw %>%
    # hack off where the measurements start and onwards
    mutate(obs_start = dplyr::if_else(stringr::str_starts(text_col, 'Curve Fit') & text_row > 10,
                               # have to avoid first mention in meta info
                               T, NA)) %>%
    tidyr::fill(obs_start) %>%
    dplyr::filter(is.na(obs_start)) %>% # this drops the bottom rows
    # now hack off the early bits up to calib data
    dplyr::mutate(std_start = dplyr::if_else(stringr::str_starts(text_col, "Std    Conc.       Abs"),
                               TRUE,
                               NA)) %>%
    tidyr::fill(std_start) %>%
    dplyr::filter(!is.na(std_start)) %>%
    # at this point, should begin with Std  Conc.  Abs; now just keep the actual values
    dplyr::filter(stringr::str_starts(text_col, "[:digit:]")) %>%
    dplyr::mutate(
      std_id = stringr::str_split_i(text_col, "[:blank:]+", 1),
      std_conc = stringr::str_split_i(text_col, "[:blank:]+", 2),
      std_abs = stringr::str_split_i(text_col, "[:blank:]+", 3)
    ) %>%
    dplyr::select(std_id, std_conc, std_abs) %>%
    dplyr::mutate(std_id = as.integer(std_id),
                  dplyr::across(c(std_conc, std_abs), as.numeric))

  return(spec_calib)
}
