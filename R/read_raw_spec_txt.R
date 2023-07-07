#' Utility function to read in the raw text output from the machine & standardise it. This is just an initial step before prettying it up.
#'
#' @param filename The txt file to read from, given in the machine's lovely format.
#'
#' @return A standardised tibble with just 3 columns -- these are for subsequent use.
#' @export
#'
# @examples # TODO
read_raw_spec_txt <- function(filename){
  out <- readr::read_csv(file = filename) %>%
    dplyr::mutate(text_row = dplyr::row_number()) %>% # can help with cleaning
    dplyr::rename(text_col = `TEST SETUP`) %>%
    dplyr::select(text_row, text_col)
  return(out)
}
