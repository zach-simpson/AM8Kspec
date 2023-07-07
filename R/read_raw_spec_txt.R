read_raw_spec_txt <- function(filename){
  out <- read_csv(file = filename) %>%
    mutate(text_row = row_number()) %>% # can help with cleaning
    rename(text_col = `TEST SETUP`) %>% select(text_row, text_col)
  return(out)
}
