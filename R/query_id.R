#' query_id
#'
#' Creates tibble with query output file names and query names
#' @param folder Directory with GCAM queries only
#' @import tidyverse stringr
#' @export
#' @examples
#' query_id(output_dir = "gcam_queries")

query_id <- function(folder){
  out_df <- tibble(file = character(), title = character())
  input_files <- dir(folder)
  for (file in input_files){
    line <- read_lines(paste0(folder,file), n_max =1) %>%
      # Assumes no commas in any query title
      str_replace_all(",","")
    out_df <- out_df %>% bind_rows(tibble(file = file, title = line ))
  }
  return(out_df)
}
