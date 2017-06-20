#' query_id
#'
#' Creates tibble with query output file names and query names
#' @param folder Directory with GCAM queries only
#' @export
#' @examples
#' query_id(output_dir = "gcam_queries")

query_id <- function(folder){
  if (substr(folder, nchar(folder), nchar(folder)) != "/"){
    folder <- paste0(folder, "/")
  }
  out_df <- tibble::tibble(file = character(), title = character())
  input_files <- dir(folder)
  for (file in input_files){
    line <- readr::read_lines(paste0(folder,file), n_max =1) %>%
      # Assumes no commas in any query title
      stringr::str_replace_all(",","")
    out_df <- out_df %>% dplyr::bind_rows(tibble::tibble(file = file, title = line ))
  }
  return(out_df)
}
