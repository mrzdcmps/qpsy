# Functions to use for online experiments with jsPsych on qpsy.de
# General Psychology II // Department Psychology LMU
# Dr. Moritz Dechamps

#' Load Experimental Files from Server
#'
#' Read all results files (.csv) from the server and write to dataframe
#' @param expfolder The data folder of the experiment to be loaded
#' @return Dataframe containing all trials of the combines result files
#' @examples
#' temp1 <- C_to_F(22);
#' temp2 <- C_to_F( c(-2, 12, 23) );
#' @export

loadexp <- function(expfolder){
  library(rvest)
  library(data.table)
  library(tidyverse)

  #read the page
  url <- paste0("https://qpsy.de/data/",expfolder,"/")
  page <- read_html(url)

  #find the hrefs attributes which contain ".csv"
  filenames <- html_elements(page, xpath = ".//a[contains(@href, '.csv')]") %>% html_text()
  links <- paste0(url, filenames)

  raw <- lapply(links, fread)
  rbindlist(raw, id="file", fill=T) %>%
    mutate(response=gsub("\"\"", "\"", response))
}
