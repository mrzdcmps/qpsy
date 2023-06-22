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

igt <- loadexp("del_hmt")



url <- paste0("https://qpsy.de/data/del_hmt/")
page <- read_html(url)
filenames <- html_elements(page, xpath = ".//a[contains(@href)]")# %>% html_text()





#####








library(rvest)

#read the page
url <- "https://qpsy.de/data/del_age/study1/"
page <- read_html(url)

#find all .csv files
filenames <- html_elements(page, xpath = ".//a[contains(@href, '.csv')]") %>% html_text()

#look for subdirectories
if(subdirs == TRUE){



  #list subdirectories
  folders <- html_elements(page, xpath = ".//a[contains(@href, '/')]") %>% html_text()
  #exclude "Parent directory"
  folders <- folders[-1]

  if(length(folders > 0)){
    for (f in folders){
      page <- read_html(paste0(url,f))
      tmp <- html_elements(page, xpath = ".//a[contains(@href, '.csv')]") %>% html_text()
      if(length(tmp)>0) tmp <- paste0(f,tmp)
      filenames <- append(filenames, tmp)
    }

  }


}

filenames
links <- paste0(url, filenames)


rff <- function(exp){
  url <- "https://qpsy.de/data/"
  page <- read_html(paste0(url,exp))

  #find all .csv files
  filenames <- html_elements(page, xpath = ".//a[contains(@href, '.csv')]") %>% html_text()

  #list subdirectories
  folders <- html_elements(page, xpath = ".//a[contains(@href, '/')]") %>% html_text()
  #exclude "Parent directory"
  folders <- folders[-1]

  if(length(folders > 0)){
    for (f in folders){
      tmp <- rff(paste0(exp,f))
      filenames <- append(filenames, tmp)
    }

  }
  rfiles <- filenames

  rfiles
}

rff("del_hmt/")



