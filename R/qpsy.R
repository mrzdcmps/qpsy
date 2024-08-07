# Functions to use for online experiments with jsPsych on qpsy.de
# General Psychology II // Department Psychology LMU
# Dr. Moritz Dechamps

#' Load Experimental Files from Server
#'
#' Read all results files (.csv) from the server and write to dataframe
#' @param exp String. The name of the data folder of the experiment to be loaded.
#' @param site Either "q" for qpsy.de or "g" for ganzfeld.study.
#' @param subdirs Logical. Should subdirectories be considered?
#' @param splitreponse Logical. Should survey responses be automatically written into separate columns?
#' @param localcopy Logical. Should a local copy be saved and used for future calls?
#' @return Dataframe containing all trials of the combined result files.
#' @examples
#' myexp <- loadexp("myexp")
#' @export

loadexp <- function(exp, site="q", subdirs=TRUE, splitresponse=TRUE, localcopy=TRUE){
  
  # which site to use
  domain <- "qpsy.de"
  if(site == "g") domain <- "ganzfeld.study"

  # check if config containing user and password is already downloaded
  key <- try(config::get("qpsy"), silent = T)

  # ask for password and download config if it is not loaded
  if(inherits(key, "try-error")){

    user <- "serverdata"
    pw <- rstudioapi::askForPassword("Please enter the password")

    download.file(paste0("https://",user,":",pw,"@qpsy.de/data/config.yml"), "config.yml")
    key <- config::get("qpsy")

  }

  # Give error if config could not be downloaded
  if(inherits(key, "try-error")) stop("Wrong credentials! Please try again.")

  # read the page
  burl <- paste0("https://",key$uid,":",key$pwd,"@",domain,"/data/")
  url <- paste0(burl,exp,"/")
  page <- rvest::read_html(url)

  # find the hrefs attributes which contain ".csv"
  filenames <- rvest::html_elements(page, xpath = ".//a[contains(@href, '.csv')]") %>% rvest::html_text()

  # look for subdirectories
  if(subdirs == TRUE){
    #list subdirectories
    folders <- rvest::html_elements(page, xpath = ".//a[contains(@href, '/')]") %>% rvest::html_text()
    #exclude "Parent directory"
    folders <- folders[-1]

    if(length(folders > 0)){
      for (f in folders){
        tmp <- .rff(paste0(exp,"/",f), site)
        if(length(tmp)>0) tmp <- paste0(f,tmp)
        filenames <- append(filenames, tmp)
      }

    }
  }

  # empty folder?
  if(length(filenames) == 0) stop("No files found in folder.")

  # create links
  links <- paste0(url, filenames)
  links <- gsub(" ", "%20", links)
  
  # look for local copy
  exp_escape <- gsub("/", "-", exp)
  if(localcopy == TRUE){
    if(file.exists(paste0("raw_",exp_escape,".rds"))){
      message("Local copy found.")
      old <- readRDS(paste0("raw_",exp_escape,".rds"))
      oldfiles <- length(unique(old$file))
      newfiles <- length(unique(filenames))
      if(oldfiles == newfiles){
        message("No new files found. Using local copy.")
        return(old)
      } else {
        message("New files found. Updating local copy.")
      }
    }
  }

  # read and bind files
  raw <- pbapply::pblapply(links, data.table::fread, showProgress = FALSE)
  out <- data.table::rbindlist(raw, id="file", fill=T)

  # fix double quotes on JSON input
  if("response" %in% colnames(out)) out$response <- gsub("\"\"", "\"", out$response)
  if("responses" %in% colnames(out)) out$responses <- gsub("\"\"", "\"", out$responses)
  if("view_history" %in% colnames(out)) out$view_history <- gsub("\"\"", "\"", out$view_history)

  # split responses
  if(splitresponse == TRUE){
    if ("response" %in% colnames(out)) {
      if (nrow(dplyr::filter(out, grepl("\\{\"", response))) > 0) {
        out <- splitresponse(out)
      }
    }
  }
  
  # save local copy
  if(localcopy == TRUE){
      saveRDS(out, file=paste0("raw_",exp_escape,".rds"))
  }

  out
}



# Helper function: Read files recursively

.rff <- function(exp, site){
  
  # which site to use
  domain <- "qpsy.de"
  if(site == "g") domain <- "ganzfeld.study"
  
  key <- config::get("qpsy")

  burl <- paste0("https://",key$uid,":",key$pwd,"@",domain,"/data/")
  url <- paste0(burl,exp,"/")
  page <- rvest::read_html(url)

  #find all .csv files
  filenames <- rvest::html_elements(page, xpath = ".//a[contains(@href, '.csv')]") %>% rvest::html_text()

  #list subdirectories
  folders <- rvest::html_elements(page, xpath = ".//a[contains(@href, '/')]") %>% rvest::html_text()
  #exclude "Parent directory"
  folders <- folders[-1]

  if(length(folders > 0)){
    for (f in folders){
      tmp <- .rff(paste0(exp,"/",f), site)
      if(length(tmp)>0) tmp <- paste0(f,tmp)
      filenames <- append(filenames, tmp)
    }

  }
  filenames
}


#' Write responses into variables.
#'
#' Write all answers from the "response"-column generated by a survey-plugin into individual variables.
#' @return Generates new variables (columns) named after the question and storing the given responses.
#' @examples
#' survey <- splitresponse(myexp)
#'
#' survey <- myexp %>%
#'   filter(trial_part == "survey") %>%
#'   splitresponse()
#'
#' @export

splitresponse <- function(data){
  if(!("response" %in% colnames(data))) stop("Dataframe does not contain a \"response\" column")
  if(nrow(dplyr::filter(data, grepl("\\{\"",response)))==0) stop("No responses found")

  message("Splitting responses...")
  data$line <- as.numeric(row.names(data))

  r <- data %>%
    dplyr::filter(grepl("\\{\"",response)) %>% #only lines that contain JSON responses
    dplyr::mutate(
      tmp = jsonlite::stream_in(textConnection(response), simplifyDataFrame = FALSE),
      tmp = lapply(tmp, function(x) replace(x,which(x==""),NA)),
      tmp = lapply(tmp, dplyr::bind_cols)
    ) %>%
    tidyr::unnest(tmp) %>%
    dplyr::select(line:last_col()) #select only new variables

  out <- dplyr::left_join(data, r, by="line")

  # fill to other rows of file
  if("file" %in% colnames(data)){
    out <- out %>%
      dplyr::group_by(file) %>%
      tidyr::fill(line:last_col() & -line, .direction = "downup") %>%
      dplyr::ungroup()
  } else if("subject" %in% colnames(data)){
    out <- out %>%
      dplyr::group_by(subject) %>%
      tidyr::fill(line:last_col() & -line, .direction = "downup") %>%
      dplyr::ungroup()
  }

  out %>%
    dplyr::select(-c(line))
}


