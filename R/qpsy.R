# Functions to use for online experiments with jsPsych on qpsy.de
# General Psychology II // Department Psychology LMU
# Dr. Moritz Dechamps

#' Load and process experimental files from server
#'
#' Reads and combines CSV files from server, optionally processing JSON response data
#' @param exp String. The name of the data folder of the experiment to be loaded.
#' @param site Either "q" for qpsy.de or "g" for ganzfeld.study.
#' @param subdirs Logical. Should subdirectories be considered? Default: TRUE
#' @param splitresponse Logical. Should survey responses be automatically written into separate columns? Default: TRUE
#' @param localcopy Logical. Should a local copy be saved and used for future calls? Default: TRUE
#' @param response_col Character. Name of the column containing JSON responses (default: "response")
#' @param fill_direction Character. Direction to fill split responses: "downup" (default), "down", "up", or "none"
#' @return Dataframe containing all trials of the combined result files
#' @examples
#' # Basic usage
#' myexp <- loadexp("myexp")
#' 
#' # Custom response processing
#' myexp <- loadexp("myexp", 
#'                  splitresponse = TRUE,
#'                  response_col = "responses")
#' @export
loadexp <- function(exp, 
                    site = "q", 
                    subdirs = TRUE, 
                    splitresponse = TRUE, 
                    localcopy = TRUE,
                    response_col = "response",
                    fill_direction = "downup") {
  
  # Input validation
  if (!is.character(exp)) stop("exp must be a character string")
  if (!site %in% c("q", "g")) stop("site must be either 'q' or 'g'")
  
  # Domain mapping
  domains <- c(q = "qpsy.de", g = "ganzfeld.study")
  domain <- domains[site]
  
  # Get credentials
  credentials <- get_credentials()
  if (is.null(credentials)) {
    return(NULL)  # Error message already shown by get_credentials()
  }
  
  # Construct base URL
  base_url <- paste0("https://", credentials$uid, ":", credentials$pwd, "@", domain, "/data/")
  url <- paste0(base_url, exp, "/")
  
  # Try to read the page
  page <- tryCatch({
    rvest::read_html(url)
  }, error = function(e) {
    message("Could not connect to server. Please check your internet connection and credentials.")
    return(NULL)
  })
  
  if (is.null(page)) return(NULL)
  
  # Get files
  filenames <- get_files(page, exp, site, subdirs, base_url)
  if (length(filenames) == 0) {
    message("No .csv files found in the specified folder.")
    return(data.frame())
  }
  
  # Process local copy if enabled
  exp_escape <- gsub("/", "-", exp)
  if (localcopy) {
    local_result <- process_local_copy(exp_escape, filenames)
    if (!is.null(local_result)) return(local_result)
  }
  
  # Create and clean links
  links <- paste0(url, filenames)
  links <- gsub(" ", "%20", links)
  
  # Read and combine files
  message("Reading files from server...")
  raw_data <- read_and_combine_files(links)
  if (is.null(raw_data)) return(data.frame())
  
  # Process responses if requested
  if (splitresponse && response_col %in% colnames(raw_data)) {
    raw_data <- process_responses(raw_data, 
                                  response_col = response_col,
                                  fill_direction = fill_direction)
  }
  
  # Save local copy if enabled
  if (localcopy) {
    saveRDS(raw_data, file = paste0("raw_", exp_escape, ".rds"))
  }
  
  raw_data
}

get_credentials <- function() {
  # Try to get existing config
  key <- try(config::get("qpsy"), silent = TRUE)
  
  if (!inherits(key, "try-error")) {
    return(key)
  }
  
  # If no config, ask for password
  user <- "serverdata"
  
  # Try RStudio API first, fall back to console
  if (rstudioapi::isAvailable()) {
    pw <- rstudioapi::askForPassword("Please enter the password")
  } else {
    pw <- readline("Please enter the password: ")
  }
  
  # Try to download config
  tryCatch({
    download.file(
      paste0("https://", user, ":", pw, "@qpsy.de/data/config.yml"), 
      "config.yml",
      quiet = TRUE
    )
    config::get("qpsy")
  }, error = function(e) {
    message("Failed to authenticate. Please check your credentials.")
    NULL
  })
}

safely_read_csv <- function(path) {
  tryCatch({
    data.table::fread(path, showProgress = FALSE)
  }, error = function(e) {
    message(sprintf("Failed to read file: %s", path))
    NULL
  })
}

clean_json_fields <- function(df) {
  json_cols <- c("response", "responses", "view_history")
  for (col in json_cols) {
    if (col %in% colnames(df)) {
      df[[col]] <- gsub("\"\"", "\"", df[[col]])
    }
  }
  df
}

process_local_copy <- function(exp_escape, new_files) {
  local_file <- paste0("raw_", exp_escape, ".rds")
  if (!file.exists(local_file)) return(NULL)
  
  message("Local copy found.")
  old_data <- readRDS(local_file)
  old_files <- length(unique(old_data$file))
  new_files_count <- length(unique(new_files))
  
  if (old_files == new_files_count) {
    message("No new files found: Using local copy.")
    return(old_data)
  }
  
  message("New files found: Updating local copy.")
  NULL
}

read_and_combine_files <- function(links) {
  #raw <- pbapply::pblapply(links, safely_read_csv, showProgress = FALSE)
  raw <- pbapply::pblapply(links, safely_read_csv)
  raw <- Filter(Negate(is.null), raw)
  
  if (length(raw) == 0) {
    message("No files could be read successfully.")
    return(NULL)
  }
  
  out <- data.table::rbindlist(raw, id = "file", fill = TRUE)
  clean_json_fields(out)
}

process_responses <- function(data, 
                              response_col = "response",
                              fill_direction = "downup") {
  
  # Check if there are any JSON responses
  if (!any(grepl("\\{\"", data[[response_col]]))) {
    return(data)
  }
  
  message("Splitting responses to columns...")
  tryCatch({
    splitresponse(data, 
                  response_col = response_col,
                  fill_direction = fill_direction)
  }, error = function(e) {
    message("Failed to process responses: ", e$message)
    data
  })
}

get_files <- function(page, exp, site, subdirs = TRUE, base_url = NULL, current_path = "") {
  # Get CSV files in current directory
  filenames <- rvest::html_elements(page, xpath = ".//a[contains(@href, '.csv')]") %>% 
    rvest::html_text()
  
  # If not checking subdirectories, return current files
  if (!subdirs) {
    return(filenames)
  }
  
  # Get subdirectories (excluding parent directory)
  folders <- rvest::html_elements(page, xpath = ".//a[contains(@href, '/')]") %>% 
    rvest::html_text()
  folders <- folders[-1]  # Remove "Parent directory"
  
  # Process subdirectories if any exist
  if (length(folders) > 0) {
    for (folder in folders) {
      # Construct the new path
      new_path <- if (current_path == "") {
        paste0(exp, "/", folder)
      } else {
        paste0(current_path, "/", folder)
      }
      
      # Read the subdirectory page
      sub_url <- paste0(base_url, new_path, "/")
      tryCatch({
        sub_page <- rvest::read_html(sub_url)
        # Get files from subdirectory
        sub_files <- get_files(sub_page, exp, site, TRUE, base_url, new_path)
        # Add folder prefix to filenames
        if (length(sub_files) > 0) {
          sub_files <- paste0(folder, sub_files)
          filenames <- c(filenames, sub_files)
        }
      }, error = function(e) {
        warning(sprintf("Could not access subdirectory: %s", folder))
      })
    }
  }
  
  filenames
}


#' Write responses into variables
#'
#' Write all answers from the "response" or "responses" column generated by a survey-plugin 
#' into individual variables. Groups by 'file' column, falling back to 'subject' if 'file' 
#' is not available.
#' 
#' @param data Dataframe containing response data
#' @param response_col Character. Name of the column containing JSON responses. Default "response"
#' @param fill_direction Character. Direction to fill missing values: "downup" (default), "down", "up", or "none"
#' @param keep_original Logical. Whether to keep the original response column (default: TRUE)
#' @param simplify Logical. Whether to simplify single-element arrays to vectors (default: TRUE)
#' @return Dataframe with JSON responses split into separate columns
#' @examples
#' # Basic usage
#' survey <- splitresponse(myexp)
#' 
#' # Custom configuration
#' survey <- splitresponse(myexp, 
#'                        response_col = "responses",
#'                        fill_direction = "down",
#'                        keep_original = FALSE)
#' @export
splitresponse <- function(data, 
                          response_col = "response",
                          fill_direction = "downup",
                          keep_original = TRUE,
                          simplify = TRUE) {
  
  # Input validation
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  
  if (!response_col %in% colnames(data)) {
    stop(sprintf("Column '%s' not found in data frame", response_col))
  }
  
  if (!fill_direction %in% c("downup", "down", "up", "none")) {
    stop("fill_direction must be one of: 'downup', 'down', 'up', 'none'")
  }
  
  # Add row identifier for proper joining later
  data$..row_id <- seq_len(nrow(data))
  
  # Find rows containing JSON responses
  json_rows <- grepl("^\\s*\\{.*}\\s*$", data[[response_col]])
  if (!any(json_rows)) {
    warning("No valid JSON responses found")
    return(data)
  }
  
  # Function to safely parse JSON
  safe_parse_json <- function(x) {
    if (is.na(x) || !nzchar(trimws(x))) return(NULL)
    tryCatch({
      jsonlite::fromJSON(x, simplifyVector = simplify)
    }, error = function(e) {
      warning("Failed to parse JSON: ", e$message)
      NULL
    })
  }
  
  # Parse JSON responses
  #message("Parsing JSON responses...")
  parsed_responses <- lapply(data[[response_col]][json_rows], safe_parse_json)
  
  # Remove NULL entries (failed parses)
  valid_responses <- !sapply(parsed_responses, is.null)
  if (!any(valid_responses)) {
    warning("No valid JSON could be parsed")
    return(data)
  }
  
  parsed_responses <- parsed_responses[valid_responses]
  response_rows <- which(json_rows)[valid_responses]
  
  # Convert list of responses to data frame
  #message("Converting responses to columns...")
  response_df <- tryCatch({
    response_df <- dplyr::bind_rows(parsed_responses)
    response_df$..row_id <- response_rows
    response_df
  }, error = function(e) {
    stop("Failed to convert responses to columns: ", e$message)
  })
  
  # Join with original data
  out <- dplyr::left_join(data, response_df, by = "..row_id")
  
  # Fill missing values if requested
  if (fill_direction != "none" && nrow(response_df) > 0) {
    #message("Filling missing values...")
    
    fill_cols <- setdiff(colnames(response_df), "..row_id")
    
    # Determine grouping column
    group_col <- if ("file" %in% colnames(out)) {
      #message("Grouping by file")
      "file"
    } else if ("subject" %in% colnames(out)) {
      message("File column not found, grouping by subject")
      "subject"
    } else {
      message("Neither file nor subject columns found, filling without grouping")
      NULL
    }
    
    # Apply filling with appropriate grouping
    if (!is.null(group_col)) {
      out <- out %>%
        dplyr::group_by(!!rlang::sym(group_col)) %>%
        tidyr::fill(all_of(fill_cols), .direction = fill_direction) %>%
        dplyr::ungroup()
    } else {
      out <- out %>%
        tidyr::fill(all_of(fill_cols), .direction = fill_direction)
    }
  }
  
  # Clean up
  out <- out %>%
    dplyr::select(-"..row_id")
  
  # Remove original response column if requested
  if (!keep_original) {
    out <- out %>%
      dplyr::select(-all_of(response_col))
  }
  
  # Return result
  out
}
