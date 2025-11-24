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
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select everything              
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
    return(NULL)
  }
  
  # URL encode credentials
  uid_encoded <- utils::URLencode(credentials$uid, reserved = TRUE)
  pwd_encoded <- utils::URLencode(credentials$pwd, reserved = TRUE)
  
  # Construct base URL
  base_url <- paste0("https://", uid_encoded, ":", pwd_encoded, "@", domain, "/data/")
  url <- paste0(base_url, exp, "/")
  
  # Check if experiment exists by testing HTTP status
  status_check <- tryCatch({
    httr::HEAD(url)
  }, error = function(e) {
    NULL
  })
  
  if (is.null(status_check)) {
    message("Could not connect to server. Please check your internet connection.")
    return(NULL)
  }
  
  if (httr::status_code(status_check) == 404) {
    message("Cannot find experiment '", exp, "'. Please check the experiment name.")
    return(NULL)
  }
  
  if (httr::status_code(status_check) != 200) {
    message("Server error (HTTP ", httr::status_code(status_check), "). Please try again later.")
    return(NULL)
  }
  
  # Try to read the page
  page <- tryCatch({
    rvest::read_html(url)
  }, error = function(e) {
    message("Could not read experiment page: ", e$message)
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
  local_result <- NULL
  if (localcopy) {
    local_result <- process_local_copy(exp_escape, filenames)
    
    if (!is.null(local_result) && !local_result$download_all && is.null(local_result$new_files)) {
      # No new files to download, just return the existing data
      raw_data <- local_result$data
      # Make sure "file" is the first column
      raw_data <- raw_data %>% dplyr::select(file, everything())
      # Process responses if requested and there's data
      if (nrow(raw_data) > 0 && splitresponse) {
        if (response_col %in% colnames(raw_data)) {
          raw_data <- process_responses(raw_data, 
                                        response_col = response_col,
                                        fill_direction = fill_direction)
          # Ensure "file" is first column after processing responses
          raw_data <- raw_data %>% dplyr::select(file, everything())
        } else {
          message("Response column '", response_col, "' not found in data. Skipping response processing.")
        }
      }
      return(raw_data)
    }
  } else {
    # If localcopy is disabled, download all files
    local_result <- list(download_all = TRUE)
  }
  
  # Determine which files to download
  if (local_result$download_all) {
    # Download all files
    files_to_download <- filenames
    message("Reading all files from server...")
  } else {
    # Download only new files
    files_to_download <- local_result$new_files
    message(sprintf("Reading %d new files from server...", length(files_to_download)))
  }
  
  # Create links only for files that need to be downloaded
  links <- paste0(url, files_to_download)
  links <- gsub(" ", "%20", links)
  
  # Associate filenames with links for tracking
  names(links) <- files_to_download
  
  # Read new files
  new_data <- NULL
  if (length(links) > 0) {
    new_data <- read_and_combine_files(links)
    if (is.null(new_data)) new_data <- data.frame()
  } else {
    new_data <- data.frame()
  }
  
  # Combine old and new data if needed
  if (!local_result$download_all && !is.null(local_result$data) && nrow(new_data) > 0) {
    # Ensure the columns match before binding
    all_cols <- unique(c(colnames(local_result$data), colnames(new_data)))
    for (col in all_cols) {
      if (!col %in% colnames(local_result$data)) {
        local_result$data[[col]] <- NA
      }
      if (!col %in% colnames(new_data)) {
        new_data[[col]] <- NA
      }
    }
    
    raw_data <- data.table::rbindlist(list(local_result$data, new_data), fill = TRUE)
    #message("Combined existing data with new files.")
  } else if (nrow(new_data) > 0) {
    raw_data <- new_data
  } else if (!local_result$download_all && !is.null(local_result$data)) {
    raw_data <- local_result$data
  } else {
    raw_data <- data.frame()
  }

  # "file" should be first column
  raw_data <- raw_data %>% dplyr::select(file, everything())
  
  # Save local copy if enabled
  if (localcopy && nrow(raw_data) > 0) {
    saveRDS(raw_data, file = paste0("raw_", exp_escape, ".rds"))
    #message("Local copy saved.")
  }
  
  # Process responses if requested and there's data
  if (nrow(raw_data) > 0 && splitresponse) {
    if (response_col %in% colnames(raw_data)) {
      raw_data <- process_responses(raw_data, 
                                    response_col = response_col,
                                    fill_direction = fill_direction)
      # Ensure "file" is first column after processing responses
      raw_data <- raw_data %>% dplyr::select(file, everything())
    } else {
      message("Response column '", response_col, "' not found in data. Skipping response processing.")
    }
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
  
  # Download config with proper authentication
  tryCatch({
    cred_string <- paste0(user, ":", pw)
    
    download.file("https://qpsy.de/data/config.yml", "config.yml",
                  method = "libcurl", 
                  headers = c("Authorization" = paste("Basic", 
                                                      base64enc::base64encode(charToRaw(cred_string)))),
                  quiet = TRUE)
    
    config::get("qpsy")
  }, error = function(e) {
    message("Failed to authenticate. Please check your credentials.")
    NULL
  })
}

safely_read_csv <- function(path) {
  tryCatch({
    suppressWarnings(data.table::fread(path, showProgress = FALSE))
  }, error = function(e) {
    # If direct fread fails and it's a URL, try download with proper auth
    if (grepl("^https?://", path)) {
      tryCatch({
        temp_file <- tempfile(fileext = ".csv")
        on.exit(unlink(temp_file))
        
        # Extract credentials and create proper headers
        if (grepl("://[^/]*:.*@", path)) {
          # Parse credentials from URL
          cred_match <- regmatches(path, regexpr("://\\K[^/]*:.*?(?=@)", path, perl = TRUE))
          clean_url <- gsub("://[^/]*:.*?@", "://", path)
          
          download.file(clean_url, temp_file, 
                        method = "libcurl", 
                        headers=c("Authorization"=paste("Basic", 
                                                        base64enc::base64encode(charToRaw(cred_match)))),
                        quiet = TRUE)
        } else {
          # No credentials, just use libcurl
          download.file(path, temp_file, method = "libcurl", quiet = TRUE)
        }
        
        data.table::fread(temp_file, showProgress = FALSE)
      }, error = function(e2) {
        message(sprintf("Failed to read file: %s", path))
        NULL
      })
    } else {
      message(sprintf("Failed to read file: %s", path))
      NULL
    }
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

process_local_copy <- function(exp_escape, new_filenames) {
  local_file <- paste0("raw_", exp_escape, ".rds")
  if (!file.exists(local_file)) return(list(download_all = TRUE))
  
  #message("Local copy found.")
  old_data <- tryCatch({
    readRDS(local_file)
  }, error = function(e) {
    message("Error reading local copy: ", e$message)
    return(NULL)
  })
  
  if (is.null(old_data)) {
    message("Local copy is invalid. Re-downloading all files.")
    return(list(download_all = TRUE))
  }
  
  # Check if filename column exists or is old integer file system
  if (!"file" %in% colnames(old_data) || is.integer(old_data$file)) {
    message("Local copy doesn't have filename tracking. Re-downloading all files.")
    return(list(download_all = TRUE))
  }
  
  # Get list of unique filenames from the existing data
  old_filenames <- unique(old_data$file)
  
  # Find which files are new and which have been removed
  files_to_download <- setdiff(new_filenames, old_filenames)
  files_to_remove <- setdiff(old_filenames, new_filenames)
  
  # If we have files to remove, we need to update the local data
  if (length(files_to_remove) > 0) {
    message(sprintf("Found %d files that have been removed from server. Updating local copy.", 
                    length(files_to_remove)))
    # Filter out the rows with filenames that need to be removed
    old_data <- old_data[!old_data$file %in% files_to_remove, ]
  }
  
  # Case 1: No changes at all
  if (length(files_to_download) == 0 && length(files_to_remove) == 0) {
    message("No changes detected: Using local copy.")
    return(list(download_all = FALSE, data = old_data, new_files = NULL))
  }
  
  # Case 2: Only files to remove, no new files to download
  if (length(files_to_download) == 0 && length(files_to_remove) > 0) {
    message("Files have been removed. Using updated local copy.")
    return(list(download_all = FALSE, data = old_data, new_files = NULL))
  }
  
  # Case 3: New files to download (with or without removals)
  message(sprintf("Found %d new files: Updating local copy.", length(files_to_download)))
  return(list(download_all = FALSE, data = old_data, new_files = files_to_download))
}

read_and_combine_files <- function(links) {
  # Create a function that reads a file and adds the filename
  read_with_filename <- function(i) {
    link <- links[i]
    filename <- names(links)[i]
    df <- safely_read_csv(link)
    if (!is.null(df)) {
      # Add the filename column
      df$file <- filename
    }
    return(df)
  }
  
  # Use pbapply to show progress
  raw <- pbapply::pblapply(seq_along(links), read_with_filename)
  
  # Filter out NULL results
  raw <- Filter(Negate(is.null), raw)
  
  if (length(raw) == 0) {
    message("No files could be read successfully.")
    return(NULL)
  }
  
  out <- data.table::rbindlist(raw, fill = TRUE)
  clean_json_fields(out)
}

process_responses <- function(data, 
                              response_col = "response",
                              fill_direction = "downup") {
  
  # Check if there are any JSON responses
  if (!response_col %in% colnames(data) || 
      nrow(data) == 0 || 
      !any(grepl("\\{\"", data[[response_col]], fixed = FALSE))) {
    return(data)
  }
  
  #message("Splitting responses to columns...")
  tryCatch({
    # Make sure tidyr and dplyr are loaded for the splitresponse function
    if (!requireNamespace("tidyr", quietly = TRUE) || 
        !requireNamespace("dplyr", quietly = TRUE) ||
        !requireNamespace("rlang", quietly = TRUE)) {
      message("Required packages (tidyr, dplyr, rlang) not available. Skipping response processing.")
      return(data)
    }
    
    # Call the splitresponse function
    result <- splitresponse(data, 
                            response_col = response_col,
                            fill_direction = fill_direction)
    return(result)
  }, error = function(e) {
    message("Failed to process responses: ", e$message)
    # Return the original data if processing fails
    return(data)
  })
}

#' @importFrom rvest html_elements html_text
#' @importFrom magrittr %>%
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
#'                        
#' @importFrom magrittr %>%
#' @importFrom dplyr bind_rows left_join group_by ungroup select
#' @importFrom tidyr fill all_of
#' @importFrom rlang sym !!
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
  parsed_responses <- lapply(data[[response_col]][json_rows], safe_parse_json)
  
  # Remove NULL entries (failed parses)
  valid_responses <- !sapply(parsed_responses, is.null)
  if (!any(valid_responses)) {
    warning("No valid JSON could be parsed")
    return(data)
  }
  
  parsed_responses <- parsed_responses[valid_responses]
  response_rows <- which(json_rows)[valid_responses]
  
  # Function to flatten nested structures
  flatten_response <- function(resp, prefix = "") {
    if (is.null(resp)) return(list())
    
    result <- list()
    
    if (is.list(resp) && !is.data.frame(resp)) {
      for (name in names(resp)) {
        current_prefix <- if (prefix == "") name else paste0(prefix, "_", name)
        
        if (is.list(resp[[name]]) && !is.data.frame(resp[[name]])) {
          # Recursively flatten nested lists
          nested <- flatten_response(resp[[name]], current_prefix)
          result <- c(result, nested)
        } else {
          # Add the value directly
          result[[current_prefix]] <- resp[[name]]
        }
      }
    } else {
      # If it's not a list or is a data frame, return as is
      result[[if (prefix == "") "value" else prefix]] <- resp
    }
    
    return(result)
  }
  
  # Convert list of responses to data frame
  response_df <- tryCatch({
    # Flatten all responses first
    flattened_responses <- lapply(parsed_responses, flatten_response)
    
    # Convert each flattened response to a single-row data frame
    response_dfs <- lapply(seq_along(flattened_responses), function(i) {
      flat_resp <- flattened_responses[[i]]
      if (length(flat_resp) == 0) return(NULL)
      
      # Convert to data frame
      df <- as.data.frame(flat_resp, stringsAsFactors = FALSE)
      df$..row_id <- response_rows[i]
      return(df)
    })
    
    # Remove NULL entries
    response_dfs <- response_dfs[!sapply(response_dfs, is.null)]
    
    if (length(response_dfs) == 0) {
      stop("No valid responses to process")
    }
    
    # Bind all rows together
    dplyr::bind_rows(response_dfs)
  }, error = function(e) {
    stop("Failed to convert responses to columns: ", e$message)
  })
  
  # Store original column names from response_df (excluding row_id)
  response_cols_original <- setdiff(colnames(response_df), "..row_id")
  
  # Join with original data
  out <- dplyr::left_join(data, response_df, by = "..row_id", suffix = c("", ".response"))
  
  # Identify which columns were actually added from response_df
  # (may have .response suffix if there were conflicts)
  all_new_cols <- setdiff(colnames(out), colnames(data))
  fill_cols <- all_new_cols
  
  # Fill missing values if requested
  if (fill_direction != "none" && length(fill_cols) > 0) {
    
    # Determine grouping column
    group_col <- if ("file" %in% colnames(out)) {
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
        tidyr::fill(dplyr::all_of(fill_cols), .direction = fill_direction) %>%
        dplyr::ungroup()
    } else {
      out <- out %>%
        tidyr::fill(dplyr::all_of(fill_cols), .direction = fill_direction)
    }
  }
  
  # Clean up
  out <- out %>%
    dplyr::select(-"..row_id")
  
  # Remove original response column if requested
  if (!keep_original) {
    out <- out %>%
      dplyr::select(-dplyr::all_of(response_col))
  }
  
  # Return result
  out
}
