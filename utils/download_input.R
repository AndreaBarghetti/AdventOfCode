#!/usr/bin/env Rscript

# Check for httr package
if (!requireNamespace("httr", quietly = TRUE)) {
  stop("The 'httr' package is required. Please install it with install.packages('httr').")
}

library(httr)

download_input <- function(year, day, session_cookie, output_path) {
  url <- paste0("https://adventofcode.com/", year, "/day/", day, "/input")
  ua <- user_agent("github.com/andreabarghetti/AdventOfCode Input Downloader")
  
  tryCatch({
    response <- GET(url, set_cookies(session = session_cookie), ua)
    stop_for_status(response)
    writeBin(content(response, "raw"), output_path)
    cat(sprintf("Successfully downloaded input for %s Day %s to %s\n", year, day, output_path))
  }, error = function(e) {
    cat(sprintf("Error downloading input: %s\n", e$message))
    quit(status = 1)
  })
}

# Parse arguments
args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 2) {
  cat("Usage: Rscript download_input.R <year> <day> [--output <path>] [--session <cookie>]\n")
  quit(status = 1)
}

year <- args[1]
day <- args[2]
output_path <- "input.txt"
session_cookie <- NULL

i <- 3
while (i <= length(args)) {
  if (args[i] %in% c("--output", "-o") && (i + 1) <= length(args)) {
    output_path <- args[i + 1]
    i <- i + 2
  } else if (args[i] %in% c("--session", "-s") && (i + 1) <= length(args)) {
    session_cookie <- args[i + 1]
    i <- i + 2
  } else {
    i <- i + 1
  }
}

if (is.null(session_cookie)) {
  # Try to find .session file in script directory
  initial_options <- commandArgs(trailingOnly = FALSE)
  file_arg_name <- "--file="
  script_name <- sub(file_arg_name, "", initial_options[grep(file_arg_name, initial_options)])
  script_dir <- dirname(script_name)
  if (length(script_dir) == 0) script_dir <- "."
  
  session_file <- file.path(script_dir, ".session")
  if (file.exists(session_file)) {
    session_cookie <- trimws(readLines(session_file, n = 1, warn = FALSE))
  } else {
    session_cookie <- Sys.getenv("AOC_SESSION")
  }
}

if (is.null(session_cookie) || session_cookie == "") {
  stop("Error: Session cookie not found. Please provide it via --session, a .session file, or AOC_SESSION environment variable.")
}

download_input(year, day, session_cookie, output_path)