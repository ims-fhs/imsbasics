# =============================================================================
# Independent functions:

# force_warning <- function(mytext) { #.........................................Use???
#   old <- getOption("warn")
#   options(warn = 0)
#   warning(mytext) # This message is useful! Keep it. See warn handling...
#   options(warn = old)
# }

#' decimalplaces returns the number of decimal places of x
#'
#' @param x A numeric
#'
#' @return result An integer, the number of decimalplaces of x
#'
decimalplaces <- function(x) {
  if ((x %% 1) != 0) {
    result <- nchar(strsplit(sub('0+$','', as.character(x)), ".",
      fixed = TRUE)[[1]][[2]])
    return(as.integer(result))
  } else {
    return(0L)
  }
}


#' Load data from specific folder to a variable in current environment.
#' Call as data <- imsbasics::load_rdata(filename, path)
#'
#' @param filename, can be without ".RData"
#' @param path in "../.." style. Default: getwd()
#'
#' @return Obects(s) in filename
#'
load_rdata <- function(filename,
  path = stop("Filename and path arguments are required")) {
  # Default file type:
  if (!grepl(".RData", filename)) {
    filename <- paste0(filename, ".RData")
  }

  file <- paste0(path, filename)
  if (file.exists(file)) {
    # create new clear environment which can be deleted after loading
    temp_space <- new.env()
    # load file in temp.space as name.of.object:
    name_of_object <- load(file, temp_space)
    # Assign data in name.f.objects to data:
    data <- get(name_of_object, temp_space)
    # Remove local variables to clean up environment:
    rm(temp_space, name_of_object)
  } else {
    stop(paste0("Error in load_rdata: no file: ", filename))
  }
  return(data)
}


#' Save a specific variable in environmet to file in specific folder
#' Call as cacheR::varToFile(data, filename, path)
#'
#' @param data: An object in the environment. Required input.
#' @param filename, can be without ".RData". Required input.
#' @param path in "../.." style. Default: getwd()
#'
#' @return path to file ot NULL in case file already exists.
#'
save_rdata <- function(data, filename, path, force = F) { # "inverse" of load_rdata
  # data, filename and path needed. Otherwise: error...
  if (missing(data) | missing(filename) | missing(path)) {
    stop("Data, filename and path arguments are required")
  }
  if (force == T) {
    old <- getOption("warn")
    options(warn = 0)
    warning(paste0("force = T"))
    options(warn = old)
  }

  # Default file type:
  if (!grepl(".RData", filename)) {
    filename <- paste0(filename, ".RData")
  }
  # Check that you don't overwrite a file. If file already exists:
  if (file.exists(paste0(path, filename))) {
    if (force == F) {
      old <- getOption("warn")
      options(warn = 0)
      warning("File not saved. File already exists")
      options(warn = old)
    } else {
      old <- getOption("warn")
      options(warn = 0)
      warning("Old file overwritten")
      options(warn = old)
      save(data, file = paste0(path, filename))
    }
  } else {
    # File does not exist:
    save(data, file = paste0(path, filename))
  }
  return(NULL)
}


#' Function returns blue of FH St. Gallen.
#'
#' @return RGB value for FHS-blue
fhs <- function() {
  # Args:
  #   none (Else... x: vector of data)
  #
  # Returns:
  #   RGB-color of FH St.Gallen
  return(rgb(0, 102, 153, maxColorValue = 255))
}


#' Function to remove all variables
clear_all_var <- function() { # Define function "clr()"
  # Clear the environment.
  #
  # Args:
  #   none (Else... x: vector of data)
  #
  # Returns:
  #   none
  ENV <- globalenv() # Clear variables in the global environment
  # Error handling
  # none...
  # Errors should be raised using stop().
  vars <- ls(envir = ENV)
  vars <- vars[vars != "clr"]
  # rm(list =ls()) only deletes the variables of the script ( = nothing!)
  rm(list = vars, envir = ENV)
  # TODO(username): Explicit description of action to be taken
}


#' Function to close all graphs/ plots
close_all_graph <- function() {
  if (dev.cur() != 1) {dev.off(which = dev.cur())} #close plots
  graphics.off() #close plots in win.graph()
}


#' Function to clear console
cc <- function() {
  cat("\014")
}


# =============================================================================
# Functions with dependencies:

#' Function to remove all variables and close all graphs/ plots
#' including Garbage Collection gc()
clc <- function() {
  clear_all_var()
  close_all_graph()
  gc()
}


# =============================================================================
# Not used any more:
#
#' Function without functionality to show ims style guideline based on Hadley and google
#' See workflow-and-style.Rmd for further details
#'
#' Files end with .R: meaningful-file.R
#' Variables: meaningful_variable
#' Functions: meaningful_function => Verb for toDo...
#'
#' Indentation
#' When indenting your code, use two spaces.  Never use tabs or mix tabs and spaces.
#' Spacing
#' Place spaces around all binary operators (=, +, -, <-, etc.).
#' Do not place a space before a comma, but always place one after a comma.
#' Place a space before left parenthesis, except in a function call.
#'
#' General Layout and Ordering
#' 1. Copyright statement comment
#' 2. Author comment
#' 3. File description comment, including purpose of program, inputs, and outputs
#' 4. source() and library() statements
#' 5. Function definitions
#' 6. Executed statements, if applicable (e.g., print, plot)
#'
#' Unit tests should go in a separate file named test_originalfunction(s).R.
#'
#' Install packages with install.packages("package")
#' Include non-standard libraries using libraray(package)
# googlestyle <- function() {}

