#' Function returns blue of FH St. Gallen.
#' @export
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
#' @export
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
#' @export
close_all_graph <- function() {
  if (dev.cur() != 1) {dev.off(which = dev.cur())} #close plots
  graphics.off() #close plots in win.graph()
}


#' Function to remove all variables and close all graphs/ plots
#' including Garbage Collection gc()
#' @export
clc <- function() {
  clear_all_var()
  close_all_graph()
  gc()
}


#' Function to clear console
#' @export
cc <- function() {
  cat("\014")
}


#' Function without functionality to show R style guideline from google
#' Google R styleguide, see "https://google.github.io/styleguide/Rguide.xml#generallayout":
#' Hadley R styleguide, see ......................................................
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
#' @export
googlestyle <- function() {}

