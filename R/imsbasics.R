# install.packages("...")
# --run in the old version of R
# old_wd <- getwd()
# setwd("C:/Temp/")
# packages <- installed.packages()[,"Package"]
# save(packages, file="Rpackages")
#
# Followed by this in the new version:
#
#   #--run in the new version
#   setwd("C:/Temp/")
# load("Rpackages")
# for (p in setdiff(packages, installed.packages()[,"Package"]))
#   install.packages(p)
# setwd(old_wd)

# hms <- function(dates) {
#   res <- lubridate::ymd_hms(paste(lubridate::today(), format(lubridate::ymd_hms(dates), '%T')))
#   return(res)
# }


#' percent_deviation return deviation of a number from a reference number x_ref
#' in %.
#'
#' @param x, a numeric
#' @param x_ref, a numeric
#'
#' @return absolute value of deviation in percent
#'
percent_deviation <- function(x, x_ref) {
  return(abs(x - x_ref)/x_ref*100)
}


#' %<-% Matlabs '[...] =' operator. call as c(par1, par2, ...) %<-% somefunction(...)
#' instead of calling res <- somefunction(...), par1 <- res$par1, ... rm(res)
#' Should not be used in simulations
#'
#' @param lhs
#' @param rhs
#'
#' @return result
#'
'%<-%' <- function(lhs, rhs) {
  frame <- parent.frame()
  lhs <- as.list(substitute(lhs))
  if (length(lhs) > 1)
    lhs <- lhs[-1]
  if (length(lhs) == 1) {
    do.call(`=`, list(lhs[[1]], rhs), envir = frame)
    return(invisible(NULL))
  }
  if (is.function(rhs) || is(rhs, 'formula'))
    rhs <- list(rhs)
  if (length(lhs) > length(rhs))
    rhs <- c(rhs, rep(list(NULL), length(lhs) - length(rhs)))
  for (i in 1:length(lhs))
    do.call(`=`, list(lhs[[i]], rhs[[i]]), envir = frame)
  return(invisible(NULL))
}


#' create_log directs output to logfile_name in save_path
#'
#' @param logfile_name
#' @param save_path
#'
create_log <- function(logfile_name, save_path) {
  warning("Error messages not visible. Use closeAllConnections() in the end of the script")
  if (file.exists(paste0(save_path, logfile_name))) {
    file.remove(paste0(save_path, logfile_name))
  }
  fid <- file(paste0(save_path, logfile_name), open = "wt")
  sink(fid, type = "message", split = F) # warnings are NOT displayed. split=T not possible.
  sink(fid, append = T, type = "output", split = T) # print, cat
  return(NULL)
}


#' set_custom_rstudio sets custom parameters
#'
#' @param warn = 0
#'
#' @return NULL
#'
r_options <- function(error = NULL, warn = 0, strings_as_factors = F, english = T) {
  options(error = error)
  options(warn = warn) # 0 on / -1 off / 2 warn2err
  options(stringsAsFactors = strings_as_factors)
  if (english) {
    Sys.setenv(LANG = "en")
    Sys.setlocale(category = "LC_ALL", locale = "English_United States.1252")
  }
  return(NULL)
}


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
#' @param path in "../.." style ending with "/".
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
    name_of_object <- load(file)
    # Assign data in name.f.objects to data:
    data <- get(name_of_object)
  } else {
    stop(paste0("no file: ", file))
  }
  return(data)
}


#' Save a specific variable in environmet to file in specific folder
#' Call as cacheR::varToFile(data, filename, path)
#'
#' @param data: An object in the environment. Required input.
#' @param filename, can be without ".RData". Required input.
#' @param path in "../.." style ending with "/".
#'
#' @return NULL
#'
save_rdata <- function(data, filename,
  path = stop("Data, filename and path arguments are required"), force = F, warn = T) {
  if (missing(data) | missing(filename) | missing(path)) {
    stop("Data, filename and path arguments are required")
  }
  # Default file type:
  if (!grepl(".RData", filename)) {
    filename <- paste0(filename, ".RData")
  }
  file <- paste0(path, filename)
  # Check that you don't overwrite a file. If file already exists:
  if (file.exists(file)) {
    if (force == F) {
      if (warn == T) {
        warning(file, " not saved. File already exists")
      }
    } else {
      if (warn == T) {
        warning("Old file overwritten")
      }
      save(data, file = file)
    }
  } else {
    # File does not exist:
    save(data, file = file)
  }
  return(NULL)
}


#' Function returns blue of FH St. Gallen.
#'
#' @return RGB value for FHS-blue
#'
fhs <- function() {
  return(rgb(0, 102, 153, maxColorValue = 255))
}


#' Function to remove all variables
#'
clear_all_var <- function() {
  ENV <- globalenv()
  vars <- ls(envir = ENV)
  vars <- vars[vars != "clr"]
  rm(list = vars, envir = ENV)
  return(NULL)
}


#' Function to close all graphs/ plots
#'
close_all_graph <- function() {
  if (dev.cur() != 1) {dev.off(which = dev.cur())} #close plots
  graphics.off() #close plots in win.graph()
  return(NULL)
}


#' Function to clear console
#'
cc <- function() {
  cat("\014")
  return(NULL)
}


#' zero_n creates string of form 000x from integer x.
#'
#' @param number
#' @param position
#'
#' @return standard string
#' @export
#'
zero_n <- function(number, position=3) {
  res <- substr(number + 10^position, 2, nchar(number + 10^position))
  return(res)
}


#' Copy and rename one file. Used in archive_data
#'
#' @param path, a string: ending with "/"
#' @param files, a list of characters: filenames or "all"
#' @param prefix, a character added to the target filename
#'
copy_rename_file <- function(path, file, save_path, prefix="") {
  n_suffix = 0
  if (file.exists(paste0(path, file))) {
    while (n_suffix < 999) {
      file_sep <- unlist(strsplit(file, ".", fixed = T))
      if (file_sep[2] == "RData") {
        data <- imsbasics::load_rdata(file, path)
        var <- data$uuid
        if (!length(var) == 0) {
          # uuid exists
          if (n_suffix == 0) {
            suffix <- substr(var, 1, 8)
          } else {
            # file already exists, use _HSASH_00x format
            suffix <- paste0(substr(var, 1, 8), "_", zero_n(n_suffix, 3))
          }
        } else {
          # uuid does not exist, use _00x format
          suffix <- zero_n(n_suffix, 3)
        }
      } else {
        # For non-.RData files always use _00x format
        suffix <- zero_n(n_suffix, 3)
      }
      save_name <- paste0(save_path, prefix, "-", file_sep[1], "_", suffix, ".", file_sep[2])
      if (!file.exists(save_name)) {
        file.copy(paste0(path, file), paste0(save_path, file))
        file.rename(paste0(save_path, file), save_name)
        n_suffix <- 999 # Terminate while-loop
      } else {
        warning(save_name, " already exists.")
        n_suffix <- n_suffix + 1
      }
    }
  } else {
    warning(paste0(path, file), " does not exist.")
  }
}


#' midpoints calculates the midpoints of a factor level (x,y] created from ..... ?
#'
#' @param x
#' @param dp
#'
#' @return mids
#'
midpoints <- function(x, dp=2){
  lower <- as.numeric(gsub(",.*","",gsub("\\(|\\[|\\)|\\]","", x)))
  upper <- as.numeric(gsub(".*,","",gsub("\\(|\\[|\\)|\\]","", x)))
  return(round(lower + (upper - lower)/2, dp))
}


#' weekdays_abbr dictionary according to lubridate
#'
#' @return german and english weekdays
#'
weekdays_abbr <- function() {
  german <- c("So", "Mo", "Di", "Mi", "Do", "Fr", "Sa")
  # names from lubridate.
  english <- c("Sun", "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat")
  return(list(german = german, english = english))
}


#' g2e german to english using weekdays_abbr as reference.
#'
#' @param weekday_ger
#'
#' @return res
#'
g2e <- function(weekday_ger) {
  # assertthat(ger!)
  dict <- weekdays_abbr() # if (!exists("dict")) {dict <- ...}
  res <- character(length(weekday_ger))
  for (i in 1:length(weekday_ger)) {
#     split <- strsplit(weekday_ger[i], split = ", ")
#     if split == weekday_ger[i] {
    res[i] <- dict$english[which(dict$german == weekday_ger[i])]
    # } else {for (k in 1:length(split)) {res[i]...} paste(res, collapse=", ")}
  }
  return(res)
}


#' e2g english to german using weekdays_abbr as reference.
#'
#' @param weekday_eng
#'
#' @return res
#'
e2g <- function(weekday_eng) {
  dict <- weekdays_abbr()
  res <- character(length(weekday_eng))
  for (i in 1:length(weekday_eng)) {
    res[i] <- dict$german[which(dict$english == weekday_eng[i])]
  }
  return(res)
}


#' rectangle step to zero in (t0, t1). Otherwise 0.
#'
#' @param t
#' @param t0
#' @param t1
#'
#' @return
#'
rectangle <- function(t, lower, upper, at_step=0.5) { # Not used.
  if (upper > lower) {
    y <- fBasics::Heaviside(t,lower) * fBasics::Heaviside(-t,-upper)
  } else {
    stop("upper > lower")
  }
  if (at_step == 0.5) {
    return(y)
  } else if (at_step == 0) {
    return(floor(y))
  } else if(at_step == 1) {
    return(ceiling(y))
  } else {
    stop("at_step = 0, 0.5 or 1")
  }
}


# =============================================================================
# Functions with dependencies:

#' Function to remove all variables and close all graphs/ plots
#' including Garbage Collection gc()
#'
clc <- function() {
  clear_all_var()
  close_all_graph()
  gc()
  return(NULL)
}


#' Archive data with date and version tag.
#'
#' @param path, a string: ending with "/". The source directory.
#' @param files, a list of characters: filenames or "all"
#' @param save_path, a string: ending with "/". The target directory.
#'
archive_data <- function(path, files, save_path) {
  t0 <- lubridate::now()
  prefix <- paste(lubridate::year(t0), lubridate::month(t0), lubridate::day(t0), sep = "-")
  if (files[1] == "all") {
    if (path == "") {
      path <- getwd()
    }
    lf <- list.files(path, full.names = T)
    ld <- list.dirs(path, recursive = F)
    file_list <- lf[match(setdiff(normalizePath(lf), normalizePath(ld)), normalizePath(lf))]
    for (i in 1:length(file_list)) {
      copy_rename_file(paste0(dirname(file_list[i]), "/"),
        basename(file_list[i]), save_path, prefix)
    }
    # could be extended to subfolders via list.files(path, full.names = T, recursive = T)
  } else {
    for (i in 1:length(files)) {
      # save i in folder to folder with date and tag if !file.exists(i)
      copy_rename_file(path, files[i], save_path, prefix)
    }
  }
  return(NULL)
}


load_fom_archive <- function(short_uuid, path2archive) {
  lf <- list.files(path2archive, full.names = T)
  ld <- list.dirs(path2archive, recursive = F)
  file_list <- lf[match(setdiff(normalizePath(lf), normalizePath(ld)), normalizePath(lf))]
  cond <- grepl(short_uuid, strsplit(file_list, split = "_", fixed = T))
  if (sum(cond) != 1) {
    stop("short_uuid not unique")
    # Could be extended: grepl(substr(short_uuid, 1, 8)...)
  }
  file <- unlist(strsplit(file_list[cond], split = "/", fixed = T))
  if (grepl(".RData", file[length(file)]))
    return(load_rdata(file[length(file)], path2archive))
  else {
    stop("File not found")
    return(NULL)
  }
}

