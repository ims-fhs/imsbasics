#' install_simiverse: Installs all the packages for the sim911-universe.
#' Uses devtools::install_github, accesses always master branches.
#' No examples and tests provided since they will change your library..
#'
#' @param auth_token The token to access your privat Repos on GitHub
#'
#' @return df_package_status A data.frame with package names and installation status
#' @export
install_simiverse <- function(auth_token = stop(
  "Provide GitHub AuthToken to access private Repos")) {

  res_data911 <- devtools::install_github("ims-fhs/data911", auth_token = auth_token)
  needed_ims_packages <- data911::needed_ims_packages()
  assertthat::are_equal(needed_ims_packages[1], "data911") # otherwise the return shows nonsense ... SCN
  installed <- devtools::install_github(paste0("ims-fhs/", needed_ims_packages[
    needed_ims_packages != "data911"]), auth_token = auth_token)
  installed <- c(res_data911, installed)

  assertthat::noNA(installed)

  return(data.frame(
    package = needed_ims_packages,
    installed = installed
  ))
}

#' pre_commit: uses devtools to document package, run all examples and
#' run all testthat tests and if use_r_cmd_check = T even run devtools::check.
#'Use this before every commit.
#'
#' R CMD check() can be set inactive (use_r_cmd_check = F), because it may
#' throw errors even if devtools::run_examples() and devtools::test()
#' run without errors. Not fully understood by SCN.
#'
#' @param use_r_cmd_check A boolean if TRUE, devtools::check() will be used
#'
#' @return NULL
#' @export
pre_commit <- function(use_r_cmd_check = T) {
  devtools::document()
  devtools::run_examples()
  devtools::test()
  if (use_r_cmd_check) {
    devtools::check()
  }
  return(NULL)
}


#' Plot runtime graph on a double log scale
#'
#' @param n iterations
#' @param dt runtime
#' @param dt_unit A character, the unit of runtime
#' @param title  A character, the title of the graph
#' @param display A bool, display fit if T
#'
#' @export
#'
#' @examples
#' n <- 10^c(0:5)
#' dt <- 300*n+50 # in us
#' plot_runtime(n, dt, "us", "My function", T)
plot_runtime <- function(n, dt, dt_unit, title, display) {
  x_min <- floor(log10(min(n)))
  y_min <- floor(log10(min(dt)))
  x_max <- ceiling(log10(max(n)))
  y_max <- ceiling(log10(max(dt)))

  plot(n, dt, col = "black", xlim = 10^c(x_min, x_max), ylim = 10^c(y_min, y_max),
       xlab = "n", ylab = paste0("dt [", dt_unit, "]"),
       log = "xy", xaxt = "n", yaxt = "n", main = title)
  if (display) {
    abline(lm(dt ~ n), col = "blue", untf = T)
  }

  # Labels...
  at.y <- outer(1:9, 10^(y_min:y_max))
  lab.y <- ifelse(log10(at.y) %% 1 == 0,
                  sapply(at.y, function(i)
                    as.expression(bquote(10^.(log10(i))))
                  ), NA)
  axis(2, at = at.y, labels = lab.y, las = 1)

  at.x <- outer(1:9, 10^(x_min:x_max))
  lab.x <- ifelse(log10(at.x) %% 1 == 0,
                  sapply(at.x, function(i)
                    as.expression(bquote(10^.(log10(i))))
                  ), NA)
  axis(1, at = at.x, labels = lab.x, las = 1)
  grid (NULL,NULL, lty = 6, col = "cornsilk2")
  return(NULL)
}

is.installed <- function(mypkg) {
  return(is.element(mypkg, installed.packages()[,1]))
}

#' Return deviation of a number from a reference number x_ref in percent.
#'
#' @param x, a numeric
#' @param x_ref, a numeric
#'
#' @return absolute value of deviation in percent
#' @export
percent_deviation <- function(x, x_ref, digits=1) {
  return(round(abs(x - x_ref)/x_ref*100, digits))
}

#' shift array by n positions to the right or left
#'
#' @param x An array
#' @param n An integer, the number of shifts
#' @param default NA or any other type. which should be placed where i < n.
#'
#' @return An array
#' @export
shift_array <- function(x, n, default = NA) {
  stopifnot(length(x) >= n)
  if (n == 0) {
    return(x)
  }
  if (n < 0) {
    n <- abs(n)
    forward <- F
  } else {
    forward <- T
  }
  if (forward) {
    return(c(rep(default, n), x[seq_len(length(x)-n)]))
  }
  if (!forward) {
    return(c(x[seq_len(length(x) - n) + n], rep(default, n)))
  }
}

#' %<-% Matlabs '[...] =' operator. call as c(par1, par2, ...) %<-% somefunction(...)
#' instead of calling res <- somefunction(...), par1 <- res$par1, ... rm(res)
#' Should not be used in simulations
#'
#' @param lhs
#' @param rhs
#'
#' @return result
#' @export
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
#' @export
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
#' @export
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
#' @export
#' @examples decimalplaces(12.234)
decimalplaces <- function(x) {
  assertthat::assert_that(is.numeric(x))
  if ((x %% 1) != 0) {
    result <- nchar(strsplit(sub('0+$','', as.character(x)), ".",
      fixed = TRUE)[[1]][[2]])
    return(as.integer(result))
  } else {
    return(0L)
  }
  assertthat::assert_that(is.integer(x))
}


#' Load data from specific folder to a variable in current environment.
#' Call as data <- imsbasics::load_rdata(filename, path)
#'
#' @param filename, can be without ".RData"
#' @param path in "../.." style ending with "/".
#'
#' @return Obects(s) in filename
#' @export
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
#' @export
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
      assign(deparse(substitute(data)), data)
      save(list = deparse(substitute(data)), file = file)
    }
  } else {
    # File does not exist:
    assign(deparse(substitute(data)), data)
    save(list = deparse(substitute(data)), file = file)
  }
  return(NULL)
}


#' Function returns blue of FH St. Gallen.
#'
#' @return RGB value for FHS-blue
#' @export
fhs <- function() {
  return(rgb(0, 102, 153, maxColorValue = 255))
}


#' Title
#'
#' @return
#' @export
clear_all_var <- function() {
  ENV <- globalenv()
  vars <- ls(envir = ENV)
  vars <- vars[vars != "clr"]
  rm(list = vars, envir = ENV)
  return(NULL)
}



#' Title
#'
#' @return
#' @export
close_all_graph <- function() {
  if (dev.cur() != 1) {dev.off(which = dev.cur())} #close plots
  graphics.off() #close plots in win.graph()
  return(NULL)
}



#' Title
#'
#' @return
#' @export
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
zero_n <- function(number, position=3) {
  res <- substr(number + 10^position, 2, nchar(number + 10^position))
  return(res)
}


#' Copy and rename one file. Used in archive_data
#'
#' @param path, a string: ending with "/"
#' @param files, a list of characters: filenames or "all"
#' @param prefix, a character added to the target filename
#' @export
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
#' @export
midpoints <- function(x, dp=2){
  lower <- as.numeric(gsub(",.*","",gsub("\\(|\\[|\\)|\\]","", x)))
  upper <- as.numeric(gsub(".*,","",gsub("\\(|\\[|\\)|\\]","", x)))
  return(round(lower + (upper - lower)/2, dp))
}


#' weekdays_abbr dictionary according to lubridate
#'
#' @return german and english weekdays
#' @export
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
#' @export
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
#' @export
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
#' @export
rectangle <- function(t, lower, upper, at_step=0.5) { # Not used.
  if (upper > lower) {
    y <- fBasics::Heaviside(t,lower) * fBasics::Heaviside(-t,-upper)
  } else if (upper == lower) {
    y <- rep(0, length(t))
  } else {
    browser()
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
#' @return
#' @export
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
#' @param exclude_file_type, an array, the type to be excluded. Either "none" or e.g. c(".pdf", ".html")
#'
#' @return NULL
#' @export
#'
#' @examples
#' imsbasics::archive_data("C:/Temp/Test/", "all", "C:/Temp/Archiv/", c(".html", ".pdf"))
#' imsbasics::archive_data("C:/Temp/Test/", "all", "C:/Temp/Archiv/")
archive_data <- function(path, files, save_path, exclude_file_type = "none") {
  t0 <- lubridate::now()
  prefix <- paste(lubridate::year(t0), lubridate::month(t0), lubridate::day(t0), sep = "-")
  if (files[1] == "all") {
    if (path == "") {
      path <- getwd()
    }
    lf <- list.files(path, full.names = T)
    ld <- list.dirs(path, recursive = F)
    file_list <- lf[match(setdiff(normalizePath(lf), normalizePath(ld)), normalizePath(lf))]
    if (exclude_file_type[1] != "none") {
      keep_files_df <- t(as.data.frame(lapply(exclude_file_type, function(x)
        !grepl(x, basename(file_list))), stringsAsFactors = F))
      colnames(keep_files_df) <- paste0("V", c(1:ncol(keep_files_df)))
      rownames(keep_files_df) <- NULL
      file_list <- file_list[unlist(lapply(c(1:ncol(keep_files_df)), function(x) all(keep_files_df[, x])))]
    }
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


#' Title
#'
#' @param short_uuid
#' @param path2archive
#'
#' @return
#' @export
load_fom_archive <- function(short_uuid, path2archive) {
  lf <- list.files(path2archive, full.names = T)
  ld <- list.dirs(path2archive, recursive = F)
  file_list <- lf[match(setdiff(normalizePath(lf), normalizePath(ld)), normalizePath(lf))]
  cond <- grepl(short_uuid, strsplit(file_list, split = "_", fixed = T))
  if (sum(cond) > 1) {
    cat(paste0("The following files do not have a unique short uuid:\n",
               paste(basename(file_list[cond]), collapse = ",\n ")))
    stop("short_uuid not unique")
    # Could be extended: grepl(substr(short_uuid, 1, 8)...)
  } else if (sum(cond) == 0) {
    stop("short_uuid not found")
  }
  file <- unlist(strsplit(file_list[cond], split = "/", fixed = T))
  if (grepl(".RData", file[length(file)]))
    return(load_rdata(file[length(file)], path2archive))
  else {
    stop("File not found")
    return(NULL)
  }
}

#' Define a German <-> English dictionary for weekday and month names
#'
#' @return A dictionary as list (subset possible)
#' @export
dict <- function() { # Not used anymore
  dict_g2e <- setNames(
    c("Sun", "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Jan", "Feb", "Mar",
      "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
    c("So", "Mo", "Di", "Mi", "Do", "Fr", "Sa", "Jan", "Feb", "Mar",
      "Apr", "Mai", "Jun", "Jul", "Aug", "Sep", "Okt", "Nov", "Dez"))

  dict_e2g <- setNames(
    c("So", "Mo", "Di", "Mi", "Do", "Fr", "Sa", "Jan", "Feb", "Mar", "Apr",
      "Mai", "Jun", "Jul", "Aug", "Sep", "Okt", "Nov", "Dez"),
    c("Sun", "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Jan", "Feb", "Mar", "Apr",
      "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
  return(list(g2e = dict_g2e, e2g = dict_e2g))
}

#' Define a Integer-alias <-> English dictionary for weekday and month names
#'
#' @return A dictionary as list (subset possible)
#' @export
int_dict <- function() { # This is used for Jan -> integer conversion
  dict_int2e  <- setNames(
    c("Sun", "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Jan", "Feb", "Mar",
      "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
    c("07", "01", "02", "03", "04", "05", "06", "01", "02", "03",
      "04", "05", "06", "07", "08", "09", "10", "11", "12"))

  dict_e2int <- setNames(
    c("07", "01", "02", "03", "04", "05", "06", "01", "02", "03",
      "04", "05", "06", "07", "08", "09", "10", "11", "12"),
    c("Sun", "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Jan", "Feb", "Mar",
      "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
  return(list(e2int = dict_e2int, int2e = dict_int2e))
}

#' German => English translator based on dict()
#'
#' @param ger_expr A character (in german)
#'
#' @return A character, the translation
#' @export
g2e <- function(ger_expr) {
  if (!exists("dict_g2e")) {
    # dictg_g2e <- dict()$g2e
    assign("dict_g2e", dict()$g2e, envir = .GlobalEnv)
  }
  res <- character(length(ger_expr))
  for (i in 1:length(ger_expr)) {
    split <- unlist(strsplit(ger_expr[i], split = ", "))
    assertthat::assert_that(all(split %in% names(dict_g2e)))
    if (length(split) == length(ger_expr[i])) {
      res[i] <- dict_g2e[split]
    } else {
      res[i] <- paste(dict_g2e[split], collapse = ", ")
    }
  }
  return(res)
}

#' English => German translator based on dict()
#'
#' @param eng_expr A character (in english)
#'
#' @return A character, the translation
#' @export
e2g <- function(eng_expr) {
  if (!exists("dict_e2g")) {
    assign("dict_e2g", dict()$e2g, envir = .GlobalEnv)
  }
  res <- character(length(eng_expr))
  for (i in 1:length(eng_expr)) {
    split <- unlist(strsplit(eng_expr[i], split = ", "))
    assertthat::assert_that(all(split %in% names(dict_e2g)))
    if (length(split) == length(eng_expr[i])) {
      res[i] <- dict_e2g[split]
    } else {
      res[i] <- paste(dict_e2g[split], collapse = ", ")
    }
  }
  return(res)
}

