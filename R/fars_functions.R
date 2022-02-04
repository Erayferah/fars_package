#' Read the data
#'
#' This is a function that checks if the file exists and takes couple of
#' decisions based on. If the file does not exist, it throws a warning message.
#' If the file exists, it basically imports it as csv and than transforms it
#' into tbl data frame.
#'
#' @param filename a character string. if the file is in the current working
#'    directory, you do not have to include the path and only the file name with
#'    .csv ending would be enough.
#'
#' @return the data in tbl_df format
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @export
#'
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Create a file name
#'
#' This function creates a string, which is going to be used for a file name
#' based on the provided year parameter.
#'
#' @param year integer or string in YYYY format
#'
#' @return This function returns a string with provided year input e.g.
#'     accident_2013.csv.bz2
#'
#' @examples
#' make_filename(2015)
#'
#' @export
#'
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Read years and months from the given file
#'
#' This is a function reads accident csv files based on given year vector, and
#'    then return months and years. This function uses 2 above functions.
#'
#' @param years either scalar or vector
#'
#' @return MONTH and year from the accident data based on the availability of
#'    provided year as a parameter.
#'
#'
#' @importFrom dplyr mutate select
#'
#' @export
#'
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Summarize given accident data
#'
#' This is a function that summarize accident csv files based on given year
#' vector. This function uses fars_read_years function.
#'
#' @param years either scalar or vector
#'
#' @return a data.frame of summarized data that is converted to wider format
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Plots selected state number and year
#'
#' Takes state number and year and plots a map. Based on the existence of state
#' number, could throw message "invalid STATE number: state.num". Also if there
#' are no accidents, shows the message of "no accidents to plot". Uses
#' previously defined make_filename and fars_read functions.
#'
#' @param state.num number of state integer or string (coerses to integer)
#' @param year integer
#'
#' @return a plot with selected criteria
#'
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}

