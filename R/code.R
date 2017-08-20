
#'Reads data and output a dataframe
#'
#'@description The function \code{fars_read} takes as argument a given file (assuming it is in the working directory)
#'and check if exist. If it cannot be located, then the function retrieves the message
#'that the file does not exist. Otherwise, reads it through the external package \code{readr}
#'assigning it the name data, not displaying the loading progress (progress false) or
#'any other message (suppressMessages).
#'
#'@return And then, it is converted as a dataframe (final output) through the function
#'
#'@param filename It is the input of the function, consisting of a string
#'
#'@import magrittr
#'
#'@importFrom "dplyr" "tbl_df"
#'
#'@importFrom "readr" "read_csv"
#'
#'@examples \dontrun{fars_read()}
#'
#'@export



fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#'Creates a file name
#'
#'@description The function \code{make_filename} takes as argument a number
#'(it should be a number otherwise an error could appear), it is converted to
#'integer.
#'
#'@param year It is the input of the function, consisting of an ideally integer in order to specify a correct file.
#'If a string is entered, an error can appear.
#'
#'@return the function retrieves a long string indicating the name
#'of a file being the integer inserted inside of the string.
#'
#'@note In conjunction
#'with the previous function it can accomplish in a handy way the import of
#'datasets, just entering a given number as input.
#'
#'@examples \dontrun{make_filename()}
#'
#'@export



make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

year<-1999

#'Data List By Year
#'
#'@description This function retrieves a list consisting of datasets.
#'It uses the previously described functions (\code{make_filename}, \code{fars_read})
#'while it is inserted an error handler. This time it allows inputting
#'a vector thanks to the function \code{lapply} (for each element of the vector,
#'applies the specified function). First, thanks to the functions
#'\code{make_filename} and \code{fars_read}; gathers a dataset (expecting that the file
#'will be in the working directory) and then the function reads it and tries
#'to select a given subset through the external package \code{dplyr}, adding
#'the variable year to the dataset.
#'
#'@param years It is the input of the function, consisting of ideally integers in order to specify correct files.
#'If a string is entered, an error can appear
#'
#'@note In case of error, no data is retrieved
#'and the message invalid year is displayed.
#'
#'@importFrom "dplyr" "mutate"
#'
#'@importFrom "dplyr" "select"
#'
#'@import magrittr
#'
#'
#'@return A subset of a dataframe (final output) is retrieved
#'
#'@examples \dontrun{fars_read_years()}
#'
#'@export



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

#'Summary of the dataset
#'
#'@description This function first uses the already described one, \code{fars_read_years}.
#'Using the external package \code{dplyr}, first bind the provided list into
#'a compact dataset with the function \code{bind_rows}. And creates a summary
#'dataset thanks to the function \code{summarize}, using the function \code{n()} which
#'counts the number of occurrences stratified by month and year thanks
#'to the function \code{group_by}. Finally, it is created a contingency table
#'thanks to the package \code{tidyr}, which displays in a better the summary
#'data (number of observations by year and moth)
#'
#'@param years It is the input of the function, consisting of ideally integers in order to specify correct files.
#'If a string is entered, an error can appear
#'
#'@importFrom "dplyr" "bind_rows" "group_by" "summarize" "n"
#'
#'@importFrom "tidyr" "spread"
#'
#'@import magrittr
#'
#'@return A summary of a dataframe as a table is retrieved
#'
#'@examples \dontrun{fars_summarize_years()}
#'
#'@export



fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = dplyr::n()) %>%
    tidyr::spread(year, n)
}

#'Create a graphical map with the dataset
#'
#'@description This function draws a map of USA and places in it certain points
#'defined in the dataset. In order to accomplish this task, it is
#'created a function which takes two arguments as inputs, they
#'should be numeric values otherwise an error can be retrieved
#'(year and sate_number). First, gathers a given dataset thanks
#'to a number provided as years and then filters the dataset
#'according to a state number provided (examining if the state
#'is in the dataset and if there is data for the given state,
#'a basic error handling procedure). Then, it cleans the column
#'longitude, deleting values which are higher than 900 and column
#'latitude, this time if values are higher than 90. Finally, a map
#'is produced thanks to the package \code{maps} and points specified by
#'the dataset are placed in it thanks to the package \code{graphics}
#'and the function \code{points}.
#'
#'@param state.num It is the input of the function, consisting of ideally integers in order to specify correct files
#'and subsets.
#'If a string is entered, an error can appear
#'
#'@param year It is the input of the function, consisting of ideally integers in order to specify correct files
#'and subsets.
#'If a string is entered, an error can appear
#'
#'@importFrom "maps" "map"
#'
#'@importFrom "graphics" "points"
#'
#'@return Finally, a plot is retrieved
#'
#'@examples \dontrun{fars_map_state()}
#'
#'@export



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

