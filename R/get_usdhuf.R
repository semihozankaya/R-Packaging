#' Gets the latest USD/HUF rate
#' @param retried it defines the system's sleeping time. No input needed.
#' @return numeric the current USD/HUF rate
#' @export
#' @importFrom checkmate assert_number
#' @importFrom jsonlite fromJSON
#' @importFrom logger log_info log_error
#' @examples
#' get_usdhuf()

get_usdhuf <- function(retried = 0) {
  tryCatch({
    usdhuf <- fromJSON('https://api.exchangerate.host/latest?base=USD&symbols=HUF')$rates$HUF
    assert_number(usdhuf, lower = 250, upper = 400)
  }, error = function(e) {
    ## str(e)
    log_error(e$message)
    Sys.sleep(1 + retried ^ 2)
    get_usdhuf(retried = retried + 1)
  })
  log_info('1 USD = {forint(usdhuf)}')
  usdhuf
}

#' Gets the historical USD/HUF rate
#' @param start_date starting date to receive exchange rates information
#' @param end_date the last date for our exchange rates
#' @inheritParams get_usdhuf
#' @return \code{data.table} object
#' @export
#' @importFrom checkmate assert_data_table
#' @importFrom logger log_error log_info
#' @importFrom data.table data.table
#' @importFrom httr GET content
#' @examples
#' get_usdhufs(start_date = "2021-05-16", end_date = "2021-05-18")

get_usdhufs <- function(start_date = Sys.Date() - 30, end_date = Sys.Date(), retried = 0) {
  tryCatch({
    response <- GET(
      "https://api.exchangerate.host/timeseries",
      query = list(
        start_date = start_date,
        end_date = end_date,
        base = "USD",
        symbols = "HUF"
      )
    )
    exchange_rates <- content(response)$rates
    usdhufs <- data.table(date = as.Date(names(exchange_rates)),
                                 usdhuf = as.numeric(unlist(exchange_rates)))
    assert_data_table(usdhufs, any.missing = FALSE)
  }, error = function(e) {
    log_error(e$message)
    Sys.sleep(1 + retried ^ 2)
    get_usdhufs(start_date = start_date, end_date = end_date, retried = retried + 1)
  })
  log_info('The average USD/HUF rate over the last {as.Date(end_date)-as.Date(start_date) + 1} days is {forint(mean(usdhufs$usdhuf))}')
  usdhufs
}
