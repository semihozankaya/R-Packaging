#' Gets historical exchange rates
#' @param numerator numerator of the requested exchange rate
#' @param denominator denominator of the requested exchange rate
#' @inheritParams get_usdhufs
#' @return \code{data.table} object
#' @export
#' @importFrom checkmate assert_data_table
#' @importFrom logger log_error log_info
#' @importFrom data.table data.table
#' @importFrom httr GET content
#' @examples
#' get_exchange_rates(start_date = "2021-05-16", end_date = "2021-05-18", numerator = "EUR", denominator = "USD")

get_exchange_rates <- function(start_date = Sys.Date() - 30, end_date = Sys.Date(), retried = 0, numerator = "USD", denominator = "HUF") {
  tryCatch({
    response <- GET(
      "https://api.exchangerate.host/timeseries",
      query = list(
        start_date = start_date,
        end_date = end_date,
        base = numerator,
        symbols = denominator
      )
    )
    exchange_rates <- content(response)$rates
    rates <- data.table(date = as.Date(names(exchange_rates)),
                          exchange_rate = as.numeric(unlist(exchange_rates)))
    assert_data_table(rates, any.missing = FALSE)
  }, error = function(e) {
    log_error(e$message)
    Sys.sleep(1 + retried ^ 2)
    get_exchange_rates(start_date = start_date, end_date = end_date, retried = retried + 1, numerator = numerator, denominator = denominator)
  })
  log_info('The average {numerator}/{denominator} rate over the last {as.Date(end_date)-as.Date(start_date) + 1} days is {round(mean(rates$exchange_rate), 2)}')
  rates
}
