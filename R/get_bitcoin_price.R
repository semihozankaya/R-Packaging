#' Gets the latest bitcoin price in USD
#' @param retried it defines the system's sleeping time. No input needed.
#' @return numeric the current Bitcoin price
#' @export
#' @importFrom checkmate assert_number
#' @importFrom binancer binance_coins_prices
#' @importFrom logger log_info log_error
#' @import data.table
#' @examples
#' get_bitcoin_price()

get_bitcoin_price <- function(retried = 0) {
  tryCatch({
    btcusdt <- binance_coins_prices()[symbol == 'BTC', usd]
    assert_number(btcusdt, lower = 1000)
  },
  error = function(e) {
    log_error(e$message)
    Sys.sleep(1 + retried ^ 2)
    get_bitcoin_price(retried = retried + 1)
  })
  log_info('The current Bitcoin price is {dollar(btcusdt)}')
  btcusdt
}
