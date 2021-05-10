#' Formats a number to HUF
#' @param x number, pereferably a monetary value
#' @return string formated for ease of use
#' @export
#' @importFrom checkmate assert_number
#' @importFrom scales dollar
#' @examples
#' forint(42)
#' forint(420189567.244)

forint <- function(x) {
  assert_number(x)
  dollar(x, prefix = '', suffix = ' HUF')
}
