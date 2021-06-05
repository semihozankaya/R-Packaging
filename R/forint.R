#' Formats a number to HUF
#' @param x numeric vector, pereferably a monetary value
#' @return string formated for ease of use
#' @export
#' @importFrom checkmate assert_numeric
#' @importFrom scales dollar
#' @examples
#' forint(42)
#' forint(420189567.244)
#' forint(1:10)

forint <- function(x) {
  assert_numeric(x)
  dollar(x, prefix = '', suffix = ' HUF')
}
