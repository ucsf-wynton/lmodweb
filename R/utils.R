trim <- function(x) {
  if (!is.character(x)) return(x)
  x <- sub("^[\t\n\f\r ]*", "", x)
  sub("[\t\n\f\r ]*$", "", x)
}


#' Converts a Markdown string into HTML
#'
#' @param x A Markdown-formatted character string.
#'
#' @return A HTML-formatted character string.
#'
#' @importFrom commonmark markdown_html
#' @export
as_html <- function(x) {
  if (!is.character(x)) return(x)
  x <- markdown_html(x, extensions = TRUE)
  x <- gsub("(^<p>|</p>\n$)", "", x)
  x
}
