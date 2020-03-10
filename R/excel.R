
gen_cell_limits <- function(rows, columns) {

  if (is.null(rows) && is.null(columns)) {
    return(cellranger::as.cell_limits())
  } else if (is.null(rows) && !is.null(columns)) {
    return(cellranger::cell_cols(columns))
  } else if (is.null(columns) && !is.null(rows)) {
    return(cellranger::cell_rows(rows))
  } else {
    cs <- cell_format_cols(columns)
    rs <- cell_format_rows(rows)
  }

  # cellranger format if both present
  s <- paste0(cs[1], rs[1])
  e <- paste0(cs[2], rs[2])
  . <- paste(s, e, sep = ":")

  cellranger::as.cell_limits(.)
}

cell_format_cols <- function(cols) {
  cs <- strsplit(cols, ":")[[1]]
  sc <- cs[1]
  if (length(cs) == 1) {
    ec <- cs[1]
  } else {
    ec <- cs[2]
  }
  . <- c(sc, ec)
  return(.)
}

cell_format_rows <- function(rows) {
  sr <- min(rows)
  er <- max(rows)
  . <- c(sr, er)
  return(.)
}

#' Low-level interface for reading excel workbooks
#'
#' @param path
#' @param sheet
#' @param rows
#' @param columns
#' @param na
#' @param trim_ws
#' @param progress
#' @param .name_repair
#'
#' @return
#' @export
#'
#' @examples
read_excel <- function(path, sheet = NULL, rows = NULL, columns = NULL,
                       na = "", trim_ws = TRUE, progress = readxl_progress(),
                       .name_repair = "unique") {

  ranges <- purrr::map(columns, ~ gen_cell_limits(rows = rows, .x))
  . <-
    purrr::map_dfc(
      ranges,
      ~ readxl::read_excel(
        path = path,
        sheet = sheet,
        range = .x,
        col_names = TRUE,
        col_types = NULL,
        na = "",
        trim_ws = TRUE,
        progress = readxl_progress(),
        .name_repair = "unique"
      )
    )
  return(.)
}
