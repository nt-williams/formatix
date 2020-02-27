
#' Set gtsummary options for wcmtheme
#'
#' @export
wcmtheme.gtsummary_options <- function() {
  options(
    gtsummary.as_gt.addl_cmds =
      "gt::tab_style(
       style = list(
       gt::cell_fill(color = 'white'),
           gt::cell_text(font = 'Roboto Condensed'),
           gt::cell_borders(color = 'white')
         ),
         location = gt::cells_body()
       ) %>% gt::tab_style(
       style = list(gt::cell_text(font = 'Roboto Condensed')),
         location = gt::cells_column_labels(columns = dplyr::everything())
       ) %>% gt::tab_options(
       table.font.size = 13.5,
         data_row.padding = gt::px(1),
         table.border.top.color = 'white'
       )"
  )
}

