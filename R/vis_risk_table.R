#' Visualise risk scores in a table.
#'
#' @importFrom DT datatable
#' @importFrom scales percent
#'
#' @author Finlay Campbell
#'
#' @param risk The updated risk table access via \code{values$}
#' @param weightings The updated weightings access via \code{values$}
#'
vis_risk_table <- function(tbl, weightings, key_col = NULL) {
  if (is.null(tbl) || is.null(weightings)) {
    return(NULL)
  }

  # Identify key column
  if (is.null(key_col)) {
    key_col <- names(tbl)[1]
  }

  # Identify last (total) column
  last_col <- names(tbl)[ncol(tbl)]

  # ---- Sort by last column, NAs always last ----
  tbl <- tbl[
    order(
      is.na(tbl[[last_col]]),
      -tbl[[last_col]],
      na.last = TRUE
    ),
  ]

  # ---- Format numeric columns to 2 decimals ----
  tbl_fmt <- tbl %>%
    dplyr::mutate(across(where(is.numeric), ~ sprintf("%.2f", .x)))

  # ---- Add % to weight-bearing indicator columns ----
  tbl_fmt <- tbl_fmt %>%
    dplyr::rename_with(function(cols) {
      sapply(cols, function(col) {
        if (col %in% names(weightings)) {
          paste0(col, " (", scales::percent(weightings[col]), ")")
        } else {
          col
        }
      })
    })

  # ---- Label total column ----
  names(tbl_fmt)[names(tbl_fmt) == last_col] <- paste0(last_col, " (Total)")

  tbl_fmt
}
