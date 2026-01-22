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

  # ---- STEP 1: Sort by last column, NAs always last ----
  last_col <- names(tbl)[ncol(tbl)]
  tbl <- tbl[
    order(
      is.na(tbl[[last_col]]), # NA = TRUE → bottom
      -tbl[[last_col]], # descending
      na.last = TRUE
    ),
  ]

  # ---- STEP 2: Format numeric columns to 2 decimals ----
  tbl_fmt <- tbl %>%
    mutate(across(where(is.numeric), ~ sprintf("%.2f", .x)))

  # ---- STEP 3: Add % to weight-bearing columns ----
  tbl_fmt <- tbl_fmt %>%
    rename_with(function(cols) {
      sapply(cols, function(col) {
        if (col %in% names(weightings)) {
          paste0(col, " (", scales::percent(weightings[col]), ")")
        } else {
          col
        }
      })
    })

  tbl_fmt
}
