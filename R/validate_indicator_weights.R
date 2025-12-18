validate_indicator_weights <- function(df, tol = 1e-6) {
  stopifnot(all(c("Pillar", "Indicator", "Indicator Weight") %in% names(df)))

  df %>%
    group_by(Pillar) %>%
    summarise(
      total = sum(`Indicator Weight`, na.rm = TRUE),
      off_by = total - 1,
      valid = abs(off_by) < tol,
      .groups = "drop"
    )
}
