validate_pillar_weights <- function(df, tol = 1e-6) {
  stopifnot(all(c("Pillar", "Pillar Weight") %in% names(df)))

  total <- sum(df$`Pillar Weight`, na.rm = TRUE)
  off_by <- total - 1

  list(
    valid = abs(off_by) < tol,
    total = total,
    off_by = off_by
  )
}
