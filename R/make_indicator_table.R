make_indicator_table <- function(scores, risks, groupings, pillar_name) {
  # Extract indicator weights for this pillar
  indicators <- groupings[[pillar_name]] # named numeric vector
  indicator_names <- names(indicators)

  # Extract raw indicator values
  raw <- scores[, indicator_names, drop = FALSE]

  # Create weighted column labels (Temp Severity Score (100%), Elderly Severity Score (17%), etc)
  col_labels <- sprintf("%s (%.0f%%)", indicator_names, indicators * 100)
  names(raw) <- col_labels

  # Extract pillar-level score (already computed in get_risks)
  pillar_scores <- risks[[pillar_name]]

  # Build final table
  tibble::tibble(
    Adm1 = scores$Adm1,
    raw,
    !!pillar_name := pillar_scores
  )
}
