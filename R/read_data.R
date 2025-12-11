#' Read scores and groupings from data file provided
#'
#' @importFrom readxl read_excel
#'
#' @author Finlay Campbell
#'
read_data <- function(path) {

  # --- Read both sheets ---
  indicator_data <- readxl::read_excel(path, sheet = "Indicator Data", skip = 2)
  indicator_meta <- readxl::read_excel(path, sheet = "Indicator Metadata") |> 
    filter(Include)

  # --- Extract indicator columns automatically ---
  indicator_cols <- setdiff(names(indicator_data), c("Adm0", "Adm1"))

  # --- Filter metadata so it only includes indicators present in the data ---
  meta_clean <- indicator_meta %>%
    filter(Indicator %in% indicator_cols)

  # --- Build groupings: pillar → vector of equal-weight indicators ---
  groupings <- meta_clean %>%
    split(.$Pillar) %>%      # split into Exposure / Vulnerability / LOCC
    lapply(function(df) {
      n <- nrow(df)
      w <- rep(1/n, n)       # equal weights for each indicator
      names(w) <- df$Indicator
      w
    })

  # --- Scores table ---
  scores <- indicator_data %>%
    select(Adm1, all_of(indicator_cols))

  return(list(
    scores = scores,
    groupings = groupings,
    metadata = meta_clean
  ))
}