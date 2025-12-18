#' Visualise scores on a map.
#'
#' @importFrom sf st_centroid
#'
#' @author Finlay Campbell
#'
vis_scores <- function(
  map_sf,
  value,
  value_label = "Risk Score",
  title = NULL,
  region = "HQ",
  data_source = "World Health Organization",
  date = Sys.Date()
) {
  stopifnot(inherits(map_sf, "sf"))
  stopifnot(value %in% names(map_sf))

  risk_palette <- c(
    "#0f2d5b",
    "#53abd0",
    "#d6dae5",
    "#d9777d",
    "#a00016"
  )

  disclaimer_labs <- whomapper::who_map_annotate(
    region = region,
    data_source = data_source
  )[[1]]

  who_map_text_theme <- theme(
    plot.title = element_text(
      color = who_map_col("title"),
      size = 16,
      face = "bold",
      hjust = 0
    ),
    plot.subtitle = element_text(
      color = who_map_col("title"),
      size = 13,
      hjust = 0
    ),
    plot.caption = element_text(
      hjust = 0,
      size = 6,
      lineheight = 1.1
    ),
    legend.position = "bottom"
  )

  ggplot(map_sf) +
    geom_sf_who_poly(aes(fill = !!sym(value))) +
    scale_fill_gradientn(
      colours = risk_palette,
      limits = c(1, 5),
      oob = scales::squish,
      na.value = who_map_col("no_data"),
      name = value_label
    ) +
    labs(
      title = title,
      subtitle = paste("As of", format(as.Date(date), "%d %b %Y"))
    ) +
    disclaimer_labs +
    theme_void() +
    who_map_text_theme
}
