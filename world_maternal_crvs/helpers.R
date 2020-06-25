### map helpers
# very liberally and blatantly copied from Mathew Kiang's wonderful code
# https://github.com/mkiang/opioid_geographic/tree/master/online_results_viewer/shiny_helpers

######

gen_color_legend <- function() {
  c_legend <-
    expand.grid(list(
      mort_lev = c("low_mort", "med_mort", "high_mort"),
      apc_lev  = c("low_apc", "med_apc", "high_apc")
    ),
    stringsAsFactors = TRUE) %>%
    as_tibble() %>%
    mutate(
      color_hex = c(
        "#e8e6f2",
        "#b5d3e7",
        "#4fadd0",
        "#e5b4d9",
        "#b8b3d8",
        "#3983bb",
        "#d34fa6",
        "#b03598",
        "#2a1a8a"
      ),
      color_hex_a = c(
        "#e8e8e8",
        "#e4acac",
        "#c85a5a",
        "#b0d5df",
        "#ad9ea5",
        "#985356",
        "#64acbe",
        "#627f8c",
        "#574249"
      )
    )
  
  return(c_legend)
}

gen_hotspots_legend <- function(mort_mid_bin, apc_mid_bin) {
  ## Create the plot
  p_legend <-
    ggplot(gen_color_legend(),
           aes(x = mort_lev, y = apc_lev, fill = color_hex_a)) +
    geom_tile(color = "white") +
    scale_fill_identity(na.value = "grey50") +
    scale_x_discrete(
      "Maternal mortality ratio\n(per 100,000 live births)",
      expand = c(0.05, 0),
      labels = c(
        sprintf("Low: 0-%0.1f",
                mort_mid_bin[1]),
        sprintf(
          "Medium: %0.1f-%0.1f",
          mort_mid_bin[1],
          mort_mid_bin[2]
        ),
        sprintf("High: >%0.1f",
                mort_mid_bin[2])
      )
    ) +
    scale_y_discrete(
      "CRVS coverage (%)",
      expand = c(0.05, 0),
      labels = c(
        sprintf("Low: 0-%i",
                apc_mid_bin[1]),
        sprintf("Moderate: %i-%i",
                apc_mid_bin[1],
                apc_mid_bin[2]),
        sprintf("High: >%i",
                apc_mid_bin[2])
      )
    ) +
    labs(subtitle = "Legend") +
    coord_equal()
  
  return(p_legend)
}