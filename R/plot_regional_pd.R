plot_regional_pd = function(prepared_regional_pd_data, target.feature.name, node_idx, color_ice, color_pd, ymin, ymax, split_condition = NULL) {

  plot = lapply(names(prepared_regional_pd_data), function(feat) {
    data = prepared_regional_pd_data[[feat]]
    data_subset = data[data$node == node_idx, ]

    ice.long = tidyr::gather(data_subset[, -ncol(data_subset)], grid, value)
    ice.long$grid = as.numeric(ice.long$grid)
    ice.long$id = rep(seq_len(nrow(data_subset)), times = length(unique(ice.long$grid)))
    ice.long$type = "ICE"

    pdp_centered = ice.long %>%
      dplyr::group_by(grid) %>%
      dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
      dplyr::mutate(id = NA, type = "PDP")

    plot_data = rbind(
      ice.long[, c("grid", "value", "id", "type")],
      pdp_centered[, c("grid", "value", "id", "type")]
    )

    p = ggplot(plot_data, aes(x = grid, y = value, group = id, color = type)) +
      geom_line(alpha = 0.9, linewidth = 0.3, linetype = "dotted", na.rm = TRUE) +
      geom_line(data = subset(plot_data, type == "PDP"), linewidth = 0.8) +
      # geom_point(data = data_subset, aes_string(x = feat, y = "y"), alpha = 0.4) +
      scale_color_manual(values = c("ICE" = color_ice, "PDP" = color_pd), labels = c("ICE" = "Mean centered ICE", "PDP" = "PDP")) +
      coord_cartesian(ylim = c(ymin, ymax)) +
      theme_bw(base_size = 9) +
      theme_bw() +
      labs(
        x = if (!is.null(split_condition)) paste0(feat, " | ", split_condition) else feat,
        y = target.feature.name,
        title = "Partial Dependence Plot",
        color = NULL
      ) +
      theme(
        legend.position = c(0.95, 0.95),
        legend.justification = c("right", "top"),
        legend.background = element_rect(fill = NA, color = NA),
        legend.box.background = element_rect(fill = NA, color = "grey", linewidth = 0.1),
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.6, "lines"),
        plot.title = element_text(hjust = 0.5, size = 9),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 9)
      )
    return(p)
  })
  return(plot)
}
