plot_regional_pd = function(prepared.data, origin.data, target.feature.name, node.idx,
                            color.ice, color.pd, ymin, ymax, split.condition = NULL,
                            show.point, mean.center) {
  plot = lapply(names(prepared.data), function(feat) {
    data = prepared.data[[feat]]
    subset.idx = which(data$node == node.idx)
    data.subset = data[subset.idx, ]
    origin.data.subset = origin.data[subset.idx, ]

    #  wide to long for the plot
    data.subset = data.subset[, -ncol(data.subset)]
    grid = rep(colnames(data.subset), each = nrow(data.subset))
    value = as.vector(as.matrix(data.subset))
    ice.long = data.frame(grid = grid, value = value)
    ice.long$grid = as.numeric(ice.long$grid)
    ice.long$id = rep(seq_len(nrow(data.subset)), times = length(unique(ice.long$grid)))
    ice.long$type = "ICE"

    mean.ice = tapply(ice.long$value, ice.long$grid, mean, na.rm = TRUE)
    pdp.centered = data.frame(grid = as.numeric(names(mean.ice)),
      value = as.numeric(mean.ice),
      id = NA,
      type = "PDP")

    plot.data = rbind(
      ice.long[, c("grid", "value", "id", "type")],
      pdp.centered[, c("grid", "value", "id", "type")]
    )

    p = ggplot(plot.data, aes(x = grid, y = value, group = id, color = type)) +
      geom_line(alpha = 0.9, linewidth = 0.3, linetype = "dotted", na.rm = TRUE) +
      geom_line(data = subset(plot.data, type == "PDP"), linewidth = 0.8) +
      geom_point(data = origin.data.subset, aes_string(x = feat, y = target.feature.name),
                 alpha = if (show.point) 0.3 else 0, size = 0.8, inherit.aes = FALSE) +
      scale_color_manual(values = c("ICE" = color.ice, "PDP" = color.pd),
                         labels = c("ICE" = if (mean.center) "Mean centered ICE" else "ICE",
                                    "PDP" = if (mean.center) "Mean centered PDP" else "PDP")) +
      coord_cartesian(ylim = c(ymin, ymax)) +
      theme_bw(base_size = 9) +
      theme_bw() +
      labs(
        x = if (!is.null(split.condition)) paste0(feat, " | ", split.condition) else feat,
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
