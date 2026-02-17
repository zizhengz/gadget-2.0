plot_regional_pd = function(prepared.data, origin.data, target.feature.name, node.idx,
  color.ice, color.pd, ymin, ymax, split.condition = NULL,
  show.point, mean.center) {
  plot = lapply(names(prepared.data), function(feat) {
    data = prepared.data[[feat]]
    subset.idx = which(data$node == node.idx)
    data.subset = data[subset.idx, ]
    origin.data.subset = origin.data[subset.idx, ]
    if (feat %in% colnames(origin.data.subset) && is.factor(origin.data.subset[[feat]])) {
      origin.data.subset[[feat]] = factor_to_numeric(origin.data.subset[[feat]])
    }

    # data transformation
    data.subset = data.subset[, -ncol(data.subset), drop = FALSE]
    n_rows = nrow(data.subset)
    n_cols = ncol(data.subset)

    if (n_rows == 0 || n_cols == 0) {
      # return empty plot if no data
      return(ggplot() + theme_bw() + labs(title = "No data"))
    }

    # wide to long conversion
    grid_values = as.numeric(colnames(data.subset))
    value_matrix = as.matrix(data.subset)

    # create long format data
    ice.long = data.frame(
      grid = rep(grid_values, each = n_rows),
      value = as.vector(value_matrix),
      id = rep(seq_len(n_rows), times = n_cols),
      type = "ICE"
    )

    # calculate mean ICE for PDP
    valid_values = !is.na(ice.long$value)
    if (sum(valid_values) > 0) {
      mean.ice = tapply(ice.long$value[valid_values], ice.long$grid[valid_values], mean, na.rm = TRUE)
      pdp.centered = data.frame(
        grid = as.numeric(names(mean.ice)),
        value = as.numeric(mean.ice),
        id = NA,
        type = "PDP"
      )
      plot.data = rbind(
        ice.long[, c("grid", "value", "id", "type")],
        pdp.centered[, c("grid", "value", "id", "type")]
      )
    } else {
      plot.data = ice.long[, c("grid", "value", "id", "type")]
    }

    # check if we can draw lines
    noline = length(unique(plot.data$grid[!is.na(plot.data$value)])) < 2

    p = ggplot(plot.data, aes(x = .data[["grid"]], y = .data[["value"]], group = .data[["id"]], color = .data[["type"]]))
    if (!noline) {
      p = p + geom_line(alpha = 0.9, linewidth = 0.5, linetype = "dotted", na.rm = TRUE)
      p = p + geom_line(data = subset(plot.data, type == "PDP"), linewidth = 0.8)
    } else {
      p = p + geom_point(size = 1, shape = 4, na.rm = TRUE)
      p = p + geom_point(data = subset(plot.data, type == "PDP"), size = 3, shape = 4, na.rm = TRUE)
    }
    if (feat %in% colnames(origin.data.subset)) {
      p = p + geom_point(data = origin.data.subset,
        aes(x = .data[[feat]], y = .data[[target.feature.name]]),
        alpha = if (show.point) 0.3 else 0, size = 0.8, inherit.aes = FALSE)
    }
    p = p +
      scale_color_manual(values = c("ICE" = color.ice, "PDP" = color.pd),
        labels = c("ICE" = if (mean.center) "Mean centered ICE" else "ICE",
          "PDP" = if (mean.center) "Mean centered PDP" else "PDP")) +
      coord_cartesian(ylim = c(ymin, ymax)) +
      # ylim(ymin, ymax) +
      theme_bw(base_size = 9) +
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
