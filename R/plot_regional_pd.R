#' Plot regional PD/ICE for one node (internal).
#' @param prepared_data,origin_data,target_feature_name,node_idx,color_ice,
#'   color_pd,ymin,ymax,split_condition,show_point,mean_center Plot arguments.
#' @keywords internal
plot_regional_pd = function(prepared_data, origin_data, target_feature_name, node_idx,
  color_ice, color_pd, ymin, ymax, split_condition = NULL,
  show_point, mean_center) {
  plot = lapply(names(prepared_data), function(feat) {
    data = prepared_data[[feat]]
    subset_idx = which(data$node == node_idx)
    data_subset = data[subset_idx, ]
    origin_data_subset = origin_data[subset_idx, ]
    if (feat %in% colnames(origin_data_subset) && is.factor(origin_data_subset[[feat]])) {
      origin_data_subset[[feat]] = factor_to_numeric(origin_data_subset[[feat]])
    }

    # data transformation
    data_subset = data_subset[, -ncol(data_subset), drop = FALSE]
    n_rows = nrow(data_subset)
    n_cols = ncol(data_subset)

    if (n_rows == 0 || n_cols == 0) {
      # return empty plot if no data
      return(ggplot() + theme_bw() + labs(title = "No data"))
    }

    # wide to long conversion
    grid_values = as.numeric(colnames(data_subset))
    value_matrix = as.matrix(data_subset)

    # create long format data
    ice_long = data.frame(
      grid = rep(grid_values, each = n_rows),
      value = as.vector(value_matrix),
      id = rep(seq_len(n_rows), times = n_cols),
      type = "ICE"
    )

    # calculate mean ICE for PDP
    valid_values = !is.na(ice_long$value)
    if (sum(valid_values) > 0) {
      mean_ice = tapply(ice_long$value[valid_values], ice_long$grid[valid_values], mean, na.rm = TRUE)
      pdp_centered = data.frame(
        grid = as.numeric(names(mean_ice)),
        value = as.numeric(mean_ice),
        id = NA,
        type = "PDP"
      )
      plot_data = rbind(
        ice_long[, c("grid", "value", "id", "type")],
        pdp_centered[, c("grid", "value", "id", "type")]
      )
    } else {
      plot_data = ice_long[, c("grid", "value", "id", "type")]
    }

    # check if we can draw lines
    noline = length(unique(plot_data$grid[!is.na(plot_data$value)])) < 2

    p = ggplot(plot_data, aes(x = .data[["grid"]], y = .data[["value"]],
        group = .data[["id"]], color = .data[["type"]]))
    if (!noline) {
      p = p + geom_line(alpha = 0.9, linewidth = 0.5, linetype = "dotted", na.rm = TRUE)
      p = p + geom_line(data = subset(plot_data, type == "PDP"), linewidth = 0.8)
    } else {
      p = p + geom_point(size = 1, shape = 4, na.rm = TRUE)
      p = p + geom_point(data = subset(plot_data, type == "PDP"), size = 3, shape = 4, na.rm = TRUE)
    }
    if (feat %in% colnames(origin_data_subset)) {
      p = p + geom_point(data = origin_data_subset,
        aes(x = .data[[feat]], y = .data[[target_feature_name]]),
        alpha = if (show_point) 0.3 else 0, size = 0.8, inherit.aes = FALSE)
    }
    p = p +
      scale_color_manual(values = c("ICE" = color_ice, "PDP" = color_pd),
        labels = c("ICE" = if (mean_center) "Mean centered ICE" else "ICE",
          "PDP" = if (mean_center) "Mean centered PDP" else "PDP")) +
      coord_cartesian(ylim = c(ymin, ymax)) +
      # ylim(ymin, ymax) +
      theme_bw(base_size = 9) +
      labs(
        x = if (!is.null(split_condition)) paste0(feat, " | ", split_condition) else feat,
        y = target_feature_name,
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
    p
  })
  plot
}
