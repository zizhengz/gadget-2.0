# source("R/helper_effects.R")
#---------------------------------------------------------------------------------------------------
# HELPER FUNCTIONS FOR PLOTTING
#---------------------------------------------------------------------------------------------------

#' Mean-center ICE curves
#'
#' Converts a long‐format ICE data set into a wide matrix
#' @param ice A **data.frame** (or **data.table**) in long ICE format;
#'   must contain a value column \code{.value}.
#' @param feature Column name (string) used as the key when spreading to
#'   wide format.
#'
#' @return A **data.table** (in wide format) of centered ICE curves
#'
#' @importFrom tidyr pivot_wider
#' @importFrom data.table setDT
#'
#' @keywords internal
mean_center_ice = function(ice, feature) {
  Y = tidyr::pivot_wider(ice, names_from = feature, values_from = .value)
  Y = Y[, setdiff(colnames(Y), c(".type", ".id"))]

  # center ICE curves by their mean
  Y = Y - rowMeans(Y)
  data.table::setDT(Y)
}



#' Prepare centered ICE data for regional PDP plots
#'
#' @param effect A list returned by your earlier pipeline, expected to
#'   contain \code{$results} with ICE curves per feature.
#' @param tree A list-of-lists representing the tree (as in
#'   \code{compute_tree()} output).
#' @param depth Integer depth level whose nodes should be visualized.
#'
#' @return A named \code{list} of **data.table** objects, one per
#'   feature, each with an added integer column \code{node}.
#'
#' @keywords internal
regional_pd = function(effect, tree, depth) {

  nodes = tree[[depth]]
  ice_curves = effect$results
  ice_centered = lapply(names(ice_curves), function(feat) {
    ice = mean_center_ice(ice_curves[[feat]][, -which(colnames(ice_curves[[feat]]) == ".feature")], ".borders")
    grid_total = colnames(ice)
    ice$node = NA_integer_

    for (node_num in 1:length(nodes)) {
      node = nodes[[node_num]]
      subset = node$subset.idx
      grid = node$grid[[feat]]
      ice$node[subset] = node_num

      if (length(grid) < length(grid_total)) {
        ice[subset, which(!(grid_total %in% grid))] = NA
        # ice[subset, (grid_total)] = ice[subset, ..grid_total] - rowMeans(ice[subset, ..grid_total], na.rm = TRUE)
        ice[subset, grid_total] = ice[subset, grid_total, with = FALSE] - rowMeans(ice[subset, grid_total, with = FALSE], na.rm = TRUE)
      }

    }
    return(ice)
  })
  names(ice_centered) = names(ice_curves)
  return(ice_centered)
}


#' Generate regional ICE + PDP plot for single tree node
#'
#' @param effect A named \code{list} whose elements are \code{data.table}s /
#'   \code{data.frame}s produced by \code{regional_pd()}; each element
#'   contains centered ICE data for one feature and a column
#'   \code{node}.
#' @param target.feature Character. The response-axis label to show
#'   on the y-axis (e.g., \code{"\\hat{f}_j"}).
#' @param node_num Integer. Which node (at the chosen depth) to
#'   visualize.
#' @param color_ice Color value used for ICE curves
#' @param color_pd Color value used for the PDP line
#' @param ymin, ymax Numeric limits for the y-axis (kept identical
#'   across features so multiple plots can share a scale).
#' @param split_condition Optional character string appended to each
#'   facet label, e.g. \code{"x1 > 0.3"}.
#'
#' @return A \code{list} of ggplot objects—one for every feature in the
#'   supplied \code{effect}.
#'
#' @importFrom tidyr gather
#' @importFrom dplyr group_by summarise mutate
#' @importFrom ggplot2 ggplot aes geom_line scale_color_manual
#'
#' @keywords internal
regional_pd_plot = function(effect, target.feature, node_num, color_ice, color_pd, ymin, ymax, split_condition = NULL) {
  plot = lapply(names(effect), function(feat) {

    data = effect[[feat]]
    data_subset = data[data$node == node_num, ]

    ice = tidyr::gather(data_subset[, -ncol(data_subset)], grid, value)
    ice$grid = as.numeric(ice$grid)
    # ice$id = rep(1:nrow(data_subset), length(unique(ice$grid)))
    ice$id = rep(seq_len(nrow(data_subset)), times = length(unique(ice$grid)))
    ice$type = "ICE"

    pdp_centered = ice %>%
      dplyr::group_by(grid) %>%
      dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
      dplyr::mutate(id = NA, type = "PDP")

    plot_data = rbind(
      ice[, c("grid", "value", "id", "type")],
      pdp_centered[, c("grid", "value", "id", "type")]
    )

    # p_centered_ice  = ggplot() + geom_line(data = ice, aes(x = grid, y = value, group = id), alpha = 0.4, col = color_ice) +
    #   geom_line(data = pdp_centered, aes(x = grid, y = pdp), col = color_pd, lwd = 1.5) +
    #   theme_bw() + xlab(feat) + ylab(expression(hat(f)[j])) + ylim(ymin,ymax)
    # p_centered_ice

    p = ggplot(plot_data, aes(x = grid, y = value, group = id, color = type)) +
      geom_line(alpha = 0.4, linewidth = 0.3) +
      geom_line(data = subset(plot_data, type == "PDP"), linewidth = 1) +
      scale_color_manual(values = c("ICE" = color_ice, "PDP" = color_pd)) +
      coord_cartesian(ylim = c(ymin, ymax)) +
      theme_bw(base_size = 9) +
      theme_bw() +
      labs(
        # x = feat,
        # y = expression(hat(f)[j]),
        x = if (!is.null(split_condition)) paste0(feat, " | ", split_condition) else feat,
        y = target.feature,
        title = "Partial Dependence Plot",
        color = NULL
      ) +
      theme(
        legend.position = c(0.95, 0.95),
        legend.justification = c("right", "top"),
        # legend.background = element_rect(fill = alpha("white", 0.7), color = NA),
        legend.background = element_rect(fill = NA, color = NA),
        legend.box.background = element_rect(fill = NA, color = "grey", linewidth = 0.1),
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.4, "lines"),
        # plot.title = element_text(hjust = 0.5)
        plot.title = element_text(hjust = 0.5, size = 9),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 9)
      )

    return(p)

  })
  return(plot)
}

# generate regional ale plot
regional_ale_plot = function(effect, color_ale, ymin, ymax, ymin2, ymax2, xmin = NULL, xmax = NULL) {
  plot = lapply(names(effect), function(feat) {

    mean_data = effect[[feat]]$mean_effect
    sd_data = effect[[feat]]$sd_effect

    p_ale = ggplot() + geom_line(data = mean_data, aes(x = x.grid, y = dL), col = color_ale, lwd = 1.5) +
      theme_bw() + xlab(feat) + ylab(expression(hat(f)[j])) + ylim(ymin, ymax)
    p_sd = ggplot() + geom_line(data = sd_data, aes(x = (x.right + x.left) / 2, y = sd), col = "orange", lwd = 1.5) +
      theme_bw() + xlab("") + ylab("sd(deriv)") + ylim(ymin2, ymax2)
    if (!is.null(xmin)) {
      p_ale = p_ale + xlim(xmin, xmax)
      p_sd = p_sd + xlim(xmin, xmax)
    }
    list(p_sd, p_ale)

  })
  names(plot) = names(effect)
  return(plot)
}

# generate regional shap plot
regional_shap_plot = function(effect, color_local, color_global, ymin, ymax, xmin = NULL, xmax = NULL) {
  plot = lapply(names(effect), function(feat) {
    # browser()
    data_shap = effect[[feat]]
    gam_mod = mgcv::gam(phi ~ s(feat.val, k = 3), data = data_shap)

    p_shap = ggplot() + geom_point(data = data_shap, aes(x = feat.val, y = phi), col = color_local, alpha = 0.4) +
      geom_line(data = data_shap, aes(x = feat.val, y = gam_mod$fitted.values), col = color_global, lwd = 1.5) +
      theme_bw() + xlab(feat) + ylab(expression(hat(f)[j])) + ylim(ymin, ymax)

    if (!is.null(xmin)) {
      p_shap = p_shap + xlim(xmin, xmax)
    }
    p_shap

  })
  names(plot) = names(effect)
  return(plot)
}


# create explanation plots: pdp
plot_pdp_split = function(sp_L2, eff, data, feature_num) {

  feature = names(data)[feature_num]
  ice_right = eff$results[[feature]][which(eff$results[[feature]]$.id %in% which(data[, sp_L2$feature[sp_L2$best.split]] > sp_L2$split.points[[which(sp_L2$best.split)]])), ]
  ice_left = eff$results[[feature]][which(eff$results[[feature]]$.id %in% which(data[, sp_L2$feature[sp_L2$best.split]] <= sp_L2$split.points[[which(sp_L2$best.split)]])), ]
  pdp = aggregate(.value ~ .borders, data = eff$results[[feature]], FUN = mean)
  pdp_right = aggregate(.value ~ .borders, data = ice_right, FUN = mean)
  pdp_left = aggregate(.value ~ .borders, data = ice_left, FUN = mean)
  colnames(pdp)[1] = colnames(pdp_right)[1] = colnames(pdp_left)[1] = feature

  p_pdp = ggplot() +
    geom_line(data = ice_right, aes_string(x = ".borders", y = ".value", group = ".id"), color = brewer.pal(11, "RdBu")[4], alpha = 0.6) +
    geom_line(data = ice_left, aes_string(x = ".borders", y = ".value", group = ".id"), color = brewer.pal(11, "RdBu")[8], alpha = 0.6) +
    geom_line(data = pdp_right, aes_string(x = feature, ".value"), lwd = 2, color = brewer.pal(11, "RdBu")[3]) +
    geom_line(data = pdp_left, aes_string(x = feature, ".value"), lwd = 2, color = brewer.pal(11, "RdBu")[9]) +
    geom_line(data = pdp, aes_string(x = feature, ".value"), color = "darkgrey", lwd = 2) +
    geom_point(data = data, aes_string(x = feature, y = "y"), alpha = 0.4) +
    theme_bw() + ylab(bquote(hat(f)[.(feature_num)])) + xlab(feature) +
    geom_rug(data = data, aes_string(x = feature), color = ifelse(data[, sp_L2$feature[sp_L2$best.split]] > sp_L2$split.points[[which(sp_L2$best.split)]], brewer.pal(11, "RdBu")[4], brewer.pal(11, "RdBu")[8]))
  p_pdp

}

# create explanation plots: ale
plot_ale_split = function(ale_preds) {

  # average over instances within each interval
  setkeyv(ale_preds, c("interval.index"))
  delta.aggr = ale_preds[, list(dL = mean(dL, na.rm = TRUE), interval.n = .N), by = c("interval.index", "x.left", "x.right")]
  delta.aggr1 = ale_preds[ale_preds$node == 1, list(dL = mean(dL, na.rm = TRUE), interval.n = .N), by = c("interval.index", "x.left", "x.right")]
  delta.aggr2 = ale_preds[ale_preds$node == 2, list(dL = mean(dL, na.rm = TRUE), interval.n = .N), by = c("interval.index", "x.left", "x.right")]

  # accumulate over the intervals
  delta.acc = delta.aggr[, list(dL.cumsum = cumsum_na(c(0, dL)), index0 = c(0, interval.index), index1 = c(interval.index, max(interval.index) + 1))]
  delta.acc1 = delta.aggr1[, list(dL.cumsum = cumsum_na(c(0, dL)), index0 = c(0, interval.index), index1 = c(interval.index, max(interval.index) + 1))]
  delta.acc2 = delta.aggr2[, list(dL.cumsum = cumsum_na(c(0, dL)), index0 = c(0, interval.index), index1 = c(interval.index, max(interval.index) + 1))]

  # the mean effect is the weighted mean of the interval mid point effects
  # weighted by the number of points in the interval
  fJ0 = delta.acc[, list(.ale0 = sum(((dL.cumsum[1:(nrow(.SD) - 1)] +
    dL.cumsum[2:nrow(.SD)]) / 2) * delta.aggr$interval.n) / sum(delta.aggr$interval.n))]
  fJ01 = delta.acc1[, list(.ale0 = sum(((dL.cumsum[1:(nrow(.SD) - 1)] +
    dL.cumsum[2:nrow(.SD)]) / 2) * delta.aggr1$interval.n) / sum(delta.aggr1$interval.n))]
  fJ02 = delta.acc2[, list(.ale0 = sum(((dL.cumsum[1:(nrow(.SD) - 1)] +
    dL.cumsum[2:nrow(.SD)]) / 2) * delta.aggr2$interval.n) / sum(delta.aggr2$interval.n))]

  # centering the ALEs
  fJ = delta.acc[, list(dL = dL.cumsum - fJ0$.ale0, x.grid = c(delta.aggr$x.left, delta.aggr$x.right[length(delta.aggr$x.right)]))]
  fJ1 = delta.acc1[, list(dL = dL.cumsum - fJ01$.ale0, x.grid = c(delta.aggr1$x.left, delta.aggr1$x.right[length(delta.aggr1$x.right)]))]
  fJ2 = delta.acc2[, list(dL = dL.cumsum - fJ02$.ale0, x.grid = c(delta.aggr2$x.left, delta.aggr2$x.right[length(delta.aggr2$x.right)]))]
  p_ale = ggplot() + geom_point(aes(x = feat.val, y = y - mean(y), color = node), data = ale_preds, alpha = 0.5) +
    geom_line(data = fJ, aes(x = x.grid, y = dL), color = "darkgrey", lwd = 2) +
    geom_line(data = fJ1, aes(x = x.grid, y = dL), color = brewer.pal(11, "RdBu")[9], lwd = 2) +
    geom_line(data = fJ2, aes(x = x.grid, y = dL), color = brewer.pal(11, "RdBu")[3], lwd = 2) +
    geom_vline(data = data.frame(interval = c(min(ale_preds$x.left), unique(ale_preds$x.right))), aes(xintercept = interval), lty = 2) + theme_classic()
  p_ale

}


#' Locate a node in a list by its id
#'
#' @param node_list A list of node objects.
#' @param id The identifier to look for.
#'
#' @return The matched node object, or \code{NULL} when no node with
#'   that \code{id} exists in \code{node_list}.
#'
#' @keywords internal
find_node_by_id = function(node_list, id) {
  for (n in node_list) {
    if (!is.null(n) && isTRUE(n$id == id)) {
      return(n)
    }
  }
  return(NULL)
}


#' Plot ICE + partial-dependence panels for every node in a tree
#'
#' @param tree  A list-of-lists representing the fitted tree. Each
#'   depth level is a list of node objects (as produced by
#'   \code{compute_tree()}).
#' @param effect  An \code{FeatureEffects} object created earlier in the pipeline that holds
#'   centered ICE curves; must have a slot \code{$results} and a vector \code{$features}.
#' @param color_ice  Color for ICE curves. Default \code{"lightblue"}.
#' @param color_pd   Color for the PDP line. Default \code{"lightcoral"}.
#' @param target.feature  Character string shown on the y-axis
#'   (e.g.\ \code{"\\hat{f}_j"}). Passed down to
#'   \code{regional_pd_plot()}.
#' @param show.plot  Logical. Print each node panel to the active
#'   graphics device? Default \code{TRUE}.
#' @param save.plot  Logical. Save each node panel as a PDF file?
#'   Default \code{FALSE}.
#' @param path  Path where PDFs are written (ignored if
#'   \code{save.plot = FALSE}). Defaults to the current directory
#'   \code{"."}.
#' @param file.prefix  File name prefix for saved PDFs
#'   (ignored unless \code{save.plot = TRUE}).
#'
#' @return A nested \code{list}. The top level is one entry per depth,
#'   named \code{"Depth_1"}, \code{"Depth_2"}, …; each contains a list
#'   of ggplot objects named \code{"Node_1"}, \code{"Node_2"}, ….
#'
#' @importFrom ggplot2 ggsave theme element_text
#' @importFrom patchwork wrap_plots plot_annotation
#'
#' @export
plot_tree = function(tree, effect,
  color_ice = "lightblue", color_pd = "lightcoral",
  target.feature,
  show.plot = TRUE,
  save.plot = FALSE,
  path = ".", file.prefix = "tree_plot") {

  plot_list = list()
  max_depth = length(tree)

  for (depth in 1:max_depth) {
    reg = regional_pd(effect, tree, depth)
    plots_at_depth = list()

    for (node_num in seq_along(tree[[depth]])) {
      if (!is.null(tree[[depth]][[node_num]])) {

        values_all = unlist(lapply(reg, function(dt) {
          dt_node = dt[dt$node == node_num, ]
          cols = setdiff(names(dt_node), "node")
          unlist(dt_node[, cols, with = FALSE], use.names = FALSE)
        }), use.names = FALSE)

        values_all = values_all[!is.na(values_all)]
        value_min = min(values_all)
        value_max = max(values_all)
        y_range = value_max - value_min
        ymin = value_min
        ymax = value_max + 0.2 * y_range

        node = tree[[depth]][[node_num]]
        n_samples = length(node$subset.idx)
        split_condition = NULL

        if (depth == 1) {
          # title = paste0("Depth ", depth, " - Node ", node_num)
          title = paste0("Root node", " (N = ", n_samples, ")")
        } else {
          parent_node = find_node_by_id(tree[[depth - 1]], node$id.parent)
          op = "?"
          if (!is.null(parent_node)) {
            if (!is.null(parent_node$children[[1]]) &&
              identical(parent_node$children[[1]], node)) {
              op = "≤"
              direction = "Left"
            } else if (!is.null(parent_node$children[[2]]) &&
              identical(parent_node$children[[2]], node)) {
              op = ">"
              direction = "Right"
            }
          }
          #split_condition = paste0(node$split.feature.parent, " ", op, " ", round(node$split.value.parent, 3))
                  # 构建完整 path conditions（从当前往上回溯）
          path_conditions = c()
          current_node = node

          while (!is.null(current_node$id.parent)) {
            parent_node = find_node_by_id(tree[[current_node$depth - 1]], current_node$id.parent)

            if (is.null(parent_node)) break

            is_left = !is.null(parent_node$children[[1]]) && identical(parent_node$children[[1]], current_node)
            op = if (is_left) "≤" else ">"
            cond = paste0(parent_node$split.feature, " ", op, " ", round(parent_node$split.value, 3))
            path_conditions = c(cond, path_conditions)

            current_node = parent_node
          }

          if (length(path_conditions) > 0) {
            split_condition = paste(path_conditions, collapse = " & ")
          } else {
            split_condition = NULL
          }

          title = paste0(depth - 1, ".Split results: ", split_condition, " (N = ", n_samples,")")
        }

        # if (node$improvement.met | node$stop.criterion.met | node$depth == length(tree)) {
        #   title = sub("node", "(leaf) node", title)
        # }

        plots = regional_pd_plot(reg, target.feature, node_num,
          color_ice = color_ice,
          color_pd = color_pd,
          ymin = ymin, ymax = ymax,
          split_condition = split_condition)

        p = patchwork::wrap_plots(plots, ncol = if (length(effect$features) <= 3) length(effect$features) else 2) +
          patchwork::plot_annotation(title = title) &
          theme(plot.title = element_text(hjust = 0.5))

        if (show.plot) print(p)
        if (save.plot) {
          ggsave(filename = file.path(path, paste0(file.prefix, "_d", depth, "_n", node_num, ".pdf")),
            plot = p, width = 8, height = 5)
        }

        plots_at_depth[[paste0("Node_", node_num)]] = p
      }
    }

    plot_list[[paste0("Depth_", depth)]] = plots_at_depth
  }

  return(plot_list)
}

#' Prepare Tree Layout for Visualization
#'
#'@param tree A list of lists representing a tree structure.
#'
#' @return A data.frame with one row per node. Columns include:
#' \describe{
#'   \item{id}{Unique internal ID for the node (e.g., "2_1").}
#'   \item{node.id}{Node identifier from the original tree.}
#'   \item{id.parent}{Node ID of the parent node.}
#'   \item{child.type}{Type of child node ("<=" or ">").}
#'   \item{split.feature}{Feature used to split at this node. `"final"` for leaf nodes.}
#'   \item{split.value}{Threshold value for the split.}
#'   \item{depth}{Depth level of the node (starting from 1).}
#'   \item{index}{Index of the node within its depth level.}
#'   \item{label}{Text label summarizing the split condition (for internal nodes) or full path (for leaf nodes).}
#' }
#'
#' @keywords internal
prepare_tree_layout = function(tree) {
  rows = vector("list", 0)
  k = 1

  for (depth in seq_along(tree)) {
    nodes = tree[[depth]]
    for (i in seq_along(nodes)) {
      node = nodes[[i]]
      if (!is.null(node)) {
        rows[[k]] = list(
          id            = paste0(depth, "_", i),
          node.id       = if (!is.null(node$id)) node$id else NA,
          id.parent     = if (!is.null(node$id.parent)) node$id.parent else NA,
          child.type    = if (!is.null(node$child.type)) node$child.type else NA,
          split.feature = if (!is.null(node$split.feature)) node$split.feature else NA,
          split.value   = if (!is.null(node$split.value)) node$split.value else NA,
          depth         = depth,
          index         = i
        )
        k = k + 1
      }
    }
  }

  layout = do.call(rbind.data.frame, rows)
  rownames(layout) = NULL

  layout$label = NA
  for (i in seq_len(nrow(layout))) {
    path_conditions = c()
    current = layout[i, ]

    while (!is.na(current$id.parent)) {
      parent = layout[layout$node.id == current$id.parent & layout$depth == current$depth - 1, ]
      if (nrow(parent) != 1) break

      op = if (current$child.type == "<=") "≤" else ">"
      cond = paste0(parent$split.feature, " ", op, " ", round(as.numeric(parent$split.value), 3))
      path_conditions = c(cond, path_conditions)

      current = parent
    }

    if (!is.na(layout$split.feature[i]) && layout$split.feature[i] != "final") {
      cond_self = paste0(layout$split.feature[i], " ", " ≤ ", " ", round(as.numeric(layout$split.value[i]), 3))
      path_conditions = cond_self
    } else {
      path_conditions = path_conditions
    }

    layout$label[i] = paste(path_conditions, collapse = " \n& ")
  }

  return(layout)
}


#' Plot Tree Structure with ggraph
#'
#'@param tree A list of lists representing a tree structure.
#'
#' @return A \code{ggplot} object (of class \code{ggraph}) representing the tree layout.
#'   Typically used for visualization purposes.
#'
#' @importFrom igraph graph_from_data_frame
#' @importFrom ggraph ggraph geom_edge_elbow geom_node_label circle
#' @importFrom ggplot2 aes coord_flip scale_fill_brewer theme_void scale_y_reverse
#' @importFrom ggplot2 theme margin unit arrow expansion
#'
#' @export
plot_tree_structure = function(tree) {
  layout = prepare_tree_layout(tree)

  parent_map = setNames(layout$id, layout$node.id)
  layout$parent.id = parent_map[as.character(layout$id.parent)]

  edge_list = na.omit(layout[, c("parent.id", "id")])
  colnames(edge_list) = c("from", "to")

  g = igraph::graph_from_data_frame(edge_list, vertices = layout, directed = TRUE)

  ggraph(g, layout = "tree") +
    coord_flip(clip = "off") +
    geom_edge_elbow(
      arrow = arrow(length = unit(0.05, "cm")),
      end_cap = circle(1.5, "mm"),
      edge_colour = "grey40",
      edge_width = 0.4
    ) +
    geom_node_label(
      aes(label = label, fill = factor(depth)),
      size = 3.5,
      label.padding = unit(0.25, "lines"),
      label.size = 0.3,
      label.r = unit(0.1, "lines")
    ) +
    scale_fill_brewer(palette = "Set2") +
    theme_void() +
    scale_y_reverse(expand = expansion(mult = c(0.01, 0.01))) +
    theme(
      legend.position = "none",
      plot.margin = margin(t = 10, r = 20, b = 10, l = 20, unit = "mm")
    )
}




# prepare_tree_layout = function(tree) {
#   rows = vector("list", 0)
#   k = 1
#
#   for (depth in seq_along(tree)) {
#     nodes = tree[[depth]]
#     for (i in seq_along(nodes)) {
#       node = nodes[[i]]
#       if (!is.null(node)) {
#         label = if (!is.null(node$split.feature)) {
#           paste0(node$split.feature, "≤ ", round(node$split.value, 3))
#         } else {
#           "Leaf node"
#         }
#
#         rows[[k]] = list(
#           id = paste0(depth, "_", i),
#           node.id = node$id,
#           id.parent = if (!is.null(node$id.parent)) node$id.parent else NA,
#           depth = depth,
#           index = i,
#           x_manual = i,
#           y_manual = -depth,
#           label = label
#         )
#         k = k + 1
#       }
#     }
#   }
#
#   layout = do.call(rbind.data.frame, rows)
#   rownames(layout) = NULL
#   return(layout)
# }
