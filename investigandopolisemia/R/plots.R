source(here::here("R","presentations.R"))

# Generate plots ----
sense_empty <- base_plot +
  geom_point(aes(color = significado), size = 3) +
  stylize_list()

sense_examples <- base_plot +
  geom_point(aes(color = significado, alpha = alpha, size = alpha*5)) +
  geom_label_repel(
    data = ejemplos,
    aes(x = model.x, y = model.y,
        label = Plot, segment.color = significado,
        fill = significado),
    xlim = c(25, NA),
    segment.shape = -1, force = 0.1,
    segment.curvature = -0.6,
    color = 'white', label.padding = 0.5) +
  stylize_list(darken_fill = 0.3) +
  scale_fill_OkabeIto(aesthetics = 'segment.color', darken = 0.1, guide = 'none',
                      drop=FALSE) +
  scale_alpha_identity() +
  scale_size_identity()

cluster_examples <- base_plot +
  geom_point(aes(color = cluster, alpha = alpha, size = alpha*5)) +
  geom_label_repel(
    data = ejemplos,
    aes(x = model.x, y = model.y,
        label = Plot, segment.color = cluster,
        fill = cluster),
    xlim = c(25, NA),
    segment.shape = -1, force = 0.1,
    segment.curvature = -0.6,
    color = 'white', label.padding = 0.5) +
  stylize_list() +
  scale_fill_OkabeIto(aesthetics = 'segment.color', darken = 0.1, guide = 'none',
                      drop=FALSE) +
  scale_alpha_identity() +
  scale_size_identity()

cluster_hulls <- base_plot +
  geom_point(aes(color = cluster, alpha = eps), size = 3) +
  geom_mark_hull(
    aes(fill = cluster, filter = !is.na(cluster),
        label = cluster_name),
    label.buffer = unit(0.5, "mm"),
    concavity = 5, alpha = 0.15,
    con.cap = unit(0, "mm")) +
  stylize_list(max_x = max(range_coords)) +
  scale_alpha_continuous(range = c(1, 0.2), guide = 'none')

plotlist <- list(
  'sense-empty' = sense_empty,
  'sense-examples' = sense_examples,
  'cluster-examples'= cluster_examples,
  'cluster-hulls' = cluster_hulls
)
save_plots <- function(plt, fname){
  filename <- paste0('img/', fname, '.png')
  ggsave(plot = plt, filename = filename, bg = "transparent")
  knitr::plot_crop(filename)
}
iwalk(plotlist, save_plots)
