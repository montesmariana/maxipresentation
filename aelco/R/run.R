library(tidyverse)
# library(ggforce)
library(ggrepel)
library(here)
source(here('R', 'scales.R'))

d <- readRDS(here('aelco', 'data', 'models.rds'))$schaal$medoidCoords

cluster_order <- d[[5]]$cws %>%
  group_by(cluster) %>%
  filter(Fscore == max(Fscore)) %>%
  ungroup() %>%
  select(cluster, cw) %>%
  mutate(cw = if_else(cluster == "0", NA_character_, cw)) %>%
  deframe() %>%
  sort() %>%
  names()

modelrange <- function(coords) {
  ranges <- c(coords$model.x, coords$model.y)
  return(c(min(ranges), max(ranges)))
}
stylize_list <- function(model = NULL, darken_fill = 0.1, expand_x = c(1, 1),
                         legend_position = "top") {
  xlim_range <- if (is.null(model)) NULL else modelrange(model)*expand_x
  list(
    scale_color_OkabeIto(darken = 0.1, use_black = TRUE, na.value = "#9b9c9f",
                         drop=FALSE),
    scale_fill_OkabeIto(darken = darken_fill, use_black = TRUE, na.value = "#9b9c9f", guide = "none",
                        drop=FALSE),
    coord_fixed(
      xlim = xlim_range
    ),
    theme_void(),
    theme(legend.position = legend_position,
          plot.margin=grid::unit(c(0,0,0,0), "mm"),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 10)),
    labs(x = '', y = ''),
    guides(color = guide_legend(override.aes = list(size = 5), nrow = 1))
  )
}

plotCoords <- function(model, legend_position = "top",
                       highlight_sense = NULL, highlight_cluster = NULL,
                       point_size = 3) {
  clusters <- model$cws %>% 
    group_by(cluster) %>% 
    filter(Fscore == max(Fscore)) %>%
    ungroup() %>% 
    select(cluster, cw) %>% deframe()
  
  df <- model$coords %>%
    mutate(
      eps = if (!is.null(highlight_sense)) {
        sense == highlight_sense
      } else if (!is.null(highlight_cluster)) {
        cluster == highlight_cluster
      } else {
        eps
      },
      cluster_name = clusters[cluster],
      cluster = if_else(cluster == "0", NA_character_, cluster_name)
      )

  alpha_scale <- if (is.null(highlight_sense) & is.null(highlight_cluster)){
    function() scale_alpha(range = c(1, 0), guide = 'none')
  } else {
    function() scale_alpha_discrete(range = c(0.2, 0.9), guide = 'none')
  }
  ggplot(df, aes(x = model.x, y = model.y)) +
    geom_point(aes(color = cluster, alpha = eps), size = point_size) +
    stylize_list(df, legend_position = legend_position) +
    alpha_scale()
}

# 
# # Print plot ----
# plotCoords(d[[5]])
# ggsave(here('aelco', 'img', 'schaal-richter.png'), bg = 'transparent')
# # Print text ----
# 
# extractContext <- function(df){
#   df %>% pull(ctxt) %>%
#     str_remove_all('</?strong>') %>%
#     str_remove_all('<sup>[\\d\\.]+</sup>') %>%
#     str_remove_all('[ ]?<br>[ ]?') %>% 
#     str_remove_all('</?artikel>') %>% 
#     str_replace('</span>', ':::') %>%
#     str_replace('<span class=[^>]+>', ':::') %>%
#     str_split(':::') %>% 
#     map(setNames, c('left', 'target', 'right'))
# }
# 
# m <- d[[5]] 
# 
# m$cws %>% group_by(cluster) %>% filter(Fscore == max(Fscore))
# 
# ctxts <- m$coords %>% 
#   filter(cluster == '1') %>% 
#   extractContext()
# 
# walk(c('left', 'target', 'right'),
#      ~write(map_chr(ctxts, .x), here('aelco', 'data', sprintf('richter_%s.txt', .x))))
# 
# 
# # ctxts_positie <- d$hachelijk[[1]]$coords %>% 
# #   filter(map_lgl(cws, has_element, 'positie/noun')) %>% 
# #   arrange(cluster) %>% 
# #   extractContext()
# # walk(c('left', 'target', 'right'),
# #      ~write(map_chr(ctxts_positie, .x), here('adjectives', 'data', sprintf('positie_%s.txt', .x))))

# # Plot schaal ----
# 
schaal_senses <- c(
  "range" = "schaal_1",
  "ratio" = "schaal_2",
  "magnitude" = "schaal_3",
  "plate" = "schaal_5",
  "plate (weighting)" = "schaal_6"
)
schaal5 <- d[[5]]$coords %>%
  mutate(
    homonym = if_else(
      sense %in% c("schaal_1", "schaal_2", "schaal_3"),
      "scale",
      "dish"
    ),
    code = sense,
    sense = fct_recode(sense, !!!schaal_senses)
  )
schaal_frequencies <- schaal5 %>% 
  count(code) %>% 
  deframe()

schaal_clusters <- schaal5 %>% 
  count(cluster) %>% 
  deframe()
# if (FALSE) {
#   set.seed(7)
#   schaal5 %>% 
#     select(sense, homonym, ctxt, model.x, model.y, cluster) %>% 
#     group_by(sense) %>% 
#     slice_sample(n = 1) %>% 
#     mutate(ctxt = str_remove_all(ctxt, "<sup>[0-9\\.]+</sup>") %>% 
#              str_remove_all("<[^>]+>") %>% 
#              str_replace_all(" ?([\\.,\\):])", "\\1") %>% 
#              str_replace_all("(\\() ", "\\1") %>%
#              str_squish()
#     ) %>% 
#     write_tsv(here("aelco", "data", "schaal_examples.tsv"), quote = "none")
#   # Here some manual cleaning
# }
schaal5_text <- read_tsv(here("aelco", "data", "schaal_examples.tsv"),
                           quote = "", show_col_types = FALSE) %>%
  mutate(
    sense = factor(sense, names(schaal_senses)),
    cluster = factor(cluster, cluster_order),
    ctxt = str_replace_all(ctxt, "\\\\n", "\n"))

schaal5_orig <- schaal5_text %>% 
  select(code, ctxt) %>% 
  deframe()

schaal5_translation <- schaal5_text %>%
  select(code, translation) %>%
  deframe()

schaal5_examples <- schaal5_text %>% 
  mutate(ctxt = str_remove_all(ctxt, "\\*"))

sense_barplot <- function(cl) {
  freq = sprintf("%d tokens", schaal_clusters[[as.character(cl)]])
  schaal5 %>% 
    ggplot() +
    geom_bar(aes(x = sense, fill = cluster == cl)) +
    scale_fill_grey(start = 0.6, end = 0.2, guide = "none") +
    annotate("label", x = 1, y = 160,
             label = freq, size = 6) +
    labs(x = "Sense", y = "Frequency") +
    theme_minimal(base_size = 18)
}

# 
# schaal5_plot <- schaal5 %>% 
#   ggplot(aes(x = model.x, y = model.y, color = sense)) +
#   stylize_list(schaal5, expand_x = c(1, 3)) +
#   scale_alpha_discrete(range = c(0.3, 0.9), guide = "none")
# 
# condition <- function(x) x == "scale"
