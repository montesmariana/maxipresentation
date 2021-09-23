library(tidyverse)
library(colorblindr)
library(ggforce)

d <- readRDS('data/fordefense.rds')

stylize_list <- function(darken_fill = 0.1) {
  list(
    scale_color_OkabeIto(darken = 0.1, use_black = TRUE, na.value = "#9b9c9f",
                         drop=FALSE),
    scale_fill_OkabeIto(darken = darken_fill, use_black = TRUE, na.value = "#9b9c9f", guide = "none",
                        drop=FALSE),
    coord_fixed(),
    theme_void(),
    theme(legend.position = "top",
          plot.margin=grid::unit(c(0,0,0,0), "mm"),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 10)),
    labs(x = '', y = ''),
    guides(color = guide_legend(override.aes = list(size = 5), nrow = 1))
  )
}

models_base <- d$horde$models %>% 
  ggplot(aes(x = model.x, y = model.y,
             color = foc_base))
models_clean <- models_base +
  geom_point(alpha = 0.8, size= 3) +
  stylize_list() +
  scale_color_viridis_d(end = 0.8, option = 'C') +
  theme(legend.position = 'none')
ggsave(filename = 'img/defense/models_clean.png',
       plot = models_clean,
       bg = 'transparent')
models_medoids <- models_base +
  geom_point(aes(shape = `_model` == medoid,
                 size = if_else(`_model` == medoid, 4, 2)), alpha = 0.8) +
  stylize_list() +
  scale_shape(guide = 'none') +
  scale_size_identity() +
  scale_color_viridis_d(end = 0.8, option = 'C')
# ggsave(filename = 'img/models_medoids.png',
#        plot = models_medoids,
#        bg = 'transparent')

models_medoids +
  ggforce::geom_mark_circle(
    data = filter(d$horde$models, `_model` == medoid & medoid %in% names(d$horde)),
    aes(x = model.x, y = model.y, label = medoid)
  )
plotCoords <- function(model, ...) {
  clusters <- model$cws %>% 
    group_by(cluster) %>% 
    filter(Fscore == max(Fscore)) %>%
    ungroup() %>% 
    select(cluster, cw) %>% deframe()
  
  df <- model$coords %>%
    mutate(
      cluster_name = clusters[cluster],
      cluster = if_else(cluster == "0", NA_character_, cluster_name),
      cluster = fct_relevel(cluster, ...)
      )
  
  ggplot(df, aes(x = model.x, y = model.y)) +
    geom_point(aes(color = cluster),
               size = 3, alpha = 0.8) +
    stylize_list()
}
plotHorde <- function(i){
  plotCoords(d$horde[[i]], 'meter/noun', 'neem/verb',
             'toerist/noun', 'journalist/noun') +
    theme(legend.position = 'none')
}
plotHorde(1)
plotHorde(2)
ggsave(filename = 'img/horde1.png',
       plotHorde(1), bg = 'transparent')
ggsave(filename = 'img/horde2.png',
       plotHorde(2), bg = 'transparent')

modelrange <- function(medoid) {
  xrange <- medoid$coords$model.x
  yrange <- medoid$coords$model.y
  return(c(min(xrange, yrange), max(xrange, yrange)))
}
horde_hulls <- plotCoords(d$horde[[1]], 'meter/noun', 'neem/verb',
           'toerist/noun', 'journalist/noun') +
  coord_fixed(ylim = modelrange(d$horde[[1]])) +
  geom_mark_hull(
    aes(fill = cluster, filter = !is.na(cluster),
        label = cluster_name),
    label.buffer = unit(0.5, "mm"),
    concavity = 5, alpha = 0.15,
    con.cap = unit(0, "mm")) +
  theme(legend.position = 'none')
ggsave(filename = 'img/defense/horde-hulls.png',
       horde_hulls, bg = 'transparent')

plotCoords(d$heilzaam[[1]], 'werking/noun')
plotCoords(d$heilzaam[[2]], 'werking/noun')
plotCoords(d$geldig[[1]], 'tot/prep')
plotCoords(d$geldig[[2]], 'tot/prep')
# Interesting groups ----
# first <- d$horde[[1]]$coords %>%
#   select(`_id`, cluster1 = cluster) %>%
#   filter(!cluster1 == '0')
# second <- d$horde[[2]]$coords %>%
#   select(`_id`, cluster2 = cluster) %>%
#   filter(!cluster2 == '0')
# inner_join(first, second) %>% 
#   xtabs(~cluster1 + cluster2, data = .)
# d$horde[[1]]$cws %>% 
#   group_by(cluster) %>% 
#   filter(Fscore == max(Fscore))
# d$horde[[2]]$cws %>% 
#   group_by(cluster) %>% 
#   filter(Fscore == max(Fscore))

# Cluster 2 of model 1, or 1 of model 2: 'meter'
# Cluster 3 of model 1, or 2 of model 2: 'neem'
# Cluster 4 of model 1, or 3 of model 2: 'toerist'
# Cluster 6 of model 1, or 5 of model 2: 'journalist'

ctxt <- d$horde$ctxt %>%
  left_join(first) %>% 
  left_join(second) %>% 
  filter(!is.na(cluster1), !is.na(cluster2)) %>% 
  mutate(
    ctxt = str_replace(ctxt, '([hH]orde[ns]?)', '<<\\1>>'),
    cluster = case_when(
      cluster1 == '2' & cluster2 == '1' ~ 'meter',
      cluster1 == '3' & cluster2 == '2' ~ 'neem',
      cluster1 == '4' & cluster2 == '3' ~ 'toerist',
      cluster1 == '6' & cluster2 %in% c('4', '5') ~ 'journalist+',
      TRUE ~ NA_character_)) %>% 
  filter(!is.na(cluster)) %>% 
  separate(ctxt, into = c('left', 'right'), sep = ' <<') %>% 
  separate(right, into = c('target', 'right'), sep = '>> ?') %>% 
  arrange(cluster, right)
nemen <- ctxt %>% 
  filter(cluster == 'neem')
walk(c('left', 'target', 'right'),
     ~write(nemen[[.x]], sprintf('data/nemen_%s.txt', .x)))

walk(c('left', 'target', 'right'),
     ~write(slice_sample(ctxt, prop = 1)[[.x]], sprintf('data/all_%s.txt', .x)))

