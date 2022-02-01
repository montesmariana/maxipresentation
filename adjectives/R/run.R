library(tidyverse)
library(ggforce)
library(here)
source(here('R', 'scales.R'))

d <- readRDS(here('adjectives', 'data', 'models.rds'))

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
    geom_point(aes(color = cluster, alpha = eps),
               size = 3) +
    stylize_list() +
    scale_alpha(range = c(1, 0), guide = 'none')
}
modelrange <- function(medoid) {
  ranges <- c(medoid$coords$model.x, medoid$coords$model.y)
  return(c(min(ranges), max(ranges)))
}

# Print plot ----
plotCoords(d$hachelijk[[1]])
ggsave(here('adjectives', 'img', 'hachelijk.png'),
       bg = 'transparent')
# Print text ----

extractContext <- function(df){
  df %>% pull(ctxt) %>%
    str_remove_all('</?strong>') %>%
    str_remove_all('<sup>[\\d\\.]+</sup>') %>%
    str_remove_all('[ ]?<br>[ ]?') %>% 
    str_remove_all('</?artikel>') %>% 
    str_replace('</span>', ':::') %>%
    str_replace('<span class=\"\"target\"\">', ':::') %>%
    str_split(':::') %>% 
    map(setNames, c('left', 'target', 'right'))
}
ctxts_5 <- d$hachelijk[[1]]$coords %>%
  filter(cluster == '5') %>%
  extractContext()


walk(c('left', 'target', 'right'),
     ~write(map_chr(ctxts_5, .x), here('adjectives', 'data', sprintf('onderneming_%s.txt', .x))))

ctxts_positie <- d$hachelijk[[1]]$coords %>% 
  filter(map_lgl(cws, has_element, 'positie/noun')) %>% 
  arrange(cluster) %>% 
  extractContext()
walk(c('left', 'target', 'right'),
     ~write(map_chr(ctxts_positie, .x), here('adjectives', 'data', sprintf('positie_%s.txt', .x))))
