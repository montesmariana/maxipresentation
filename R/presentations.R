library(here)
library(tidyverse)
library(colorblindr)
library(ggpubr)
library(ggforce)
library(ggrepel)
horde <- readRDS(here("investigandopolisemia", "data", "data.rds"))$horde$medoidCoords[[1]]

cw_translations <- c(
  'word/verb' = 'volverse',
  'meter/noun' = 'metro',
  'neem/verb' = 'tomar',
  'toerist/noun' = 'turista',
  'fan/noun' = 'fan',
  'journalist/noun' = 'periodista'
)
clusters <- horde$cws %>% 
  group_by(cluster) %>% 
  mutate(topFscore = max(Fscore)) %>% 
  filter(Fscore == topFscore) %>% 
  
  mutate(cluster_name = glue::glue("{cw_translations[cw]} ({round(Fscore, 2)})")) %>% 
  ungroup()
clusternames <- clusters$cluster_name
names(clusternames) <- clusters$cluster
sense_mapping <- c(
  'horde_1' = 'horda',
  'horde_1b' = 'horda_no_humana',
  'horde_2' = 'valla',
  'horde_3' = enc2utf8('obstáculo')
)

df <- horde$coords %>%
  filter(!is.na(ctxt)) %>%
  mutate(
    cluster_name = clusternames[cluster],
    cluster = if_else(cluster == "0", NA_character_, as.character(cluster)),
    significado = factor(sense_mapping[sense], levels = sense_mapping))
range_coords = c(df$model.x, df$model.y)
ejemplos <- jsonlite::read_json(here("investigandopolisemia", "data", "ejemplos.json")) %>% 
  map_df(as_tibble_row) %>% 
  left_join(df, by = c('id' = '_id'))

df <- mutate(df, alpha = if_else(`_id`%in% ejemplos$id, 1, 0.4))


base_plot <- df %>% ggplot(aes(x = model.x, y = model.y))

stylize_list <- function(max_x = max(range_coords)*3, darken_fill = 0.1) {
  list(
    scale_color_OkabeIto(darken = 0.1, use_black = TRUE, na.value = "#9b9c9f",
                         drop=FALSE),
    scale_fill_OkabeIto(darken = darken_fill, use_black = TRUE, na.value = "#9b9c9f", guide = "none",
                        drop=FALSE),
    coord_fixed(
      xlim = c(min(range_coords), max_x) #,
      # ylim = c(min(range_coords), max(range_coords))
    ),
    theme_void(),
    theme(legend.position = "top",
          plot.margin=grid::unit(c(0,0,0,0), "mm"),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 10)),
    labs(x = '', y = ''),
    guides(color = guide_legend(override.aes = list(size = 5), nrow = 1))
  )
  }
# plotCloud <- function(medoid){
#   
#   df %>% ggplot(aes(x = model.x, y = model.y, color = cluster,
#                fill = cluster)) +
    # ggforce::geom_mark_hull(
    #   aes(filter = !is.na(cluster), label = cluster_name),
    #   label.buffer = unit(0.5, "mm"),
    #   concavity = 8,
    #   con.cap = unit(0, "mm")) +
#     geom_point(size = 3) +
#     
# }
# set.seed(4)
# examples <- df %>%
#   filter(!is.na(cluster)) %>%
#   mutate(clussense = paste(sense, cluster)) %>%
#   filter(clussense %in% c("valla 1", "obstáculo 2", "horda 3")) %>%
#   split(f = .$sense) %>% 
#   map(pull, ctxt) %>%
#   map(sample, 1)
# 
# examples
# map(examples, function(x) return(list(id = names(x), Dutch = unname(x)))) %>%
#   rjson::toJSON() %>%
#   jsonlite::prettify() %>%
#   write("data/ejemplos.json")
#   



