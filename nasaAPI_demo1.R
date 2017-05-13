library(dplyr)
library(jsonlite)
library(tidyr)
library(tidyjson)


url = "http://api.us.socrata.com/api/catalog/v1?q=nasa&domains=data.nasa.gov&offset=0&limit=10000"
metadata <- fromJSON(url)
## return results; resultSetSize and timing
## combination of data.frame and list

##nasa_json = fromJSON("http://api.us.socrata.com/api/catalog/v1?q=nasa&domains=data.nasa.gov&offset=0&limit=500")

nasa_api <- data.frame(id =  metadata$results$resource$id, 
                         title = metadata$results$resource$name,
                         description = metadata$results$resource$description,
                         download_count = metadata$results$resource$download_count,
                         domain_category = metadata$results$classification$domain_category,
                         link = metadata$results$link,
                         permlink = metadata$results$permalink,
                         category = metadata$results$classification)

nasa_api


##make matrix - prepare data for graph visualisation
##x = spread(count(tags, id, category.domain_tags), category.domain_tags, n, fill = 0)

nasa_cat = nasa_api %>%
  unnest(category.categories) %>%
  select(id, category.categories)

nasa_domain_cat = nasa_api %>% unnest(category.domain_category)  %>%
  select(id, category.domain_category)

nasa_tags = nasa_api %>% unnest(category.domain_tags) %>%
  select(id, category.domain_tags)

library(widyr)
tags_pairs <- nasa_tags %>% 
  pairwise_count(category.domain_tags, id, sort = TRUE, upper = FALSE)

cat_pairs = nasa_cat %>% 
  pairwise_count(category.categories, id, sort = TRUE, upper = FALSE)

library(ggplot2)
library(igraph)
library(ggraph)

set.seed(1234)
tags_pairs %>%
  filter(n >= 250) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void()


cat_pairs %>%
  filter(n >= 50) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void()


#save.image(file="NASA.RData")


