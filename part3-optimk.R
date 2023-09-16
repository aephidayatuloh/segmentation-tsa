library(tidymodels)
library(tidyclust)

abt_cluster <- abt |> 
  select(-MemberID) 
  
pc <- abt_cluster |> 
  prcomp(retx = TRUE, scale. = TRUE, center = TRUE)

pc

pc |> 
  broom::tidy("pcs") |> 
  mutate(eigenvalue = std.dev^2) |> 
  filter(eigenvalue >= 0.7 | cumulative <= 0.95)

pc |> 
  broom::tidy("rotation") |> 
  filter(PC <= 6) |> 
  pivot_wider(id_cols = column, 
              names_from = PC, names_prefix = "PC", 
              values_from = value)

pc |> 
  broom::tidy("loadings") |> 
  filter(PC <= 6) |>
  mutate(#value = if_else(abs(value) < 0.25, 0, value), 
    influence = if_else(value < 0, "negative", "positive"), 
    PC = factor(PC, labels = paste0("PC", PC), levels = PC), 
    column = factor(column, levels = column |> unique() |> sort())
  ) |>
  ggplot(aes(x = value, y = column, fill = influence)) + 
  geom_col() + 
  geom_vline(xintercept = 0) + 
  labs(x = "Loadings", 
       y = "Column") + 
  facet_wrap(~PC, nrow = 1) + 
  theme_light() + 
  theme(legend.position = "none")

cs_recipe <- recipe(~ ., data = abt_cluster) |> 
  step_pca(all_numeric_predictors(), threshold = 0.95, 
           options = list(retx = TRUE, scale. = TRUE, center = TRUE)) |> 
  prep()

kmeans_spec <- k_means(num_clusters = tune()) |> 
  set_engine(engine = "stats", iter.max = 5000, nstart = 25)

kmeans_wf <- workflow(
  preprocessor = cs_recipe, 
  spec = kmeans_spec
  )

set.seed(1001)
bs <- vfold_cv(data = abt_cluster, v = 3)
maxk <- 10
grid_cluster <- tibble(num_clusters = 1:maxk)

bs_sse <- tune_cluster(
  object = kmeans_wf, 
  resamples = bs, 
  grid = grid_cluster, 
  metrics = cluster_metric_set(sse_within_total)
  )

# Script berikut ini membutuhkan waktu proses cukup lama!
bs_silhouette <- tune_cluster(
  object = kmeans_wf, 
  resamples = bs, 
  grid = grid_cluster, 
  metrics = cluster_metric_set(silhouette_avg)
)

gg_sse <- bs_sse |> 
  collect_metrics() |> 
  ggplot(aes(x = num_clusters, y = mean)) + 
  geom_line() + 
  geom_point() + 
  scale_x_continuous(breaks = 1:10) + 
  scale_y_continuous(labels = scales::number_format(big.mark = ",")) + 
  labs(
    x = NULL, 
    y = "SSE Within Total"
  ) + 
  theme_light() + 
  theme(
    panel.grid.minor = element_blank(), 
    axis.text = element_blank(), 
    axis.ticks = element_blank()
    )
gg_sse
gg_silhouette <- bs_silhouette |> 
  collect_metrics() |> 
  ggplot(aes(x = num_clusters, y = mean)) + 
  geom_line() + 
  geom_point() + 
  scale_x_continuous(breaks = 1:10) + 
  scale_y_continuous(labels = scales::number_format(big.mark = ",")) + 
  labs(
    x = "Number of Cluster", 
    y = "Avg Silhouette"
  ) + 
  theme_light() + 
  theme(
    panel.grid.minor = element_blank(), 
    axis.text.y = element_blank(), 
    axis.ticks = element_blank()
    )

library(gridExtra)

grid.arrange(gg_sse, gg_silhouette, ncol = 1)
Sys.time()
