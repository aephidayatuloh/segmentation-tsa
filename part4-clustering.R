library(tidymodels)
library(tidyclust)

abt_cluster <- abt |> 
  select(-MemberID) 

pc <- abt_cluster |> 
  prcomp(retx = TRUE, scale. = TRUE, center = TRUE)

optimk <- 3
kmeans_spec <- k_means(num_clusters = optimk) |> 
  set_engine(engine = "stats", iter.max = 5000, nstart = 25)

kmeans_wf <- kmeans_wf |> 
  update_model(kmeans_spec)

set.seed(1001)
kmeans_fit <- kmeans_wf |> 
  fit(data = abt_cluster)

kmeans_fit |> 
  extract_centroids()

kmeans_fit |> 
  extract_cluster_assignment() |> 
  count(.cluster) |> 
  mutate(pct = n/sum(n))

kmeans_fit |> 
  sse_within_total(new_data = abt_cluster)

kmeans_fit |> 
  silhouette_avg(new_data = abt_cluster)

cs_cluster <- abt_cluster |> 
  bind_cols(
    kmeans_fit |> 
      extract_cluster_assignment()
  )

cs_cluster |> 
  count(.cluster) |> 
  mutate(Variable = "member") |> 
  pivot_wider(id_cols = Variable, names_from = .cluster, values_from = n) |> 
  bind_rows(
    cs_cluster |> 
      count(.cluster) |> 
      mutate(Variable = "percent member", 
             pct = n/sum(n)) |> 
      pivot_wider(id_cols = Variable, names_from = .cluster, values_from = pct), 
    cs_cluster |> 
      rownames_to_column("id") |> 
      pivot_longer(cols = -c(id, .cluster), names_to = "Variable", values_to = "Values") |> 
      group_by(.cluster, Variable) |> 
      summarise(
        avg = mean(Values), 
        .groups = "drop"
      ) |> 
      pivot_wider(id_cols = Variable, names_from = .cluster, values_from = avg)
  ) 
