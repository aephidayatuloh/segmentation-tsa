source("R/fn_cluster_profile.R")

cs_cluster |> 
  cluster_profiling(
    .variable = NoChild, 
    .cluster = .cluster, 
    discrete = TRUE, 
    pop.box.width = 0.005, 
    cluster.box.width = 0.2
    )

cs_cluster |> 
  cluster_profiling(
    .variable = avg_monetary, 
    .cluster = .cluster, 
    discrete = FALSE, 
    pop.box.width = 0.0005, 
    cluster.box.width = 0.2
    )

cs_cluster |> 
  cluster_profiling(
    .variable = avg_monthly_frequency, 
    .cluster = .cluster, 
    discrete = FALSE, 
    pop.box.width = 0.3, 
    cluster.box.width = 0.2
    )

cs_cluster |> 
  cluster_profiling(
    .variable = avg_interpurchase,
    pop.box.width = 0.005, 
    cluster.box.width = 0.2
    )

cs_cluster |> 
  cluster_profiling(
    freq_last3mo, 
    pop.box.width = 0.01, 
    cluster.box.width = 0.2
    )

cs_cluster |> 
  cluster_profiling(
    tenure_months, 
    pop.box.width = 0.005, 
    cluster.box.width = 0.2
    )

cs_cluster |> 
  cluster_profiling(
    avg_consumption, 
    pop.box.width = 0.005, 
    cluster.box.width = 0.2
    )

library(rpart)
library(rpart.plot)

dtree <- cs_cluster |> 
  rpart(formula = .cluster ~ ., 
        data = _, 
        control = rpart.control(
          maxdepth = 4, 
          minsplit = 1
          )
        )

dtree |> 
  rpart.plot()

dtree |> 
  rpart.plot(
    type = 4, 
    extra = 104, 
    tweak = 1.2, 
    under = TRUE, 
    box.palette = "auto", 
    fallen.leaves = TRUE, 
    legend.cex = 1
    )

