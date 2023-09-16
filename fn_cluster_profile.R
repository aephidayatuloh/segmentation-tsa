library(gridExtra)
library(ggridges)

cluster_profiling <- function(.data, .variable, .cluster = .cluster, 
                              discrete = FALSE, pop.box.width = 0.05, 
                              cluster.box.width = 0.1){
  .data <- .data |> 
    select({{.variable}}, {{.cluster}})
  # names(.data) <- c("Variable", "Cluster")
  
  if(discrete == FALSE){
    grid.arrange(
      ggplot(data = .data, aes(x = {{.variable}})) + 
        geom_density(
          fill = "skyblue", 
          color = "grey", 
          alpha = 0.5
        ) + 
        geom_boxplot(fill = "skyblue", alpha = 0.5, width = pop.box.width) + 
        labs(y = "Population") + 
        theme_light() + 
        theme(
          panel.grid = element_blank(), 
          axis.text.y = element_blank(), 
          axis.text.x = element_blank(), 
          axis.title.x = element_blank(), 
          axis.ticks.y = element_blank()
        ),
      ggplot(data = .data, 
             aes(x = {{.variable}}, y = {{.cluster}}, fill = {{.cluster}})) + 
        geom_density_ridges(
          color = "grey", 
          alpha = 0.5
        ) + 
        geom_boxplot(width = cluster.box.width, alpha = 0.5) + 
        stat_summary(fun = mean, color = "red") + 
        labs(y = "Cluster") +  
        xlab(vars({{.variable}})) + 
        theme_light() + 
        theme(
          axis.text.y = element_blank(), 
          axis.ticks.y = element_blank(), 
          legend.position = "top"
        ), 
      ncol = 1, 
      heights = c(0.3, 0.7)
    )
  } else {
    
    grid.arrange(
      ggplot(data = .data, aes(x = {{.variable}})) + 
        geom_bar(
          fill = "skyblue", 
          color = "grey", 
          alpha = 0.5
        ) + 
        scale_x_continuous(breaks = 1:10) + 
        labs(y = "Population") + 
        theme_light() + 
        theme(
          panel.grid = element_blank(), 
          axis.text.y = element_blank(), 
          axis.title.x = element_blank(), 
          axis.ticks.y = element_blank()
        ),
      .data |> 
        count({{.cluster}}, {{.variable}}) |> 
        mutate(pct = n/sum(n)) |> 
        mutate({{.variable}} := factor({{.variable}})) |> 
        ggplot(aes(x = pct, y = {{.cluster}}, fill = {{.variable}})) + 
        geom_col(
          position = position_fill(reverse = TRUE), 
          color = "grey", 
          alpha = 0.75
        ) + 
        scale_fill_brewer(type = "qual") +
        labs(x = "Proportion", 
              y = "Cluster") +  
        theme_light() + 
        theme(
          panel.grid.major.x = element_blank(), 
          axis.text.y = element_text(hjust = 1.0), 
          axis.ticks.y = element_blank(), 
          legend.position = "top"
        ), 
      ncol = 1, 
      heights = c(0.3, 0.7)
    )
  }
  
}
# 
# cs_cluster |> 
#   ggplot(aes(x = NoChild)) + 
#   geom_bar(
#     fill = "skyblue", 
#     color = "grey", 
#     alpha = 0.5
#   ) + 
#   scale_x_continuous(breaks = 1:10) + 
#   labs(y = "Population") + 
#   theme_light() + 
#   theme(
#     panel.grid = element_blank(), 
#     axis.text.y = element_blank(), 
#     # axis.text.x = element_blank(), 
#     axis.title.x = element_blank(), 
#     axis.ticks.y = element_blank()
#   )
# 
# cs_cluster |> 
#   count(.cluster, NoChild) |> 
#   mutate(pct = n/sum(n)) |> 
#   ggplot(aes(x = pct, y = .cluster, fill = factor(NoChild))) + 
#   geom_col(
#     position = position_fill(reverse = TRUE), 
#     color = "grey", 
#     alpha = 0.75
#   ) + 
#   # geom_boxplot(width = cluster.box.width, alpha = 0.75) + 
#   # stat_summary(fun = mean, color = "red") + 
#   scale_x_continuous(labels = percent_format()) + 
#   scale_fill_brewer(type = "qual") + 
#   labs(x = "Proportion", 
#        y = "Cluster") +  
#   theme_light() + 
#   theme(
#     panel.grid.major.x = element_blank(), 
#     axis.text.y = element_text(hjust = 1.0), 
#     axis.ticks.y = element_blank(), 
#     legend.position = "top"
#   )
