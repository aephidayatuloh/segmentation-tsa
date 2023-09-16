library(tidyverse)
library(scales)

abt <- read_csv("data/customer_segmentation.csv")
abt |> 
  ggplot(aes(x = NoChild)) + 
  geom_bar() + 
  scale_x_continuous(breaks = seq(1, 10, by = 1)) + 
  scale_y_continuous(breaks = seq(0, 22000, by = 5000), 
                     labels = number_format(big.mark = ",")) +
  labs(x = "No of Child") + 
  theme_bw() + 
  theme(
    panel.grid.minor = element_blank(), 
    panel.grid.major.x = element_blank()
    )

x_mean <- mean(abt$tenure_months, na.rm = TRUE)
abt |> 
  ggplot(aes(x = tenure_months)) + 
  geom_density(fill = "skyblue", alpha = 0.75) +
  geom_boxplot(width = 0.005) + 
  geom_point(x = x_mean, y = 0, 
             inherit.aes = FALSE, color = "red") + 
  scale_x_continuous(breaks = seq(0, 150, by = 10)) + 
  labs(x = "Tenure (month)") + 
  theme_bw() + 
  theme(panel.grid.minor = element_blank())

x_mean <- mean(abt$youngest_kid_age_join, na.rm = TRUE)
abt |> 
  ggplot(aes(x = youngest_kid_age_join)) + 
  geom_density(fill = "skyblue", alpha = 0.75) +
  geom_boxplot(width = 0.1) + 
  geom_point(x = x_mean, y = 0,
             inherit.aes = FALSE, color = "red") +
  # scale_x_continuous(breaks = seq(0, 150, by = 10)) + 
  labs(x = "Youngest Kid Age when Join (year)") + 
  theme_bw() + 
  theme(panel.grid.minor = element_blank())


x_mean <- mean(abt$recency, na.rm = TRUE)
abt |> 
  ggplot(aes(x = recency)) + 
  geom_density(fill = "skyblue", alpha = 0.75) +
  geom_boxplot(width = 0.005) + 
  geom_point(x = x_mean, y = 0,
             inherit.aes = FALSE, color = "red") +
  scale_x_continuous(breaks = seq(0, 100, by = 5)) +
  labs(x = "Recency (day)") + 
  theme_bw() + 
  theme(panel.grid.minor = element_blank())

x_mean <- mean(abt$avg_monthly_frequency, na.rm = TRUE)
abt |> 
  ggplot(aes(x = avg_monthly_frequency)) + 
  geom_density(fill = "skyblue", alpha = 0.75) +
  geom_boxplot(width = 0.05) + 
  geom_point(x = x_mean, y = 0,
             inherit.aes = FALSE, color = "red") +
  scale_x_continuous(breaks = seq(0, 50, by = 5)) +
  labs(x = "Avg Monthly Visit") + 
  theme_bw() + 
  theme(panel.grid.minor = element_blank())

x_mean <- mean(abt$avg_monetary, na.rm = TRUE)
abt |> 
  ggplot(aes(x = avg_monetary)) + 
  geom_density(fill = "skyblue", alpha = 0.75) +
  geom_boxplot(width = 0.0005) + 
  geom_point(aes(x = x_mean, y = 0),
             inherit.aes = FALSE, color = "red") +
  scale_x_continuous(
    breaks = seq(0, 10000, by = 1000), 
    labels = number_format(big.mark = ",")
    ) +
  labs(x = "Avg Monthly Monetary") + 
  theme_bw() + 
  theme(panel.grid.minor = element_blank())

x_mean <- mean(abt$avg_interpurchase, na.rm = TRUE)
abt |> 
  ggplot(aes(x = avg_interpurchase)) + 
  geom_density(fill = "skyblue", alpha = 0.75) +
  geom_boxplot(width = 0.005) + 
  geom_point(x = x_mean, y = 0,
             inherit.aes = FALSE, color = "red") +
  scale_x_continuous(breaks = seq(0, 200, by = 10)) +
  labs(x = "Avg Interpurchase") + 
  theme_bw() + 
  theme(panel.grid.minor = element_blank())

x_mean <- mean(abt$freq_last3mo, na.rm = TRUE)
abt |> 
  ggplot(aes(x = freq_last3mo)) + 
  geom_density(fill = "skyblue", alpha = 0.75) +
  geom_boxplot(width = 0.01) + 
  geom_point(x = x_mean, y = 0,
             inherit.aes = FALSE, color = "red") +
  scale_x_continuous(breaks = seq(0, 200, by = 10)) +
  labs(x = "Freq Last 3 Months") + 
  theme_bw() + 
  theme(panel.grid.minor = element_blank())

library(ggcorrplot)

pmat <- abt |> 
  select(-MemberID) |>
  cor_pmat()

abt |> 
  select(-MemberID) |>
  cor() |> 
  ggcorrplot(colors = c("firebrick", "white", "green"), 
             hc.order = TRUE)

abt |> 
  select(-MemberID) |>
  cor() |> 
  ggcorrplot(colors = c("firebrick", "white", "green"), 
             hc.order = TRUE, p.mat = pmat, 
             lab = TRUE, lab_size = 4, digits = 1, 
             ggtheme = theme_bw())
