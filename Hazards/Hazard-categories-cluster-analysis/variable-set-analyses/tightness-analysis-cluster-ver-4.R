library(tidyverse)

tl <- read_csv("../../minerva/Tight-Loose/Datasets/DT-TL-FinalData.csv") %>%
  select(OWC, General_TL5B_pref_final) %>%
  mutate(
    OWC = str_trim(OWC, side = c("both")),
    
  )

cluster <- read_csv("results/cluster-analysis-ver-3-results.csv")

ds <- inner_join(cluster, tl, by = c("OWC")) %>%
  mutate(
    cluster = factor(cluster)
  ) %>%
  drop_na(General_TL5B_pref_final, cluster)

anova <- aov( ~ cluster, data = ds)

summary(anova)