# this analysis uses results from iteration 3 of the cluster analysis

library(tidyverse)

sh <- read_csv("../../2026-Sharing-Analyses/sharing-dataset-for-analysis.csv") %>%
  select(ID, S3_Resolved_IA_TR, S2_Resolved_IA_TR)

cluster <- read_csv("results/cluster-analysis-ver-3-results.csv")

ds <- inner_join(cluster, sh, by = c("ID")) %>%
  mutate(
    cluster = factor(cluster)
  )

ds_seasonal <- ds %>%
  drop_na(S3_Resolved_IA_TR, cluster)

ds_daily <- ds %>%
  drop_na(S2_Resolved_IA_TR, cluster)

anova_seasonal <- aov(S3_Resolved_IA_TR ~ cluster, data = ds_seasonal)
summary(anova_seasonal)


anova_daily <- aov(S2_Resolved_IA_TR ~ cluster, data = ds_daily)
summary(anova_daily)
