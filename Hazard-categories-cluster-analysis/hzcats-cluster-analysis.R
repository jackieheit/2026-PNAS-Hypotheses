library(tidyverse)

# for k means clustering
library(factoextra)
library(cluster)

hz <- read_csv("../../minerva/Hazards/Datasets/DT-hzcats-society-level.csv")

vars <- c(
  "allhazards_est_count_30",
  "disaster_est_count_30",
  # "normative_est_count_30",
  "allhazards_H9a_sev_exposure_30",
  "allhazards_sev_exposure_30"
)
IA_vars <- c(
  "allhazards_est_count_30_IA",
  "disaster_est_count_30_IA",
  "normative_est_count_30_IA",
  "allhazards_H9a_sev_exposure_30_IA",
  "allhazards_sev_exposure_30_IA"
)

hz_scaled <- hz %>%
  select(all_of(vars)) %>%
  drop_na() %>%
  mutate(across(everything(), ~ as.numeric(scale(.))))


IA_scaled <- hz %>%
  drop_na(all_of(IA_vars)) %>%
  mutate(across(all_of(IA_vars), ~ as.numeric(scale(.))))

fviz_nbclust(hz_scaled, kmeans, method = "wss")

kmeans_pre <- kmeans(
  hz_scaled,
  centers = 3,
  nstart = 20
)

print(kmeans_pre)

fviz_cluster(kmeans_pre, data = hz_scaled)



# 2 cases look like outliers
hz_no_outliers <- hz_scaled %>%
  mutate(cluster = kmeans_pre$cluster) %>%
  filter(cluster != 3) %>%
  select(-cluster)

fviz_nbclust(hz_no_outliers, kmeans, method = "wss")

kmeans_no <- kmeans(
  hz_no_outliers,
  centers = 3,
  nstart = 20
)

print(kmeans_no)

fviz_cluster(kmeans_no, data = hz_no_outliers)



# PCA to validate clustering results
vars_no_PCA <- prcomp(hz_no_outliers, center = FALSE, scale. = FALSE)
vars_no_PCA
