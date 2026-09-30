library(tidyverse)


tl_josh <- read.csv("../Code and Data/overall_code.csv") #this file has the overall tightness scores
tl_long <- read.csv("data/tl1_long.csv")

test_not_in_josh_sample <- anti_join(tl1_long, tl_josh, by = c("minerva_id" = "id")) %>%
  distinct(minerva_id) %>%
  pull(minerva_id)

write.csv(test_not_in_josh_sample, "sample_data/minerva_not_in_josh_sample.csv")

test_not_in_minerva_sample <- anti_join(tl_josh, tl1_long, by = c("id" = "minerva_id")) %>%
  distinct(id) %>%
  pull(id)

write.csv(test_not_in_minerva_sample, "sample_data/joshcases_not_in_minerva_sample.csv")
