library(tidyverse)
library(psych)
# install.packages("writexl")
library(writexl)

hz_event <- read_csv("../../minerva/Hazards/Datasets/DT-hz-event-level-clean.csv")

hz_ind <- hz_event %>%
  mutate(
    across(where(is.numeric) & !all_of(c("ID")), ~ na_if(., 99))
  ) %>%
  mutate(
    norm_calc_ind_3   = if_else(H.11. > 5 & H.10. < 3, 1, 0, missing = 0),
    norm_calc_ind_2.5 = if_else(H.11. > 5 & H.10. < 2.5, 1, 0, missing = 0),
    h13_norm_ind      = if_else(H.13. == 2, 1, 0, missing = 0)
  )



phi_data_3 <- table(hz_ind$norm_calc_ind_3, hz_ind$h13_norm_ind)

phi_data_2.5 <- table(hz_ind$norm_calc_ind_2.5, hz_ind$h13_norm_ind)

phi(phi_data_3, digits = 3)

phi(phi_data_2.5, digits = 3)

chisq.test(phi_data)

hz_norm <- hz_ind %>%
  mutate(
    Hz_Category = factor(
      H.13.,
      levels = c(1, 2, 3, 4),
      labels = c(
        "Disaster",
        "Normative hazard",
        "Misc environmental conditions",
        "Mild event"
      )
    )
  )%>%
  rename(
    Hazard_ID  = H.1.,
    Severity   = H.10.,
    Frequency  = H.11.,
  ) 

# Normative frequency/severity (H10 < 3) but not classified as H13 normative
hz_norm_3_calc <- hz_norm %>%
  filter(norm_calc_ind_3 == 1 & h13_norm_ind == 0) %>%
  select(ID, eHRAF.Name, Hazard_ID, Severity, Frequency, Hz_Category)

# Classified as H13 normative but not freq/severity normative (H10 < 3)
hz_norm_3_h13 <- hz_norm %>%
  filter(norm_calc_ind_3 == 0 & h13_norm_ind == 1) %>%
  select(ID, eHRAF.Name, Hazard_ID, Severity, Frequency, Hz_Category)


# Normative frequency/severity (H10 < 3) but not classified as H13 normative
hz_norm_2.5_calc <- hz_norm %>%
  filter(norm_calc_ind_2.5 == 1 & h13_norm_ind == 0) %>%
  select(ID, eHRAF.Name, Hazard_ID, Severity, Frequency, Hz_Category)

# Classified as H13 normative but not freq/severity normative (H10 < 3)
hz_norm_2.5_h13 <- hz_norm %>%
  filter(norm_calc_ind_2.5 == 0 & h13_norm_ind == 1) %>%
  select(ID, eHRAF.Name, Hazard_ID, Severity, Frequency, Hz_Category)


sheets <- list(
  norm_3_calc  = hz_norm_3_calc,
  norm_3_h13   = hz_norm_3_h13,
  norm_2_5_calc = hz_norm_2.5_calc,
  norm_2_5_h13  = hz_norm_2.5_h13
)

write_xlsx(sheets, "hz_norm_comparisons.xlsx")

