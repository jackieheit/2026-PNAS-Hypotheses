library(tidyverse)


ca <- read_csv(
  "../../minerva/CollectiveAction/DT-CollectiveAction_raw2025.csv") %>%
  mutate(
    OWC = str_trim(OWC, side = "both"),
    # Fellahin was given wrong OWC...should not be MR08 which is the Barabra
    OWC = case_when(
      `SCCS ID` == 43 ~ "MR13",
      TRUE ~ OWC)
  ) %>%
  select(-c(30:44)) %>%
  rename(
    `GA-CAL1A-R` = `GA-CALA-R`
  ) %>%
  mutate(
    across(
      where(is.numeric),
      ~ replace(.x, .x %in% c(88, 99), NA)
    )
  ) %>%
  rename_with(
    ~ str_replace_all(.x, c(
      "CAL1a" = "CAL1A",
      "CAL1b" = "CAL1B",
      "CAL1c" = "CAL1C"
    ))
  )

ca_long <- ca %>%
  pivot_longer(
    cols = matches(
      "^(GA|HU|FI|AG|AH)-CAL1(A|B|C)?-R$"
    ),
    names_to = c("subsistence_domain", "measure"),
    names_pattern = 
      "^(GA|HU|FI|AG|AH)-(CAL1(?:A|B|C)?)-R$",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = measure,
    values_from = value
  )

ca_long <- ca_long %>%
  mutate(
    practiced = case_when(
      CAL1 == 77 ~ 0L,
      is.na(CAL1) ~ NA_integer_,
      TRUE ~ 1L
    ),
    
    CAL1 = na_if(as.numeric(CAL1), 77),
    CAL1A = na_if(as.numeric(CAL1A), 77),
    
    CAL1B = case_when(
      CAL1B == 0 ~ 0,
      CAL1B == 4 ~ 1,
      CAL1B == 3 ~ 2,
      CAL1B == 2 ~ 3,
      CAL1B == 1 ~ 4,
      CAL1B == 77 ~ NA_real_,
      TRUE ~ NA_real_
    ),
    
    CAL1C = case_when(
      CAL1C %in% 1:5 ~ as.numeric(CAL1C),
      TRUE ~ NA_real_
    ),
    
    cooperation_present = case_when(
      practiced == 0 ~ NA_integer_,
      CAL1 == 0 ~ 0L,
      CAL1 %in% c(1, 2) ~ 1L,
      TRUE ~ NA_integer_
    ),
    
    # detailed attributes are necessarily zero when a practiced
    # domain has no cooperative subsistence activity
    CAL1A = case_when(
      practiced == 1 & CAL1 == 0 ~ 0,
      TRUE ~ CAL1A
    ),
    
    CAL1B = case_when(
      practiced == 1 & CAL1 == 0 ~ 0,
      TRUE ~ CAL1B
    )
  )

ea_long <- ca %>%
  select(
    OWC,
    matches("^EA-(GA|HU|FI|AG|AH)$")
  ) %>%
  pivot_longer(
    cols = -OWC,
    names_to = "subsistence_domain",
    names_pattern = "^EA-(GA|HU|FI|AG|AH)$",
    values_to = "EA_dependence"
  )

ca_long <- ca_long %>%
  left_join(
    ea_long,
    by = c("OWC", "subsistence_domain"), 
    relationship = "one-to-one"
  ) %>%
  rename(
    CAL1A_scale_of_sub = CAL1A,
    CAL1B_frequency = CAL1B,
    CAL1C_gender = CAL1C
  )


# OWC discrepancies
# ca_long %>%
#   count(OWC, subsistence, name = "n_ca") %>%
#   filter(n_ca > 1) %>%
#   arrange(desc(n_ca))
# 
# ea_long %>%
#   count(OWC, subsistence, name = "n_ea") %>%
#   filter(n_ea > 1) %>%
#   arrange(desc(n_ea))
# 
# ca %>%
#   filter(OWC == "MR08") %>%
#   select(
#     `SCCS ID`,
#     `SOCIETY NAME`,
#     OWC,
#     starts_with("EA-"),
#     matches("CAL1")
#   )

# Fellahin and Barabra have the same OWC but this is an ERROR
# Fellahin OWC = MR13, this discrepancy will be adjusted above


# quality checks

## one row per society × subsistence domain
ca_long %>%
  count(OWC, subsistence) %>%
  filter(n > 1)

## each society should usually have five subsistence rows
ca_long %>%
  count(OWC) %>%
  count(n)

## check the ranges
ca_long %>%
  summarise(
    CAL1_min = min(CAL1, na.rm = TRUE),
    CAL1_max = max(CAL1, na.rm = TRUE),
    CAL1A_min = min(CAL1A, na.rm = TRUE),
    CAL1A_max = max(CAL1A, na.rm = TRUE),
    CAL1B_min = min(CAL1B, na.rm = TRUE),
    CAL1B_max = max(CAL1B, na.rm = TRUE),
    EA_min = min(EA_dependence, na.rm = TRUE),
    EA_max = max(EA_dependence, na.rm = TRUE)
  )

## Check structural logic
ca_long %>%
  filter(
    practiced == 0 &
      (!is.na(CAL1) | !is.na(CAL1A) | !is.na(CAL1B))
  )

test <- ca_long %>%
  filter(practiced == 1, CAL1 == 0) %>%
  select(
    `SCCS ID`,
    `SOCIETY NAME`,
    subsistence,
    CAL1,
    CAL1A,
    CAL1B
  )

ca_long %>%
  count(practiced, cooperation_present, CAL1)

ca %>%
  pivot_longer(
    cols = matches(
      "^(GA|HU|FI|AG|AH)-CAL1(A|B|C)?-R$"
    ),
    names_to = c("subsistence", "measure"),
    names_pattern =
      "^(GA|HU|FI|AG|AH)-(CAL1(?:A|B|C)?)-R$",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = measure,
    values_from = value
  ) %>%
  filter(CAL1 == 0) %>%
  count(CAL1A, CAL1B, .drop = FALSE)

# When CAL1 = 0, the society practices the subsistence type but does not
# engage in cooperative labor. Although CAL1A and CAL1B are coded 77 in the
# raw data, the codebook specifies that these contingent measures should be
# recoded to 0 in this case rather than treated as structurally inapplicable

write.csv(ca_long, "collective-action-long-format.csv", row.names = FALSE)
