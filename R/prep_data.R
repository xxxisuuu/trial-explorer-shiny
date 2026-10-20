# read the CDISC pilot ADaM xpts (phuse-scripts repo) and save small rds files for the app
# run once: Rscript R/prep_data.R
library(haven)
library(dplyr)

arm_lv <- c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose")

adsl <- read_xpt("data/adsl.xpt") %>%
  filter(SAFFL == "Y") %>%
  mutate(TRT01A = factor(TRT01A, levels = arm_lv),
         AGEGR1 = factor(AGEGR1, levels = c("<65", "65-80", ">80")),
         DCDECOD = tools::toTitleCase(tolower(DCDECOD)),
         RACE = tools::toTitleCase(tolower(RACE))) %>%
  zap_labels() %>% zap_formats()

adae <- read_xpt("data/adae.xpt") %>%
  filter(SAFFL == "Y", TRTEMFL == "Y") %>%      # treatment-emergent only
  mutate(TRTA = factor(TRTA, levels = arm_lv),
         AEDECOD = tools::toTitleCase(tolower(AEDECOD)),
         AEBODSYS = tools::toTitleCase(tolower(AEBODSYS))) %>%
  select(USUBJID, TRTA, AEBODSYS, AEDECOD, AESEV, AESER, AEREL, ASTDY, ADURN) %>%
  zap_labels()

adtte <- read_xpt("data/adtte.xpt") %>%
  filter(PARAMCD == "TTDE") %>%
  mutate(TRTA = factor(TRTA, levels = arm_lv)) %>%
  select(USUBJID, TRTA, AVAL, CNSR, EVNTDESC, SEX, AGEGR1, RACE) %>%
  zap_labels()

# quick checks
stopifnot(all(adae$USUBJID %in% adsl$USUBJID))
cat("adsl", nrow(adsl), "| adae", nrow(adae), "| adtte", nrow(adtte), "\n")

saveRDS(list(adsl = adsl, adae = adae, adtte = adtte), "data/adam.rds")
