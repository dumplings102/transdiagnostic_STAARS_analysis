library(tidyverse)

ADI_tidy <- ADI %>%
  slice(-1) %>%
  select(ID, ADI_SOC_36, ADI_RRSB_36) %>%
  rename(subject = ID,
         social_ADI = ADI_SOC_36,
         rrb_ADI = ADI_RRSB_36)

ADOS_tidy <- ADOS %>%
  select(-ADOS2_CSS_36) %>%
  rename(subject = ID,
         social_ADOS = ADOS2_SA_CSS_36,
         rrb_ADOS = ADOS2_RRB_CSS_36) %>%
  mutate(across(where(is.character), ~na_if(., "#NULL!")))

CBCL_tidy <- CBCL_all %>%
  select(ID1, CBC_Extern_T_36) %>%
  rename(subject = ID1,
         extbhv_CBCL = CBC_Extern_T_36) %>%
  mutate(extbhv_CBCL = na_if(extbhv_CBCL, "#NULL!"))

dx_trait_df <- merge(ADI_tidy, ADOS_tidy) %>%
  merge(CBCL_tidy)

write_csv(dx_trait_df, 'data/processed/dx_scores.csv')
