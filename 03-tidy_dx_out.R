#tidy diagnostic outcome

dx_out <- autism_outcome %>%
  rename(subject = id,
         aut_out = Aut_NotAut) %>%
  merge(CBCL_ADHD %>%
          select(ID1, CBC_DSM_ADHD_CR_36) %>%
          rename(subject = ID1,
                 ADHD_out = CBC_DSM_ADHD_CR_36)) %>%
  mutate(ADHD_out = na_if(ADHD_out, "#NULL!"),
         ADHD_out = case_when(ADHD_out == "Below" ~ 0,
                              ADHD_out == "Clinical Range" ~ 1,
                              ADHD_out == "Borderline" ~ 1))

write_csv(dx_out, 'data/processed/dx_outcomes.csv')
