#tidy vineland data

VABS_tidy <- vineland %>%
  rename(subject = X,
         comm_VABS = COM_STD_SCORE,
         dls_VABS = DLS_STD_SCORE,
         soc_VABS = SOC_STD_SCORE,
         mot_VABS = MS_STD_SCORE,
         composite_VABS = ABC_STD_SCORE)

write_csv(VABS_tidy, 'data/processed/bhv_scores.csv')
