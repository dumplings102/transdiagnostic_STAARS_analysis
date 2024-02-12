#compare scores between subgroups
library(rstatix)

VABS_test <- VABS_tidy %>%
  merge(clust_plot, by='subject', all=T) %>%
  na.omit()

VABS_msel0 <- VABS_test %>%
  filter(msel_cluster_membership == 'outliers') %>%
  select(composite_VABS)

VABS_msel1 <- VABS_test %>%
  filter(msel_cluster_membership == 'main cluster') %>%
  select(composite_VABS)

t = t.test(VABS_msel0, VABS_msel1)
t
mean(VABS_msel0$composite_VABS)
mean(VABS_msel1$composite_VABS)

VABS_eeg0 <- VABS_test %>%
  filter(eeg_cluster_membership == 'outliers') %>%
  select(composite_VABS)

VABS_eeg1 <- VABS_test %>%
  filter(eeg_cluster_membership == 'main cluster') %>%
  select(composite_VABS)

t1 = t.test(VABS_eeg0, VABS_eeg1)
t1
