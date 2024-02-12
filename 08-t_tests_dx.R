dx_test <- dx_trait_df %>%
  merge(clust_plot, by='subject', all=T) %>%
  na.omit() %>%
  mutate(social_ADOS = as.integer(social_ADOS),
         rrb_ADOS = as.integer(rrb_ADOS),
         extbhv_CBCL = as.integer(extbhv_CBCL))

ADI_social1 <- dx_test %>%
  filter(msel_cluster_membership == 'outliers') %>%
  select(social_ADI)

ADI_social2 <- dx_test %>%
  filter(msel_cluster_membership == 'main cluster') %>%
  select(social_ADI)

t_ADIsoc = t.test(ADI_social1, ADI_social2)
t_ADIsoc

ADOS_social1 <- dx_test %>%
  filter(msel_cluster_membership == 'outliers') %>%
  select(social_ADOS)

ADOS_social2 <- dx_test %>%
  filter(msel_cluster_membership == 'main cluster') %>%
  select(social_ADOS)

t_ADOSsoc = t.test(ADOS_social1, ADOS_social2)
t_ADOSsoc

ADI_rrb1 <- dx_test %>%
  filter(msel_cluster_membership == 'outliers') %>%
  select(rrb_ADI)

ADI_rrb2 <- dx_test %>%
  filter(msel_cluster_membership == 'main cluster') %>%
  select(rrb_ADI)

t_ADIrrb = t.test(ADI_rrb1, ADI_rrb2)
t_ADIrrb

ADOS_rrb1 <- dx_test %>%
  filter(msel_cluster_membership == 'outliers') %>%
  select(rrb_ADOS)

ADOS_rrb2 <- dx_test %>%
  filter(msel_cluster_membership == 'main cluster') %>%
  select(rrb_ADOS)

t_ADOSrrb = t.test(ADOS_rrb1, ADOS_rrb2)
t_ADOSrrb

CBCL_extbhv1 <- dx_test %>%
  filter(msel_cluster_membership == 'outliers') %>%
  select(extbhv_CBCL)

CBCL_extbhv2 <- dx_test %>%
  filter(msel_cluster_membership == 'main cluster') %>%
  select(extbhv_CBCL)

t_CBCLextbhv = t.test(CBCL_extbhv1, CBCL_extbhv2)
t_CBCLextbhv

###neural clusters###
ADOS_soc11 <- dx_test %>%
  filter(eeg_cluster_membership == 'outliers') %>%
  select(social_ADOS)

ADOS_soc12 <- dx_test %>%
  filter(eeg_cluster_membership == 'main cluster') %>%
  select(social_ADOS)

t_ADOSsoc1 = t.test(ADOS_soc11, ADOS_soc12)
t_ADOSsoc1

ADOS_rrb11 <- dx_test %>%
  filter(eeg_cluster_membership == 'outliers') %>%
  select(rrb_ADOS)

ADOS_rrb12 <- dx_test %>%
  filter(eeg_cluster_membership == 'main cluster') %>%
  select(rrb_ADOS)

t_ADOSrrb2 = t.test(ADOS_rrb11, ADOS_rrb12)
t_ADOSrrb2

CBCL_extbhv11 <- dx_test %>%
  filter(eeg_cluster_membership == 'outliers') %>%
  select(extbhv_CBCL)

CBCL_extbhv12 <- dx_test %>%
  filter(eeg_cluster_membership == 'main cluster') %>%
  select(extbhv_CBCL)

t_CBCLextbhv1 = t.test(CBCL_extbhv11, CBCL_extbhv12)
t_CBCLextbhv1

ADI_social11 <- dx_test %>%
  filter(eeg_cluster_membership == 'outliers') %>%
  select(social_ADI)

ADI_social12 <- dx_test %>%
  filter(eeg_cluster_membership == 'main cluster') %>%
  select(social_ADI)

t_ADIsoc1 = t.test(ADI_social11, ADI_social12)
t_ADIsoc1

ADI_rrb11 <- dx_test %>%
  filter(eeg_cluster_membership == 'outliers') %>%
  select(rrb_ADI)

ADI_rrb12 <- dx_test %>%
  filter(eeg_cluster_membership == 'main cluster') %>%
  select(rrb_ADI)

t_ADIrrb1 = t.test(ADI_rrb11, ADI_rrb12)
t_ADIrrb1

