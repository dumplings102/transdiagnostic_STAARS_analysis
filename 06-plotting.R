library(ggalluvial)

clust_plot <- clust %>%
  mutate(msel_cluster_membership = case_when(msel_clustlab == -1 ~ 'outliers',
                                             msel_clustlab == 0 ~ 'outliers',
                                             msel_clustlab == 1 ~ 'main cluster'),
         eeg_cluster_membership = case_when(eeg_clustlab == -1 ~ 'outliers',
                                            eeg_clustlab == 0 ~ 'outliers',
                                            eeg_clustlab == 1 ~ 'main cluster')) %>%
  mutate(msel_cluster_membership = as.factor(msel_cluster_membership),
         eeg_cluster_membership = as.factor(eeg_cluster_membership))


ggplot(data = clust_plot,
       aes(axis1 = msel_cluster_membership, axis2 = eeg_cluster_membership)) +
  scale_x_discrete(limits = c("msel cluster", "eeg cluster")) +
  ylab('Number of subjects') +
  ggtitle('Alluvial Plot of Cognitive vs EEG Cluster Membership') +
  geom_alluvium(col = 'steelblue') +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  theme_classic()

dx_tidy_plot <- dx_tidy %>%
  mutate(msel_cluster_membership = case_when(msel_clustlab == -1 ~ 'outliers',
                                             msel_clustlab == 0 ~ 'outliers',
                                             msel_clustlab == 1 ~ 'main cluster'),
         eeg_cluster_membership = case_when(eeg_clustlab == -1 ~ 'outliers',
                                            eeg_clustlab == 0 ~ 'outliers',
                                            eeg_clustlab == 1 ~ 'main cluster')) %>%
  mutate(msel_cluster_membership = as.factor(msel_cluster_membership),
         eeg_cluster_membership = as.factor(eeg_cluster_membership),
         dx_lab = as.factor(dx_lab))


ggplot(data = dx_tidy_plot,
       aes(axis1 = msel_cluster_membership, axis2 = dx_lab)) +
  scale_x_discrete(limits = c("msel cluster", "Dx outcome")) +
  ylab('Number of subjects') +
  ggtitle('Alluvial Plot of Cognitive Profile and Diagnostic Outcome') +
  geom_alluvium(aes(col = msel_cluster_membership), show.legend = F) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  theme_classic()

fl_tidy_plot <- fl_tidy %>%
  mutate(msel_cluster_membership = case_when(msel_clustlab == -1 ~ 'outliers',
                                             msel_clustlab == 0 ~ 'outliers',
                                             msel_clustlab == 1 ~ 'main cluster'),
         eeg_cluster_membership = case_when(eeg_clustlab == -1 ~ 'outliers',
                                            eeg_clustlab == 0 ~ 'outliers',
                                            eeg_clustlab == 1 ~ 'main cluster')) %>%
  mutate(fl_lab = case_when(ASD == "1" & ADHD == "0" ~ "autism",
                            ASD == "0" & ADHD == "1" ~ "adhd",
                            ASD == "1" & ADHD == "1" ~ "autadhd",
                            ASD == "0" & ADHD == "0" ~ "typical")) %>%
  mutate(msel_cluster_membership = as.factor(msel_cluster_membership),
         eeg_cluster_membership = as.factor(eeg_cluster_membership),
         fl_lab = as.factor(fl_lab))

ggplot(data = fl_tidy_plot,
       aes(axis1 = msel_cluster_membership, axis2 = fl_lab)) +
  scale_x_discrete(limits = c("msel cluster", "likelihood")) +
  ylab('Number of subjects') +
  ggtitle('Alluvial Plot of Cognitive Profile and Familial Likelihood') +
  geom_alluvium(aes(col = msel_cluster_membership), show.legend = F) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  theme_classic()

ggplot(data = fl_tidy_plot,
       aes(axis1 = eeg_cluster_membership, axis2 = fl_lab)) +
  scale_x_discrete(limits = c("eeg cluster", "likelihood")) +
  ylab('Number of subjects') +
  ggtitle('Alluvial Plot of EEG Profile and Familial Likelihood') +
  geom_alluvium(aes(col = eeg_cluster_membership), show.legend = F) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  theme_classic()
