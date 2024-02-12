#compare cluster membership

eeg_clust <- rename(eeg_clust, eeg_clustlab = cluster_lab)
msel_clust <- rename(msel_clust, msel_clustlab = cluster_lab)


clust <- merge(eeg_clust, msel_clust, by = 'subject', all = T) %>%
  select(subject, eeg_clustlab, msel_clustlab) %>%
  na.omit()

clust_filtered <- clust %>%
  filter(eeg_clustlab != -1) %>%
  filter(msel_clustlab != -1)


