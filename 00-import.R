#import phenotypic data
ADI <- read.csv('data/ADI-R_36m.csv')
ADOS <- read.csv('data/ADOS_36m.csv')
autism_outcome <- read.csv('data/ASD_outcome.csv')
CBCL_ADHD <- read.csv('data/CBCL_36m_ADHD.csv')
CBCL_all <- read.csv('data/CBCL_36m_STAARS.csv')

#import vineland
vineland <- read.csv('data/Vineland_24m.csv')

#import clusters
eeg_clust <- read.csv('data/cluster_groupings/eeg_cluster.csv')
msel_clust <- read.csv('data/cluster_groupings/msel_cluster.csv')

#import familial likelihood
fl <- read.csv('data/familial_likelihood.csv')
