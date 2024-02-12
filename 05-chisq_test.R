#chi square categorical overlap msel eeg clusters
clust_cont_table1 <- clust %>%
  filter(msel_clustlab == -1 | msel_clustlab == 0) %>%
  summarise('eeg-1or0' = sum(eeg_clustlab == -1 | eeg_clustlab == 0),
            eeg1 = sum(eeg_clustlab == 1)) %>%
  `rownames<-`('msel-1or0')

clust_cont_table2 <- clust %>%
  filter(msel_clustlab == 1) %>%
  summarise('eeg-1or0' = sum(eeg_clustlab == -1 | eeg_clustlab == 0),
            eeg1 = sum(eeg_clustlab == 1)) %>%
  `rownames<-`('msel1')

clust_cont_table <- rbind(clust_cont_table1, clust_cont_table2)

x <- chisq.test(clust_cont_table)
x$observed
x

#chi square categorical overlap msel clusters & fl group
fl_tidy <- fl %>%
  rename(subject = ID) %>%
  merge(clust, by='subject', all = T) %>%
  select(subject, ASD, ADHD, msel_clustlab, eeg_clustlab) %>%
  filter(ASD != 'X') %>%
  na.omit()

fl_cont_tab1 <- fl_tidy %>%
  filter(msel_clustlab == -1 | msel_clustlab == 0) %>%
  summarise(autism = sum(ASD == 1 & ADHD == 0),
            adhd = sum(ASD == 0 & ADHD == 1),
            autadhd = sum(ASD == 1 & ADHD == 1),
            typical = sum(ASD == 0 & ADHD == 0)) %>%
  `rownames<-`('msel-1or0')

fl_cont_tab2 <- fl_tidy %>%
  filter(msel_clustlab == 1) %>%
  summarise(autism = sum(ASD == 1 & ADHD == 0),
            adhd = sum(ASD == 0 & ADHD == 1),
            autadhd = sum(ASD == 1 & ADHD == 1),
            typical = sum(ASD == 0 & ADHD == 0)) %>%
  `rownames<-`('msel1')

flxmsel_cont_table <- rbind(fl_cont_tab1, fl_cont_tab2)

x1 <- chisq.test(flxmsel_cont_table)
x1$observed
x1

#chi square categorical overlap eeg clusters & fl group
fl_cont_tab11 <- fl_tidy %>%
  filter(eeg_clustlab == -1 | eeg_clustlab == 0) %>%
  summarise(autism = sum(ASD == 1 & ADHD == 0),
            adhd = sum(ASD == 0 & ADHD == 1),
            autadhd = sum(ASD == 1 & ADHD == 1),
            typical = sum(ASD == 0 & ADHD == 0)) %>%
  `rownames<-`('eeg-1or0')

fl_cont_tab12 <- fl_tidy %>%
  filter(eeg_clustlab == 1) %>%
  summarise(autism = sum(ASD == 1 & ADHD == 0),
            adhd = sum(ASD == 0 & ADHD == 1),
            autadhd = sum(ASD == 1 & ADHD == 1),
            typical = sum(ASD == 0 & ADHD == 0)) %>%
  `rownames<-`('eeg1')

flxeeg_cont_table <- rbind(fl_cont_tab11, fl_cont_tab12)

x2 <- chisq.test(flxeeg_cont_table)
x2$observed
x2

#chisquare msel/eeg vs dx outcome
dx_tidy <- dx_out %>%
  mutate(dx_lab = case_when(aut_out == "1" & ADHD_out == "0" ~ "autism",
                            aut_out == "0" & ADHD_out == "1" ~ "adhd",
                            aut_out == "1" & ADHD_out == "1" ~ "autadhd",
                            aut_out == "0" & ADHD_out == "0" ~ "typical")) %>%
  merge(clust, by = 'subject', all = T) %>%
  na.omit()

dx_cont_tab1 <- dx_tidy %>%
  filter(msel_clustlab == -1 | msel_clustlab == 0) %>%
  summarise(autism = sum(dx_lab == 'autism'),
            adhd = sum(dx_lab == 'adhd'),
            autadhd = sum(dx_lab == 'autadhd'),
            typical = sum(dx_lab == 'typical')) %>%
  `rownames<-`('msel-1or0')

dx_cont_tab2 <- dx_tidy %>%
  filter(msel_clustlab == 1) %>%
  summarise(autism = sum(dx_lab == 'autism'),
            adhd = sum(dx_lab == 'adhd'),
            autadhd = sum(dx_lab == 'autadhd'),
            typical = sum(dx_lab == 'typical')) %>%
  `rownames<-`('msel1')

dxmsel_cont_table <- rbind(dx_cont_tab1, dx_cont_tab2)

x3 <- chisq.test(dxmsel_cont_table)
x3$expected
x3

dx_cont_tab11 <- dx_tidy %>%
  filter(eeg_clustlab == -1 | eeg_clustlab == 0) %>%
  summarise(autism = sum(dx_lab == 'autism'),
            adhd = sum(dx_lab == 'adhd'),
            autadhd = sum(dx_lab == 'autadhd'),
            typical = sum(dx_lab == 'typical')) %>%
  `rownames<-`('eeg-1or0')

dx_cont_tab12 <- dx_tidy %>%
  filter(eeg_clustlab == 1) %>%
  summarise(autism = sum(dx_lab == 'autism'),
            adhd = sum(dx_lab == 'adhd'),
            autadhd = sum(dx_lab == 'autadhd'),
            typical = sum(dx_lab == 'typical')) %>%
  `rownames<-`('eeg1')

dxeeg_cont_table <- rbind(dx_cont_tab11, dx_cont_tab12)

x4 <- chisq.test(dxeeg_cont_table)
x4$expected
x4

