#plotting the significant results to see pairwise differences
library(ggdist)

vineland_sum <- VABS_test %>%
  group_by(msel_cluster_membership) %>%
  summarise(mean = mean(composite_VABS),
            sd = sd(composite_VABS))

vabs_violin <- ggplot(VABS_test, aes(x = msel_cluster_membership,
                      y = composite_VABS)) +
  geom_violin() +
  geom_pointrange(data = vineland_sum,
                  aes(x = msel_cluster_membership,
                      y = mean,
                      ymin = mean-sd,
                      ymax = mean+sd),
                  col = 'red') +
  theme_classic() +
  ggtitle('Compare VABS-II composite score between cognitive subgroups') +
  xlab('cognitive subgroup') +
  ylab('composite score')

ggsave('vabs_violin.png', vabs_violin, width = 8, height = 5)

ADI_sum <- dx_test %>%
  group_by(msel_cluster_membership) %>%
  summarise(soc = mean(social_ADI),
            socsd = sd(social_ADI),
            rrb = mean(rrb_ADI),
            rrbsd = sd(rrb_ADI))

socADI_violin <-
  ggplot(dx_test,
         aes(x = msel_cluster_membership,
             y = social_ADI)) +
  geom_violin(trim = F) +
  geom_pointrange(data = ADI_sum,
                  inherit.aes = F,
                  aes(x = msel_cluster_membership,
                      y = soc,
                      ymin = soc-socsd,
                      ymax = soc+socsd),
                  col = "red") +
  theme_classic() +
  ggtitle('Compare scores for social algorithm total (ADI-R)') +
  xlab('cognitive subgroup') +
  ylab('social algorithm total (ADI-R)')

ggsave('socADI_violin.png', socADI_violin, width = 5, height = 5)


rrbADI_violin <-
  ggplot(dx_test, aes(x = msel_cluster_membership,
                                     y = rrb_ADI)) +
  geom_violin(trim = F) +
  geom_pointrange(data = ADI_sum,
                  inherit.aes = F,
                  aes(x = msel_cluster_membership,
                      y = rrb,
                      ymin = rrb-rrbsd,
                      ymax = rrb+rrbsd),
                  col = 'red') +
  theme_classic() +
  ggtitle('Compare scores for repetitive behaviours (ADI-R)') +
  xlab('cognitive subgroup') +
  ylab('repetitive behaviours algorithm total (ADI-R)')

ggsave('rrbADI_violin.png', rrbADI_violin, width = 5, height = 5)

#trying out raincloud plot
ggplot(dx_test,
       aes(x = msel_cluster_membership, y = social_ADI)) +
  stat_halfeye(adjust = 0.5,
               justification = -0.2,
               .width = 0,
               point_colour = NA) +
  geom_boxplot(width = 0.12) +
  theme_classic() +
  ggtitle('Compare scores for social algorithm total (ADI-R) between cogntivie subgroups') +
  xlab('cognitive subgroup') +
  ylab('social algorithm total (ADI-R)')
