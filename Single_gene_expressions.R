# Compare expression of single genes between samples
# E. Lamont 6/18/24

# Mostly want to see how Rv0494, Rv1473A, Rv0827c compare between conditions

source("Import_data.R") # to get my_tpm and my_metadata

# Plot basics
my_plot_themes <- theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "right",legend.text=element_text(size=10),
        legend.title = element_text(size = 10),
        plot.title = element_text(size=10), 
        axis.title.x = element_text(size=10), 
        axis.text.x = element_text(angle = 45, size=10, vjust=1, hjust=1),
        axis.title.y = element_text(size=10),
        axis.text.y = element_text(size=10), 
        plot.subtitle = element_text(size=10), 
        plot.margin = margin(10, 10, 10, 20))

facet_themes <- theme(strip.background=element_rect(fill="#ededed", size = 0.9),
                      strip.text = element_text(size = 10))

###########################################################
#################### PROCESS THE DATA #####################

my_tpm_t <- t(my_tpm)
my_tpm_t <- data.frame(SampleID = row.names(my_tpm_t), my_tpm_t)

# Merge tpm with metadata
my_tpm_merged <- merge(my_metadata, my_tpm_t, by = "SampleID")

###########################################################
################## PLOT TPM OF Rv0494 ####################

TPM_Rv0494_plot<- my_tpm_merged %>% ggerrorplot(x = "Condition", y = "Rv0494", 
                            desc_stat = "mean_sd", 
                            error.plot = "errorbar", add = "mean", 
                            color = "Induction", palette = cbPalette_1.5) + 
  facet_grid(~Strain, scales = "free") + 
  labs(title = "Rv0494 TPM for TxnExp2",
       y = "Rv0494 TPM",
       x = NULL) + 
  my_plot_themes + facet_themes
TPM_Rv0494_plot

ggsave(TPM_Rv0494_plot,
       file = "Rv0494_TPM.pdf",
       path = "Single_Gene_Expression_Figures",
       width = 8, height = 5, units = "in")

TPM_Rv0494_plot2 <- my_tpm_merged %>% 
  ggerrorplot(x = "Condition", y = "Rv0494", 
              desc_stat = "mean_sd", 
              error.plot = "errorbar", add = "mean", 
              color = "Induction", palette = cbPalette_1.5) + 
  facet_grid(~Strain, scales = "free") + 
  labs(title = "Rv0494 TPM for TxnExp2",
       y = "Rv0494 log(TPM)",
       x = NULL) + 
  scale_y_log10() +
  my_plot_themes + facet_themes
TPM_Rv0494_plot2

ggsave(TPM_Rv0494_plot2,
       file = "Rv0494_TPM_LogTransformed.pdf",
       path = "Single_Gene_Expression_Figures",
       width = 8, height = 5, units = "in")


###########################################################
################## PLOT TPM OF Rv1473A ####################

TPM_Rv1473A_plot<- my_tpm_merged %>% 
  ggerrorplot(x = "Condition", y = "Rv1473A", 
              desc_stat = "mean_sd", 
              error.plot = "errorbar", add = "mean", 
              color = "Induction", palette = cbPalette_1.5) + 
  facet_grid(~Strain, scales = "free") + 
  labs(title = "Rv1473A TPM for TxnExp2",
       y = "Rv1473A TPM",
       x = NULL) + 
  # scale_y_log10() + 
  my_plot_themes + facet_themes
TPM_Rv1473A_plot

ggsave(TPM_Rv1473A_plot,
       file = "Rv1473A_TPM.pdf",
       path = "Single_Gene_Expression_Figures",
       width = 8, height = 5, units = "in")


TPM_Rv1473A_plot2 <- my_tpm_merged %>% 
  ggerrorplot(x = "Condition", y = "Rv1473A", 
              desc_stat = "mean_sd", 
              error.plot = "errorbar", add = "mean", 
              color = "Induction", palette = cbPalette_1.5) + 
  facet_grid(~Strain, scales = "free") + 
  labs(title = "Rv1473A TPM for TxnExp2",
       y = "Rv1473A log(TPM)",
       x = NULL) + 
  scale_y_log10() + 
  my_plot_themes + facet_themes
TPM_Rv1473A_plot2

ggsave(TPM_Rv1473A_plot2,
       file = "Rv1473A_TPM_LogTransformed.pdf",
       path = "Single_Gene_Expression_Figures",
       width = 8, height = 5, units = "in")

###########################################################
################## PLOT TPM OF Rv0827c ####################

TPM_Rv0827c_plot <- my_tpm_merged %>% 
  ggerrorplot(x = "Condition", y = "Rv0827c", 
              desc_stat = "mean_sd", 
              error.plot = "errorbar", add = "mean", 
              color = "Induction", palette = cbPalette_1.5) + 
  facet_grid(~Strain, scales = "free") + 
  labs(title = "Rv0827c TPM for TxnExp2",
       y = "Rv0827c TPM",
       x = NULL) + 
  # scale_y_log10() +
  my_plot_themes + facet_themes
TPM_Rv0827c_plot

ggsave(TPM_Rv0827c_plot,
       file = "Rv0827c_TPM.pdf",
       path = "Single_Gene_Expression_Figures",
       width = 8, height = 5, units = "in")

TPM_Rv0827c_plot2 <- my_tpm_merged %>% 
  ggerrorplot(x = "Condition", y = "Rv0827c", 
              desc_stat = "mean_sd", 
              error.plot = "errorbar", add = "mean", 
              color = "Induction", palette = cbPalette_1.5) + 
  facet_grid(~Strain, scales = "free") + 
  labs(title = "Rv0827c TPM for TxnExp2",
       y = "Rv0827c log(TPM)",
       x = NULL) + 
  scale_y_log10() +
  my_plot_themes + facet_themes
TPM_Rv0827c_plot2

ggsave(TPM_Rv0827c_plot2,
       file = "Rv0827c_TPM_logTransformed.pdf",
       path = "Single_Gene_Expression_Figures",
       width = 8, height = 5, units = "in")
