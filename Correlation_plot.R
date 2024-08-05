# Make a correlation plot
# 6/18/24


source("Import_data.R") # to get my_tpm


###########################################################
#################### MAKE CORRELATION #####################

# Make the correlation using stats package
my_cor_spearman <- cor(my_tpm, method = "spearman")
my_cor_pearson <- cor(my_tpm, method = "pearson")

# Get the p-values using ggcorplot package
my_pvalue_spearman <- cor_pmat(my_tpm, method = "spearman")
my_pvalue_pearson <- cor_pmat(my_tpm, method = "pearson")
# They're all zeros.... not sure if this is true or not?


###########################################################
################### CORRPLOT FUNCTION #####################

# https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html

corrplot(my_cor_spearman, method = "square", order = "AOE",
         addCoef.col = "black", number.font = 0.01,
         tl.cex = 0.5, tl.col = "black" # Change variable name parameters
         )

# This method not so good because I can't figure out how to scale the colors!

###########################################################
################ GGCORRPLOT ALL TOGETHER ##################

# http://www.sthda.com/english/wiki/ggcorrplot-visualization-of-a-correlation-matrix-using-ggplot2

min(my_cor_spearman) # 0.9433115
min(my_cor_pearson) # 0.8331902
# So I know what my lower limit should be

# Plot basics
my_plot_themes <- theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "right",legend.text=element_text(size=10),
        legend.title = element_text(size = 10),
        plot.title = element_text(size=10), 
        axis.title.x = element_blank(), 
        axis.text.x = element_text(angle = 90, size=10, vjust=0, hjust=0.5),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size=10), 
        plot.subtitle = element_text(size=10), 
        plot.margin = margin(10, 10, 10, 20))

# Plot spearman
spearman_all_plot <- ggcorrplot(my_cor_spearman, hc.order = FALSE, 
           lab = TRUE, lab_size = 1.5) + 
  scale_fill_gradient2(limit = c(0.94,1), low = "blue", high =  "red", mid = "white", midpoint = 0.97) + # Make sure to change based on the min!
  my_plot_themes + 
  scale_x_discrete(guide = guide_axis(angle = 90)) + 
  labs(title = "Spearman Correlation all", fill = "Correlation")
spearman_all_plot

ggsave(spearman_all_plot,
       file = "Spearman_CorrelationPlot_all.pdf",
       path = "Correlation_Figures",
       width = 12, height = 12, units = "in")

# Plot pearson
pearson_all_plot <- ggcorrplot(my_cor_pearson, hc.order = FALSE, 
                                lab = TRUE, lab_size = 1.5) + 
  scale_fill_gradient2(limit = c(0.80,1), low = "blue", high =  "red", mid = "white", midpoint = 0.90) + # Make sure to change based on the min!
  my_plot_themes + 
  scale_x_discrete(guide = guide_axis(angle = 90)) + 
  labs(title = "Pearson Correlation all", fill = "Correlation")
pearson_all_plot

ggsave(pearson_all_plot,
       file = "Pearson_CorrelationPlot_all.pdf",
       path = "Correlation_Figures",
       width = 12, height = 12, units = "in")


###########################################################
################### GGCORRPLOT EV ONLY ####################

my_tpm_EV <- my_tpm %>% select(contains("EmptyVector"))
my_cor_spearman_EV <- cor(my_tpm_EV, method = "spearman")
my_cor_pearson_EV <- cor(my_tpm_EV, method = "pearson")


# Plot spearman
spearman_plot_EV <- my_cor_spearman_EV %>% 
  ggcorrplot(hc.order = FALSE, 
                                lab = TRUE, lab_size = 1.5) + 
  scale_fill_gradient2(limit = c(0.94,1), low = "blue", high =  "red", mid = "white", midpoint = 0.97) + # Make sure to change based on the min!
  my_plot_themes + 
  scale_x_discrete(guide = guide_axis(angle = 90)) + 
  labs(title = "Empty Vector Spearman Correlation", fill = "Correlation")
spearman_plot_EV

ggsave(spearman_plot_EV,
       file = "Spearman_CorrelationPlot_EV.pdf",
       path = "Correlation_Figures",
       width = 6, height = 6, units = "in")

# Plot pearson
pearson_plot_EV <- my_cor_pearson_EV %>% 
  ggcorrplot(hc.order = FALSE, 
             lab = TRUE, lab_size = 1.5) + 
  scale_fill_gradient2(limit = c(0.80,1), low = "blue", high =  "red", mid = "white", midpoint = 0.90) + # Make sure to change based on the min!
  my_plot_themes + 
  scale_x_discrete(guide = guide_axis(angle = 90)) + 
  labs(title = "Empty Vector Pearson Correlation", fill = "Correlation")
pearson_plot_EV

ggsave(pearson_plot_EV,
       file = "Pearson_CorrelationPlot_EV.pdf",
       path = "Correlation_Figures",
       width = 6, height = 6, units = "in")


###########################################################
##################### CONCLUSIONS #########################

# Looks like EV ATC RIF Rep 2 has the lowest correlation, 

 