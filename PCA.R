# PCA plot
# 6/18/24

source("Import_data.R") # to get my_tpm

# Plot basics
my_plot_themes <- theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "right",legend.text=element_text(size=10),
        legend.title = element_text(size = 10),
        plot.title = element_text(size=10), 
        axis.title.x = element_text(size=10), 
        axis.text.x = element_text(angle = 0, size=10, vjust=0, hjust=0.5),
        axis.title.y = element_text(size=10),
        axis.text.y = element_text(size=10), 
        plot.subtitle = element_text(size=10), 
        plot.margin = margin(10, 10, 10, 20))

###########################################################
######################## MAKE PCA #########################

# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/

# Two options in base R, prcomp() and princomp()
# prcomp() is preferred according to the website above


# Think I need to transform the data first
my_tpm_t <- as.data.frame(t(my_tpm))

# Remove columns that are all zero so the scale works for prcomp
my_tpm_t2 <- my_tpm_t %>% select_if(colSums(.) != 0)

# Make the actual PCA
my_PCA <- prcomp(my_tpm_t2, scale = TRUE)

# See the % Variance explained
summary(my_PCA)
summary_PCA <- as.data.frame(summary(my_PCA)[["importance"]]['Proportion of Variance',]) * 100
summary_PCA[1,1] # PC1 explains 37.5% of variance
summary_PCA[2,1] # PC2 explains 13.6% of variance
summary_PCA[3,1] # PC3 explains 7.9% of variance

###########################################################
################ MAKE PCA PLOT with GGPLOT ################

my_PCA_df <- as.data.frame(my_PCA$x[, 1:3]) # Extract the first 3 PCs
my_PCA_df <- data.frame(SampleID = row.names(my_PCA_df), my_PCA_df)
my_PCA_df <- merge(my_PCA_df, my_metadata, by = "SampleID")

fig_PC1vsPC2 <- my_PCA_df %>%
  ggplot(aes(x = PC1, y = PC2, color = Condition, shape = Strain, text = Replicate)) + 
  geom_point(size = 3) +
  geom_text_repel(aes(label = Replicate), color = "black", size = 2) + 
  scale_color_manual(values = c16) + 
  labs(title = "PCA plot Txn Exp 2: PC1 vs PC2",
       x = paste0("PC1: ", summary_PCA[1,1], "%"),
       y = paste0("PC2: ", summary_PCA[2,1], "%")) +
  my_plot_themes
fig_PC1vsPC2
ggplotly(fig_PC1vsPC2)

ggsave(fig_PC1vsPC2,
       file = "PCA_PC1vsPC2.pdf",
       path = "PCA_Figures",
       width = 9, height = 6, units = "in")

fig_PC1vsPC3 <- my_PCA_df %>%
  ggplot(aes(x = PC1, y = PC3, color = Condition, shape = Strain, text = Replicate)) + 
  geom_point(size = 3) +
  geom_text_repel(aes(label = Replicate), color = "black", size = 2, max.overlaps = 20) + 
  scale_color_manual(values = c16) + 
  labs(title = "PCA plot Txn Exp 2: PC1 vs PC3",
       x = paste0("PC1: ", summary_PCA[1,1], "%"),
       y = paste0("PC3: ", summary_PCA[3,1], "%")) +
  my_plot_themes
fig_PC1vsPC3
ggplotly(fig_PC1vsPC3)

ggsave(fig_PC1vsPC3,
       file = "PCA_PC1vsPC3.pdf",
       path = "PCA_Figures",
       width = 9, height = 6, units = "in")


###########################################################
################### MAKE 3D PCA PLOT ######################

# https://plotly.com/r/pca-visualization/

PCA_3D <- plot_ly(my_PCA_df, x = ~PC1, y = ~PC2, z = ~PC3,
                    type = "scatter3d", mode = "markers",
                    color = ~Condition, colors = c12,
                    text = ~Replicate)
PCA_3D
# htmlwidgets::saveWidget(as_widget(PCA_3D), "PCA_3D.html")


###########################################################
##################### CONCLUSIONS #########################

# Looks like EV DMSO RIF Rep 1 is a big outlier, maybe should be removed (not removed as of 6/18/24)









