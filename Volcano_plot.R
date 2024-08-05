# Make a volcano plot 
# 6/18/24

source("Import_data.R") 

# Plot basics
my_plot_themes <- theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "none",legend.text=element_text(size=10),
        legend.title = element_text(size = 10),
        plot.title = element_text(size=10), 
        axis.title.x = element_text(size=10), 
        axis.text.x = element_text(angle = 0, size=10, vjust=1, hjust=0.5),
        axis.title.y = element_text(size=10),
        axis.text.y = element_text(size=10), 
        plot.subtitle = element_text(size=10), 
        plot.margin = margin(10, 10, 10, 20))


###########################################################
########### FUNCTION TO MAKE ALL VOLCANO PLOTS ############

make_volcano_function <- function(my_df, graph_title) {
  
  ## Make a volcano plot using output from Bob's pipeline
  
  my_volcano <- my_df %>%
    ggplot(aes(x = LOG2FOLD, y = -log10(AVG_PVALUE), col = DE, label = DE_labels, text = GENE_NAME)) + # text is for plotly, could be GENE_ID
    geom_point() + 
    labs(title = graph_title) + 
    geom_vline(xintercept = c(-1,1), col = "grey", linetype = "dashed") + 
    geom_hline(yintercept = -log10(0.05), col = "grey", linetype = "dashed") + 
    
    # Need it this way so the colors aren't messed up by not having significant up or down
    # scale_color_manual(values = c("#00AFBB", "grey", "#bb0c00")) + 
    scale_color_manual(values = c(`significant down` = "#00AFBB", `not significant` = "grey", `significant up` = "#bb0c00")) +
    
    # Set these so all plots have the same axes
    scale_y_continuous(limits = c(-1, 130), breaks = seq(0,130,25)) + 
    scale_x_continuous(limits = c(-4.5, 4.5), breaks = seq(-4,4,1)) + 
    
    geom_text_repel(max.overlaps = Inf, size = 3) # Can do geom_text_repel or geom_label_rebel
  
  final_volcano <- my_volcano + my_plot_themes

}

# Check the function works
# test <- make_volcano_function(list_dfs_2[[1]], df_names[1])
# ggplotly(test)

###########################################################
############# LOOP TO SAVE ALL VOLCANO PLOTS ##############

# df_names # Remember I have this

my_path <- "//Users/snork-maiden/Documents/Micro_grad_school/Sherman_Lab/R_projects/Rv0494_Rv1437A/TFI_Txn2_v1/BobAverages_Volcano_plot_figures"

for (i in 1:length(list_dfs_2)) {

  current_df_name <- df_names[i]
  filename <- paste0(current_df_name, "volcano.pdf")

  my_plot <- make_volcano_function(list_dfs_2[[i]], df_names[i])

  ggsave(my_plot,
         file = filename,
         path = my_path,
         width = 7, height = 5, units = "in")
}

# Make sure to check what the warnings are... okay if they are just the geom_text_repel but make sure not data is being lost by defined axes!

###########################################################
############### MAKE A SINGLE VOLCANO PLOT ################

# Making the one volcano plot with the outlier removed for the EV 
single_plot <- make_volcano_function(list_dfs_2[[9]], df_names[9])
single_plot <- single_plot + labs(subtitle = "Outlier based on PCA plot (sample 1) removed")
single_plot
ggsave(single_plot,
       file = paste0(df_names[9], "OutlierRemoved_volcano.pdf"),
       path = my_path,
       width = 7, height = 5, units = "in")

###########################################################
############ COMPARE RV0827C DMSO TO EV DMSO ##############

# Want to know if Rv0827c is differentially expressed in the Rv0827c DMSO compared to another (using EV DMSO)
# Will need to go back to the Lenov to get the JOINED file








