# TXN 2
# Transcriptomics on Rv0494, Rv1473A, Rv0827c, EV
# ATC or DMSO
# no drug or with 2.2ng/mL RIF for 16hr
# 6/18/24


################################################
################ LOAD PACKAGES #################

library(ggplot2)
library(tidyverse)
library(ggpubr)
library(RColorBrewer)
library(knitr)
library(plotly)
library(ggprism) # for add_pvalue()
library(rstatix) # for adjust_pvalue
library(ggpmisc) # https://stackoverflow.com/questions/7549694/add-regression-line-equation-and-r2-on-graph
library(ggrepel)
library(pheatmap)
library(ggplotify) # To convert pheatmaps to ggplots
library(corrplot)
library(ggcorrplot)
library(ggfortify) # To make pca plots with plotly



cbPalette_1 <- c("#999999", "#E69F00") # Gold and Grey
cbPalette_1.5 <- c("#E69F00", "#999999") # Gold and Grey
cbPalette_2 <- c( "#0072B2", "#999999") # Blue and Grey
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbPalette2 <-  c("#bfbfbf", "#56B4E9")
cbPalette3 <-  c("#bfbfbf", "#E69F00")
cbPalette4 <- c("#56B4E9", "#009E73", "#F0E442")
c25 <- c(
  "dodgerblue2", "#E31A1C", "green4",
  "#6A3D9A","#FF7F00","black", "gold1",
  "skyblue2", "#FB9A99","palegreen2","#CAB2D6",
  "#FDBF6F","gray70", "khaki2","maroon", "orchid1", "deeppink1", "blue1", "steelblue4","darkturquoise", "green1", "yellow4", "yellow3","darkorange4", "brown"
)
c12 <- c("dodgerblue2", "#E31A1C", "green4", "#6A3D9A", "#FF7F00", "black", "palegreen2", "gray70", "maroon", "orchid1", "darkturquoise", "darkorange4") 
c16 <- c("dodgerblue2", "#E31A1C", "green4", "#6A3D9A", "#FF7F00", "black","gold1", "#FB9A99", "#CAB2D6", "palegreen2", "gray70", "maroon", "orchid1", "blue1", "darkturquoise", "darkorange4") 


# Stop scientific notation
options(scipen = 999) 
# options(scipen = 0) # To revert back to default

###########################################################
################### IMPORT BOB's DE DATA ##################

Rv0494_RIF_ATC_ComparedTo_DMSO <- read.delim("JOINED_BobAverages/MTb.MetaResults.RV0494_RIF_DMSOvsATC/Rv0494_ATC_RIF.MTb.Meta.JOINED.txt")
Rv0494_noDrug_ATC_ComparedTo_DMSO <- read.delim("JOINED_BobAverages/MTb.MetaResults.RV0494_noDrug_DMSOvsATC/Rv0494_ATC_noDrug.MTb.Meta.JOINED.txt")
Rv0494_ATC_RIF_ComparedTo_noDrug <- read.delim("JOINED_BobAverages/MTb.MetaResults.RV0494_ATC_noDrugvsRIF/Rv0494_ATC_RIF.MTb.Meta.JOINED.txt")
Rv0494_DMSO_RIF_ComparedTo_noDrug <- read.delim("JOINED_BobAverages/MTb.MetaResults.RV0494_DMSO_noDrugvsRIF/Rv0494_DMSO_RIF.MTb.Meta.JOINED.txt")

Rv1473A_RIF_ATC_ComparedTo_DMSO <- read.delim("JOINED_BobAverages/MTb.MetaResults.RV1473A_RIF_DMSOvsATC/Rv1473A_ATC_RIF.MTb.Meta.JOINED.txt")
Rv1473A_noDrug_ATC_ComparedTo_DMSO <- read.delim("JOINED_BobAverages/MTb.MetaResults.RV1473A_noDrug_DMSOvsATC/Rv1473A_ATC_noDrug.MTb.Meta.JOINED.txt")
Rv1473A_ATC_RIF_ComparedTo_noDrug <- read.delim("JOINED_BobAverages/MTb.MetaResults.RV1473A_ATC_noDrugvsRIF/Rv1473A_ATC_RIF.MTb.Meta.JOINED.txt")
Rv1473A_DMSO_RIF_ComparedTo_noDrug <- read.delim("JOINED_BobAverages/MTb.MetaResults.RV1473A_DMSO_noDrugvsRIF/Rv1473A_DMSO_RIF.MTb.Meta.JOINED.txt")

EV_RIF_ATC_ComparedTo_DMSO <- read.delim("JOINED_BobAverages/MTb.MetaResults.EV_RIF_DMSOvsATC/EV_ATC_RIF.MTb.Meta.JOINED.txt")
EV_noDrug_ATC_ComparedTo_DMSO <- read.delim("JOINED_BobAverages/MTb.MetaResults.EV_noDrug_DMSOvsATC/EV_ATC_noDrug.MTb.Meta.JOINED.txt")
EV_ATC_RIF_ComparedTo_noDrug <- read.delim("JOINED_BobAverages/MTb.EV_ATC_noDrugvsRIF/EV_ATC_RIF.MTb.Meta.JOINED.txt")
EV_DMSO_RIF_ComparedTo_noDrug <- read.delim("JOINED_BobAverages/MTb.EV_DMSO_noDrugvsRIF/EV_DMSO_RIF.MTb.Meta.JOINED.txt")

Rv0827c_RIF_ATC_ComparedTo_DMSO <- read.delim("JOINED_BobAverages/MTb.MetaResults.Rv0827c_RIF_DMSOvsATC/Rv0827c_ATC_RIF.MTb.Meta.JOINED.txt")
Rv0827c_noDrug_ATC_ComparedTo_DMSO <- read.delim("JOINED_BobAverages/MTb.MetaResults.Rv0827c_noDrug_DMSOvsATC/Rv0827c_ATC_noDrug.MTb.Meta.JOINED.txt")
Rv0827c_ATC_RIF_ComparedTo_noDrug <- read.delim("JOINED_BobAverages/MTb.MetaResults.Rv0827c_ATC_noDrugvsRIF/Rv0827c_ATC_RIF.MTb.Meta.JOINED.txt")
Rv0827c_DMSO_RIF_ComparedTo_noDrug <- read.delim("JOINED_BobAverages/MTb.MetaResults.Rv0827c_DMSO_noDrugvsRIF/Rv0827c_DMSO_RIF.MTb.Meta.JOINED.txt")

# EXTRAS
# To compare the expression of Rv0827c between the DMSO and the EV DMSO
DMSO_noDrug_Rv0827c_ComparedTo_EV <- read.delim("JOINED_BobAverages/MTb.DMSO_noDrug_EVvsRv0827c/Rv0827c_DMSO_noDrug.MTb.Meta.JOINED.txt")


###########################################################
################ MAKE A LIST OF ALL DFs ###################
list_dfs <- list(Rv0494_RIF_ATC_ComparedTo_DMSO,
                 Rv0494_noDrug_ATC_ComparedTo_DMSO, 
                 Rv0494_ATC_RIF_ComparedTo_noDrug,
                 Rv0494_DMSO_RIF_ComparedTo_noDrug,
                 Rv1473A_RIF_ATC_ComparedTo_DMSO,
                 Rv1473A_noDrug_ATC_ComparedTo_DMSO,
                 Rv1473A_ATC_RIF_ComparedTo_noDrug,
                 Rv1473A_DMSO_RIF_ComparedTo_noDrug,
                 Rv0827c_RIF_ATC_ComparedTo_DMSO,
                 Rv0827c_noDrug_ATC_ComparedTo_DMSO, 
                 Rv0827c_ATC_RIF_ComparedTo_noDrug,
                 Rv0827c_DMSO_RIF_ComparedTo_noDrug,
                 EV_RIF_ATC_ComparedTo_DMSO, 
                 EV_noDrug_ATC_ComparedTo_DMSO, 
                 EV_ATC_RIF_ComparedTo_noDrug, 
                 EV_DMSO_RIF_ComparedTo_noDrug,
                 DMSO_noDrug_Rv0827c_ComparedTo_EV)

# Make a list of the names
df_names <- c("Rv0494_RIF_ATC_ComparedTo_DMSO",
              "Rv0494_noDrug_ATC_ComparedTo_DMSO",
              "Rv0494_ATC_RIF_ComparedTo_noDrug",
              "Rv0494_DMSO_RIF_ComparedTo_noDrug", 
              "Rv1473A_RIF_ATC_ComparedTo_DMSO",
              "Rv1473A_noDrug_ATC_ComparedTo_DMSO",
              "Rv1473A_ATC_RIF_ComparedTo_noDrug",
              "Rv1473A_DMSO_RIF_ComparedTo_noDrug", 
              "Rv0827c_RIF_ATC_ComparedTo_DMSO",
              "Rv0827c_noDrug_ATC_ComparedTo_DMSO",
              "Rv0827c_ATC_RIF_ComparedTo_noDrug",
              "Rv0827c_DMSO_RIF_ComparedTo_noDrug",
              "EV_RIF_ATC_ComparedTo_DMSO", 
              "EV_noDrug_ATC_ComparedTo_DMSO", 
              "EV_ATC_RIF_ComparedTo_noDrug", 
              "EV_DMSO_RIF_ComparedTo_noDrug",
              "DMSO_noDrug_Rv0827c_ComparedTo_EV")

# Give the df list the correct df names
names(list_dfs) <- df_names



###########################################################
############### ADD COLUMNS OF DE VALUES ##################

# Make a new list to hold dataframes with extra columns
list_dfs_2 <- list()

ordered_DE <- c("significant down", "not significant", "significant up")

# Add extra DE columns to each dataframe
for (i in 1:length(list_dfs)) {
  
  current_df <- list_dfs[[i]]
  current_df_name <- df_names[i]
  
  # Make the column pointing out which ones are differentially expressed
  current_df$DE <- ifelse(current_df$LOG2FOLD < -1 & current_df$AVG_PVALUE < 0.05, "significant down",
                          ifelse(current_df$LOG2FOLD > 1 & current_df$AVG_PVALUE < 0.05, "significant up", "not significant"))
  current_df$DE <- factor(current_df$DE, levels = ordered_DE)
  
  # Make the column with DE gene names for plotting on graph
  current_df$DE_labels <- ifelse(current_df$DE != "not significant", current_df$GENE_NAME, NA)
  
  list_dfs_2[[current_df_name]] <- current_df
  
}


###########################################################
##################### IMPORT METADATA #####################

my_metadata <- read.csv("Txn2_metadata.csv") # I made this by hand in excel

my_metadata$Induction2 <- ifelse(my_metadata$Induction == "Induced", "ATC", "DMSO")
my_metadata$Condition <- paste0(my_metadata$Strain, " ", my_metadata$Induction2, " ", my_metadata$Drug)
my_metadata$Replicate <- as.character(my_metadata$Replicate)

my_metadata <- my_metadata[-nrow(my_metadata),] # remove the last row which is the Undetermined

my_metadata$SampleID <- gsub(x = my_metadata$SampleID, pattern = "_2_2_ng_mL", replacement = "")
my_metadata$SampleID <- gsub(x = my_metadata$SampleID, pattern = "_S.*", replacement = "")

###########################################################
############ IMPORT AND PROCESS ALL TPM VALUES ############

my_tpm <- read.csv("Mtb.Expression.Gene.Data.TPM.csv")

my_tpm <- my_tpm[,-ncol(my_tpm)] # remove the last column which is the Undetermined

# Adjust the names so they are slightly shorter
# names(my_tpm) <- gsub(x = names(my_tpm), pattern = "_2_2_ng_mL", replacement = "")
names(my_tpm) <- gsub(x = names(my_tpm), pattern = "_S.*", replacement = "") # This regular expression removes the _S and everything after it (I think...)

# add rownames to the tpm and metadata dataframes
rownames(my_tpm) <- my_tpm[,1] # add the rownames
my_tpm <- my_tpm[,-1] # Remove the old column of rownames
rownames(my_metadata) <- my_metadata[,1] # add the rownames
# my_metadata <- my_metadata[,-1] # Remove the old column of rownames

###########################################################
##################### ORDER CONDITIONS ####################

Ordered_Strain <- c("EV", "Rv0494", "Rv1473A", "Rv0827c")
my_metadata$Strain <- factor(my_metadata$Strain, levels = Ordered_Strain)





# NOT RUN BELOW!!!!

# ALL BELOW IS FOR OLD SHINY


###########################################################
################# ADD COLUMNS WITH DE INFO ################

Add_DE_columns_func <- function(my_df) {
  
  ## Need to make a new column saying which ones are significantly up/down regulated
  ## Need to make a new column with gene names I want to show
  
  my_df$DE <- ifelse(my_df$LOG2FOLD < -1 & my_df$AVG_PVALUE < 0.05, "significant down",
                     ifelse(my_df$LOG2FOLD > 1 & my_df$AVG_PVALUE < 0.05, "significant up", "not significant"))
  ordered_DE <- c("significant down", "not significant", "significant up")
  my_df$DE <- factor(my_df$DE, levels = ordered_DE)
  
  my_df$DE_labels <- ifelse(my_df$DE != "not significant", my_df$GENE_ID, NA)
  
  my_df <- my_df
}

# Add the columns using the function
Rv0494_RIF_ATC_ComparedTo_DMSO <- Add_DE_columns_func(Rv0494_RIF_ATC_ComparedTo_DMSO)
Rv0494_RIF_ATC_ComparedTo_DMSO$Condition <- deparse(substitute(Rv0494_RIF_ATC_ComparedTo_DMSO))

Rv1473A_RIF_ATC_ComparedTo_DMSO <- Add_DE_columns_func(Rv1473A_RIF_ATC_ComparedTo_DMSO)
Rv1473A_RIF_ATC_ComparedTo_DMSO$Condition <- deparse(substitute(Rv1473A_RIF_ATC_ComparedTo_DMSO))
EV_RIF_ATC_ComparedTo_DMSO <- Add_DE_columns_func(EV_RIF_ATC_ComparedTo_DMSO)
EV_RIF_ATC_ComparedTo_DMSO$Condition <- deparse(substitute(EV_RIF_ATC_ComparedTo_DMSO))

# Extract just the columns I want to keep
Rv0494_df <- Rv0494_RIF_ATC_ComparedTo_DMSO %>% 
  select(GENE_NAME, GENE_ID, LOG2FOLD, AVG_PVALUE, AVG_RANK, DE, DE_labels, Condition)
Rv1473A_df <- Rv1473A_RIF_ATC_ComparedTo_DMSO %>% 
  select(GENE_NAME, GENE_ID, LOG2FOLD, AVG_PVALUE, AVG_RANK, DE, DE_labels, Condition)
EV_df <- EV_RIF_ATC_ComparedTo_DMSO %>% 
  select(GENE_NAME, GENE_ID, LOG2FOLD, AVG_PVALUE, AVG_RANK, DE, DE_labels, Condition)

# Combine all dafaframes into one big dataframe, for Shiny
all_df <- rbind(Rv0494_df, Rv1473A_df)
all_df <- rbind(all_df, EV_df)

conditions_list <- unique(all_df$Condition)


