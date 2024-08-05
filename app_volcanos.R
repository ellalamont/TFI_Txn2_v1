library(shiny)
library(gmodels)

source("Import_data.R") # for list_dfs_2

# Plot basics
my_plot_themes <- theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "none",legend.text=element_text(size=12),
        legend.title = element_text(size = 12),
        plot.title = element_text(size=12), 
        axis.title.x = element_text(size=12), 
        axis.text.x = element_text(angle = 0, size=12, vjust=1, hjust=0.5),
        axis.title.y = element_text(size=12),
        axis.text.y = element_text(size=12), 
        plot.subtitle = element_text(size=12), 
        plot.margin = margin(10, 10, 10, 20))



# Define UI ----
ui <- fluidPage(
  titlePanel("EL TFI Txn Exp2 7/3/24"),
  
  fluidRow(
    
    column(width = 2.5,
           # h3("Choose the condition"),
           selectInput("my_comparison",
                       label = "Choose comparisons",
                       choices = df_names),
           textInput("my_GeneID", 
                     label = "Gene ID",
                     value = "Rv..."), 
    ),
    
    column(width = 5,
           plotlyOutput("volcano_plot",
                        width = 1000, height = 600),
    ),
  )
  
)

# Define server logic ----
server <- function(input, output) {
  
  output$volcano_plot <- renderPlotly({
    
    single_gene <- list_dfs_2[[input$my_comparison]] %>% 
      filter(GENE_ID == input$my_GeneID)
    
    my_volcano <- list_dfs_2[[input$my_comparison]] %>%
      ggplot(aes(x = LOG2FOLD, y = -log10(AVG_PVALUE), col = DE, label = DE_labels, text = GENE_ID)) + 
      geom_point() + 
      
      # Add a differently colored point
      geom_point(data = single_gene, color = "yellow", aes(col = DE, label = DE_labels, text = GENE_ID)) + 
      
      labs(title = input$my_comparison) + 
      geom_vline(xintercept = c(-1,1), col = "grey", linetype = "dashed") + 
      geom_hline(yintercept = -log10(0.05), col = "grey", linetype = "dashed") + 
      scale_color_manual(values = c(`significant down` = "#00AFBB", `not significant` = "grey", `significant up` = "#bb0c00")) +
      geom_label_repel(max.overlaps = Inf) # Can do geom_text_repel or geom_label_rebel
    
    
    final_plot <- my_volcano + my_plot_themes 
    
    final_plot
    
    
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)