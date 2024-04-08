library(shiny)
library(ggplot2)
library(plotly)
library(readxl)
library(DT)
library(dplyr)

# Read the data
data <- read_xlsx("tempdataset.xlsx")

# UI Logic
ui <- fluidPage(
  titlePanel("World Happiness Analysis"),
  sidebarLayout(
    sidebarPanel(
      class = "sidebar",
      selectInput("region_filter", "Choose Region:",
                  choices = c("All", unique(data$`Regional indicator`))),
      actionButton("reset_region_filter", "Reset")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data", dataTableOutput("filtered_data_table")),
        tabPanel("Graphs",
                 fluidRow(
                   column(10, plotlyOutput("bar_plot")),
                   column(10, plotlyOutput("correlation_plot"), style = "margin: 20px;")
                 ),
                 fluidRow(
                   column(10, plotlyOutput("scatter_plot")),
                   column(10, plotlyOutput("bubble_plot"), style = "margin: 20px;")
                 )
        ),
        tabPanel("Summary", verbatimTextOutput("summary_table"))
      )
      
    )
  )
)

# server logic
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    filtered <- data
    if (input$region_filter != "All") {
      filtered <- filtered[filtered$`Regional indicator` == input$region_filter, ]
    }
    filtered
  })
  
  observeEvent(input$reset_region_filter, {
    updateSelectInput(session, "region_filter", selected = "All")
  })
  
  output$filtered_data_table <- renderDataTable({
    filtered_data()
  }, options = list(pagingType = 'full_numbers'))
  
  output$summary_table <- renderPrint({
    summary(filtered_data())
  })
  
  # Scatter Plot Logic
  output$scatter_plot <- renderPlotly({
    gg <- ggplot(filtered_data(), aes(x = `Logged GDP per capita`, y = `Happiness score`, text = `Country name`)) +
      geom_point() +
      labs(x = "Logged GDP per capita", y = "Happiness score", text = "Country") +
      geom_smooth(method = "lm", se = FALSE, color = "red") + 
      ggtitle("Happiness Score vs. Logged GDP per Capita") +
      theme(plot.title = element_text(size = 14, face = "bold"))
    
    ggplotly(gg) %>% 
      layout(showlegend = FALSE) %>%
      add_trace(data = filtered_data(), 
                x = ~`Logged GDP per capita`, 
                y = predict(lm(`Happiness score` ~ `Logged GDP per capita`, data = filtered_data())), 
                mode = "lines", 
                line = list(color = "red", width = 2), 
                showlegend = FALSE)
  })
  
  # Correlation Plot
  output$correlation_plot <- renderPlotly({
    correlation_matrix <- cor(filtered_data()[, c("Logged GDP per capita", "Social support", "Healthy life expectancy", "Freedom to make life choices", "Perceptions of corruption", "Happiness score")])
    correlation_df <- as.data.frame(as.table(correlation_matrix))
    
    plot_ly(data = correlation_df, x = ~Var1, y = ~Var2, z = ~Freq, type = "heatmap", colorscale = "Viridis") %>%
      layout(
        title = "Correlation plot between variables"
      )
  })
  
  # Bar Plot Logic
  output$bar_plot <- renderPlotly({
    mean_happiness <- filtered_data() %>%
      group_by(`Regional indicator`) %>%
      summarise(mean_happiness = mean(`Happiness score`))
    
    plot_ly(data = mean_happiness, x = ~`Regional indicator`, y = ~mean_happiness, type = "bar", marker = list(color = "skyblue")) %>%
      layout(
        xaxis = list(title = "Regional indicator"),
        yaxis = list(title = "Mean Happiness Score"),
        title = "Mean Happiness Score by Regional Indicator"
      )
  })
  
  # Bubble Plot Logic
  output$bubble_plot <- renderPlotly({
    plot_ly(filtered_data(), x = ~`Logged GDP per capita`, y = ~`Social support`, size = ~`Happiness score`, color = ~`Happiness score`, text = ~`Country name`) %>%
      add_markers() %>%
      layout(
        xaxis = list(title = "Logged GDP per capita"),
        yaxis = list(title = "Social support"),
        size = list(title = "Happiness score"),
        color = list(title = "Happiness score")
      ) %>%
      layout(title = "Bubble Plot of Happiness Score vs. Logged GDP per Capita and Social Support")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
