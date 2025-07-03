library(shiny)
library(ggplot2)
library(reshape2)

development_factors <- list(
  "2023" = c(1, 725000/500000, 735000/725000, 805000/735000),
  "2024" = c(1, 780000/600000, 790000/780000, 865000/790000),
  "2025" = c(1, 945000/700000, 955000/945000, 1050000/955000)
)

# UI
ui <- fluidPage(
  titlePanel("Cumulative Paid Claims"),
  
  h4("Input Table (RM)"),
  sidebarLayout(
    sidebarPanel(
      numericInput("premium2023", "Premium for 2023", value = "", min = 0),
      numericInput("premium2024", "Premium for 2024", value = "", min = 0),
      numericInput("premium2025", "Premium for 2025", value = "", min = 0),
      
      sliderInput("tailFactor", "Tail Factor", 
                  min = 1.00, max = 1.50, value = "", step = 0.01),
      
      actionButton("calculate", "Calculate")
    ),
    
    mainPanel(
      conditionalPanel(
        condition = "output.showResults === true",
        h4("Cumulative Paid Claims (RM) - Table"),
        tableOutput("claimsTable"),
        br(),
        h4("Cumulative Paid Claims (RM) - Graph"),
        plotOutput("claimsPlot")
      )
    )
  )
)

# Server
server <- function(input, output) {
  values <- reactiveValues(show = FALSE)
  
  observeEvent(input$calculate, {
    premiums <- c("2023" = input$premium2023, 
                  "2024" = input$premium2024, 
                  "2025" = input$premium2025)
    
    results <- lapply(names(premiums), function(year) {
      prem <- premiums[[year]]
      factors <- development_factors[[year]]
      tail <- input$tailFactor
      full_factors <- c(factors, tail)
      round(prem * cumprod(full_factors), 0)
    })
    
    df <- data.frame(
      "Development Year" = 1:5,
      "2023" = results[[1]],
      "2024" = results[[2]],
      "2025" = results[[3]]
    )
    
    colnames(df) <- c("Development Year", "2023", "2024", "2025")
    
    #connects the input to the output
    output$claimsTable <- renderTable(df, digits = 0) 
    
    df_long <- melt(df, id.vars = "Development Year")
    colnames(df_long) <- c("DevelopmentYear", "LossYear", "CumulativePaidClaims")
    
    output$claimsPlot <- renderPlot({
      y_min <- min(df_long$CumulativePaidClaims) * 0.95
      y_max <- max(df_long$CumulativePaidClaims) * 1.05
      
      ggplot(df_long, aes(x = DevelopmentYear,y = CumulativePaidClaims,
                          color = LossYear)) +
        geom_line(size = 1) +
        geom_point(size = 3) +
        geom_text(aes(label = CumulativePaidClaims), vjust = -1.2, size = 3) +
        scale_y_continuous(labels = scales::comma, limits = c(y_min, y_max)) +
        scale_x_continuous(breaks = 1:4) +
        labs(x = "Development Year", y = "Cumulative Paid Claims (RM)") +
        theme_minimal() +
        theme(legend.title = element_blank())
    })
    values$show <- TRUE
  })
  output$showResults <- reactive({
    values$show
  })
  outputOptions(output, "showResults", suspendWhenHidden = FALSE)
}

# Run the app
shinyApp(ui = ui, server = server)
