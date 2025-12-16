library(shiny)

ui <- fluidPage(
  titlePanel("Systematic Sampling – Summary Only"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("N", "Population Size (N)", value = 300, min = 50),
      sliderInput("Z", "Confidence Level (Z value)",
                  min = 1.64, max = 2.58, value = 1.96, step = 0.01),
      sliderInput("B", "Sampling Bias / Allowable Error",
                  min = 1, max = 10, value = 5),
      actionButton("calc", "Generate Sampling Design")
    ),
    
    mainPanel(
      h4("Sampling Plan Summary"),
      tableOutput("summary"),
      br(),
      h4("Selected Units"),
      tableOutput("selected_units")
    )
  )
)

server <- function(input, output) {
  
  sampling_design <- eventReactive(input$calc, {
    
    S <- 20  # assumed population standard deviation
    
    # Sample size for population mean
    n <- ceiling((input$Z^2 * S^2) / (input$B^2))
    
    # Sampling interval
    k <- floor(input$N / n)
    
    # Random start
    r <- sample(1:k, 1)
    
    # Selected units
    selected <- seq(r, input$N, by = k)
    
    list(N = input$N, n = n, k = k, r = r, selected = selected)
  })
  
  # Table: Sampling Plan Summary
  output$summary <- renderTable({
    d <- sampling_design()
    data.frame(
      Parameter = c("Population Size (N)",
                    "Sample Size (n)",
                    "Sampling Interval (k)",
                    "Random Start"),
      Value = c(d$N, d$n, d$k, d$r)
    )
  })
  
  # Table: Selected Units
  output$selected_units <- renderTable({
    d <- sampling_design()
    data.frame(
      Sample_Order = 1:length(d$selected),
      Selected_Unit = d$selected
    )
  })
}

shinyApp(ui, server)
