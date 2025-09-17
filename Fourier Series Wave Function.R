library(shiny)
library(plotly)

# Fourier series function for a square wave (odd harmonics)
fourier_series <- function(K, t) {
  y <- rep(0, length(t))
  for (n in seq(1, K, by = 2)) {
    y <- y + (4 / (n * pi)) * sin(n * t)
  }
  return(y)
}

# UI
ui <- fluidPage(
  titlePanel("Fourier Series"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("K", "Select value of K:",
                  choices = c(1, 5, 10, 25, 50, 75, 100, "N = ∞"),
                  selected = 10)
    ),
    
    mainPanel(
      plotlyOutput("fourierPlot", height = "500px"),
      tags$div(id = "formula-display", style = "margin-top: 10px; font-size: 16px;")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  output$fourierPlot <- renderPlotly({
    K <- input$K
    if (K == "N = ∞") {
      K <- 200   # approximate infinity with many terms
    } else {
      K <- as.numeric(K)
    }
    
    # Data for the wave
    t <- seq(0, 18, length.out = 2500)
    y <- fourier_series(K, t)
    
    # Data for the circle
    theta <- seq(0, 2 * pi, length.out = 360)
    circle_x <- cos(theta)
    circle_y <- sin(theta)
    
    # Create the plotly figure
    p <- plot_ly() %>%
      add_lines(x = circle_x, y = circle_y, name = "Unit Circle",
                line = list(dash = "dash", color = "red")) %>%
      add_markers(x = 0, y = 0, name = "Center", marker = list(size = 8)) %>%
      add_lines(x = t, y = y, name = paste("Wave (K=", K, ")", sep=""),
                line = list(color = "blue")) %>%
      layout(
        xaxis = list(title = "x (Wave Function)", range = c(-1.2, 18)),
        yaxis = list(title = "Amplitude", range = c(-2.2, 2.2)),
        legend = list(x = 0.02, y = 0.98),
        margin = list(l = 40, r = 30, t = 50, b = 40),
        height = 500
      )
    
    p
  })
  
  output$formula_display <- renderUI({
    K <- input$K
    if (K == "N = ∞") {
      K <- 200
    } else {
      K <- as.numeric(K)
    }
    terms <- sapply(seq(1, K, by = 2),
                    function(n) sprintf("%.3f*sin(%dt)", 4/(n*pi), n))
    if (length(terms) > 10) {
      terms <- c(terms[1:10], "...")
    }
    formula <- paste("f(t) =", paste(terms, collapse = " + "))
    tags$pre(formula)
  })
}

# Run the app
shinyApp(ui, server)
