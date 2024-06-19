#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# Load libraries

library(shiny)
library(ggrepel)
library(dplyr)
library(ggplot2)
library(magrittr)
library(fuzzyjoin)
library(ggimage)

# Define UI for application 
ui <- fluidPage(
  
  # Application title
  titlePanel("GlycoAnnotator"),
  
  # Sidebar with inputs
  sidebarLayout(
    sidebarPanel(
      fileInput('spectrumFile', 'Upload Mass Spectrum (.csv)', accept = c('text/csv', 'text/comma-separated-values,text/plain', '.txt')),
      fileInput('annotationFile', 'Upload Annotation Library (.csv)', accept = c('text/csv', 'text/comma-separated-values,text/plain', '.txt')),
      sliderInput("massRange", "Select Displayed Mass Range:", min = 500, max = 5000, value = c(500, 1000), step = 25),
      numericInput("intensityThreshold", "Annotate peaks above this intensity:", value = 0, min = -100, max = Inf),
      sliderInput("massTolerance", "Mass Tolerance:", min = -0.05, max = 0.05, value = c(-0.01, 0.01), step = 0.005),
      sliderInput("maxOverlap", "Max Overlap", min = 1, max = 50, value = 5, step = 1),
      selectInput("annotationType", "Annotation Type:", choices = c("Text", "Image"))
    ),
    
    # Show MS plot
    mainPanel(
      plotOutput("massSpectrumPlot")
    )
  )
)

server <- function(input, output) {
  
  massSpectrum <- reactive({
    req(input$spectrumFile)
    read.csv(input$spectrumFile$datapath, header = TRUE)
  })
  
  annotationLibrary <- reactive({
    req(input$annotationFile)
    read.csv(input$annotationFile$datapath, header = TRUE)
  })
  
  output$massSpectrumPlot <- renderPlot({
    req(massSpectrum())
    
    spectrumData <- massSpectrum()
    
    filteredSpectrum <- spectrumData %>%
      filter(mass >= input$massRange[1], mass <= input$massRange[2])
    
    filteredPeaks <- filteredSpectrum %>%
      filter(intensity >= input$intensityThreshold)
    
    p <- ggplot(filteredSpectrum, aes(x = mass, y = intensity)) +
      geom_line() +
      #geom_point(data = filteredPeaks, aes(x = mass, y = intensity), color = "red", size = 1) +
      labs(title = "Mass spectrum", x = "m/z", y = "Intensity") +
      theme_minimal() +
      coord_cartesian(xlim = input$massRange)
    
    if (!is.null(input$annotationFile)) {
      annotationData <- annotationLibrary()
      
      annotatedPeaks <- filteredPeaks %>%
        difference_inner_join(annotationData, by = "mass", max_dist = input$massTolerance) %>%
        group_by(annotation) %>%
        filter(intensity == max(intensity))
      
      if (input$annotationType == "Text") {
        p <- p +
          geom_text_repel(data = annotatedPeaks, aes(x = mass.x, y = intensity, label = annotation),
                          nudge_y = 0.1 * max(filteredSpectrum$intensity),
                          max.overlaps = input$maxOverlap,
                          segment.color = 'grey')}
        
      else if (input$annotationType == "Image") {
        p <- p +
          geom_image(data = annotatedPeaks, aes(x = mass.x, y = intensity, image = image_link),
                     size = 0.5)

      }
    }
    
    p
  })
}

shinyApp(ui = ui, server = server)