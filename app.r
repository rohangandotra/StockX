library(shiny)
require(RColorBrewer)
library(dplyr)
library(magrittr)
library(tidyr)
library(ggplot2)
library(mplot)

shoedata <- read.csv("resaledatabase.csv")

ui <- fluidPage(
  
  titlePanel(title=div(img(height = 100, width = 626, src = "TitleImage.png"), style="text-align: center;")),
  
  # Sidebar
  sidebarLayout(
  sidebarPanel(
    
    width=3,
    
    selectInput(inputId = "xvar", 
                label = "Which Question Would You Like To See Answered?", 
                choices = c("Which Shoe Size Is Most Profitable?",
                            "What Times of Day Does StockX Make Sales?",
                            "How Does Profit Change Over Time?",
                            "What Are The Total Sales Figures?"),
                multiple = FALSE),
    selectInput(inputId = "yvar",
                label = "By Which Variable Do You Want To Separate The Data?",
                choices = c("brand", "model"),
                multiple = FALSE),
    
  ),
  
  # Show a plot, image of sneakers
  mainPanel(
    plotOutput("distPlot"),
    imageOutput("Sneakers"),
  ))
)

server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    if (input$yvar == "brand") {
    if (input$xvar == "Which Shoe Size Is Most Profitable?") {
      ggplot(data = shoedata) +
        geom_smooth(aes_string(x = shoedata$shoeSize, y = shoedata$markUpPercent, color = input$yvar)) +
        scale_color_manual(values = c("black", "darkorange")) +
        scale_x_continuous(breaks = c(4, 6, 8, 10, 12, 14, 16)) +
        labs(title = "Which Shoe Size Is Most Profitable?",
             x = "Shoe Size (US Men)",
             y = "Resale Profit %",
             color = "Brand") +
        theme(axis.text.x = element_text(size = 20),
              axis.text.y = element_text(size = 20),
              axis.title.x = element_text(size = 20),
              axis.title.y = element_text(size = 20),
              title = element_text(size = 20),
              legend.title = element_text(size = 20),
              legend.text = element_text(size = 15))
    } else if (input$xvar == "What Times of Day Does StockX Make Sales?") {
      ggplot(data = shoedata, aes_string(x = shoedata$time, fill = input$yvar)) +
        geom_bar(position = "stack", stat="count") + 
        scale_fill_manual(values = c("black", "darkorange")) +
        scale_x_discrete(breaks = c("02:00", "04:00", "06:00", "08:00", "10:00", "12:00", "14:00", "16:00", "18:00", "20:00", "22:00")) +
        labs(title = "Number of Sales at Different Times of Day (Stacked Bar Graph)",
             x = "Times of Day (24-hr Scale)",
             y = "Number of Sales",
             fill = "Brand") +
        theme(axis.text.x = element_text(size = 20),
              axis.text.y = element_text(size = 20),
              axis.title.x = element_text(size = 20),
              axis.title.y = element_text(size = 20),
              title = element_text(size = 20),
              legend.title = element_text(size = 20),
              legend.text = element_text(size = 15))
    } else if (input$xvar == "How Does Profit Change Over Time?") {
      ggplot(data = shoedata, aes_string(shoedata$timeDiff, shoedata$markUpPercent, color = input$yvar)) + 
        geom_smooth() +
        scale_color_manual(values = c("black", "darkorange")) +
        geom_vline(xintercept = 0, linetype="dotted", size=1.5) +
        geom_text(aes(x=0, label="Release Date", y=100), colour="black", angle=0, vjust = 1, text=element_text(size=15)) +
        labs(title = "Comparison of Profit % of Different Shoe Brands",
             x = "Days from Release Date",
             y = "Resale Profit %",
             color = "Brand") + 
        theme(axis.text.x = element_text(size = 20),
              axis.text.y = element_text(size = 20),
              axis.title.x = element_text(size = 20),
              axis.title.y = element_text(size = 20),
              title = element_text(size = 20),
              legend.title = element_text(size = 20),
              legend.text = element_text(size = 15))
    } else if (input$xvar == "What Are The Total Sales Figures?") {
      ggplot(data = shoedata, aes_string(x = shoedata$timeDiff, color = input$yvar)) +
        geom_freqpoly(alpha = 0.7, size = 1.5) +
        scale_color_manual(values = c("black", "darkorange")) + 
        geom_vline(xintercept = 0, linetype="dotted", size=1.5) +
        geom_text(aes(x=0, label="Release Date", y=1000), colour="black", angle=0, vjust = 1, hjust = 0, text=element_text(size=15)) + 
        labs(title = "Total Number of Sales by Brand",
             x = "Days From Release Date",
             y = "Total Number of Sales",
             color = "Brand") + 
        theme(axis.text.x = element_text(size = 20),
              axis.text.y = element_text(size = 20),
              axis.title.x = element_text(size = 20),
              axis.title.y = element_text(size = 20),
              title = element_text(size = 20),
              legend.title = element_text(size = 20),
              legend.text = element_text(size = 15))
    }} else if (input$yvar == "model") {
      if (input$xvar == "Which Shoe Size Is Most Profitable?") {
        ggplot(data = shoedata) +
          geom_smooth(aes_string(x = shoedata$shoeSize, y = shoedata$markUpPercent, color = input$yvar)) +
          scale_color_manual(values = c("firebrick", "darkslateblue", "darkslategray", "darkolivegreen1", "darkslategray1", "lightsalmon")) +
          scale_x_continuous(breaks = c(4, 6, 8, 10, 12, 14, 16)) +
          labs(title = "Which Shoe Size Is Most Profitable?",
               x = "Shoe Size (US Men)",
               y = "Resale Profit %",
               color = "Model") + 
          theme(axis.text.x = element_text(size = 20),
                axis.text.y = element_text(size = 20),
                axis.title.x = element_text(size = 20),
                axis.title.y = element_text(size = 20),
                title = element_text(size = 20),
                legend.title = element_text(size = 20),
                legend.text = element_text(size = 15))
      } else if (input$xvar == "What Times of Day Does StockX Make Sales?") {
        ggplot(data = shoedata, aes_string(x = shoedata$time, fill = input$yvar)) +
          geom_bar(position = "stack", stat="count") + 
          scale_fill_manual(values = c("firebrick", "darkslateblue", "darkslategray", "darkolivegreen1", "darkslategray1", "lightsalmon")) +
          scale_x_discrete(breaks = c("02:00", "04:00", "06:00", "08:00", "10:00", "12:00", "14:00", "16:00", "18:00", "20:00", "22:00")) +
          labs(title = "Number of Sales at Different Times of Day (Stacked Bar Graph)",
               x = "Times of Day (24-hr Scale)",
               y = "Number of Sales",
               fill = "Model") + 
          theme(axis.text.x = element_text(size = 20),
                axis.text.y = element_text(size = 20),
                axis.title.x = element_text(size = 20),
                axis.title.y = element_text(size = 20),
                title = element_text(size = 20),
                legend.title = element_text(size = 20),
                legend.text = element_text(size = 15))
      } else if (input$xvar == "How Does Profit Change Over Time?") {
        ggplot(data = shoedata, aes_string(shoedata$timeDiff, shoedata$markUpPercent, color = input$yvar)) + 
          geom_smooth() + 
          scale_color_manual(values = c("firebrick", "darkslateblue", "darkslategray", "darkolivegreen1", "darkslategray1", "lightsalmon")) +
          geom_vline(xintercept = 0, linetype="dotted", size=1.5) +
          geom_text(aes(x = 0, label = "Release Date", y = 250), colour="black", angle = 0, vjust = 1, text=element_text(size=15)) +
          labs(title = "Comparison of Profit % of Different Shoe Models",
               x = "Days from Release Date",
               y = "Resale Profit %",
               color = "Model") + 
          theme(axis.text.x = element_text(size = 20),
                axis.text.y = element_text(size = 20),
                axis.title.x = element_text(size = 20),
                axis.title.y = element_text(size = 20),
                title = element_text(size = 20),
                legend.title = element_text(size = 20),
                legend.text = element_text(size = 15))
      } else if (input$xvar == "What Are The Total Sales Figures?") {
        ggplot(data = shoedata, aes_string(x = shoedata$timeDiff, color = input$yvar)) +
          geom_freqpoly(alpha = 0.5, size = 1.5) +
          scale_color_manual(values = c("firebrick", "darkslateblue", "darkslategray", "darkolivegreen1", "darkslategray1", "lightsalmon")) + 
          geom_vline(xintercept = 0, linetype="dotted", size=1.5) +
          geom_text(aes(x=0, label="Release Date", y=1000), colour="black", angle=0, vjust = 1, hjust = 0, text=element_text(size=15)) + 
          labs(title = "Total Number of Sales by Model",
               x = "Days From Release Date",
               y = "Total Number of Sales",
               color = "Brand") + 
          theme(axis.text.x = element_text(size = 20),
                axis.text.y = element_text(size = 20),
                axis.title.x = element_text(size = 20),
                axis.title.y = element_text(size = 20),
                title = element_text(size = 20),
                legend.title = element_text(size = 20),
                legend.text = element_text(size = 15))
      }
    }
    
  })
  
  output$Sneakers <- renderImage({
    return(list(
      src = "sneakerimage.png",
      contentType = "image/png",
      width = 750,
      height = 500,
      alt = "Face"
    ))
  }, deleteFile = FALSE)
  
  
}

shinyApp(ui = ui, server = server)