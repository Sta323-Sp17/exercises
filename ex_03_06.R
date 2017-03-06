library(dplyr)
library(shiny)
library(ggplot2)

# Code book - http://www2.stat.duke.edu/~cr173/Sta323_Sp17/data/movies/movies.html

movies = read.csv("http://www.stat.duke.edu/~cr173/Sta323_Sp17/data/movies/movies.csv", 
                  stringsAsFactors = FALSE) %>% tbl_df()

shinyApp(
  ui = fluidPage(
    sidebarLayout(
      sidebarPanel(
        selectInput("var_x", label = "x-axis variable", choices = names(movies), selected = "imdb_rating"),
        selectInput("var_y", label = "y-axis variable", choices = NA)
      ),
      mainPanel(
        h4("Plot of the movies data"),
        plotOutput("main_plot"),
        h4("X Summary statistics:"),
        tableOutput("x_summary"),
        conditionalPanel(
          "input.var_y != 'None'",
          h4("Y Summary statistics:"),
          tableOutput("y_summary")
        )
          
      )
    )
  ),
  server = function(input, output, session)
  {
    observe({
      y_choices = c("None", names(movies))
      new_y_choices = setdiff(y_choices, input$var_x)
      
      updateSelectInput(session, inputId = "var_y", choices=new_y_choices)
    })
    
    output$main_plot = renderPlot({
      
      if (input$var_y == "None")
      { 
        base = ggplot(movies, aes_string(x=input$var_x))
        
        if (is.numeric(movies[[input$var_x]]))
        {
          base + geom_histogram()
        } else {
          base + geom_bar(aes_string(fill=input$var_x))
        }
      } else {
        if (is.numeric(movies[[input$var_x]]) & is.numeric(movies[[input$var_y]]))
        {
          ggplot(movies, aes_string(x=input$var_x, y=input$var_y)) +
            geom_point()
        }
      }
    })
    
    output$x_summary = renderTable({
      data.frame(
        mean = mean(movies[[input$var_x]], na.rm=TRUE),
        med = median(movies[[input$var_x]], na.rm=TRUE),
        iqr = IQR(movies[[input$var_x]], na.rm=TRUE),
        min = min(movies[[input$var_x]], na.rm=TRUE),
        max = max(movies[[input$var_x]], na.rm=TRUE)
      )
    })
    
    output$y_summary = renderTable({
      if (input$var_y != "None")
      {
        data.frame(
          mean = mean(movies[[input$var_y]], na.rm=TRUE),
          med = median(movies[[input$var_y]], na.rm=TRUE),
          iqr = IQR(movies[[input$var_y]], na.rm=TRUE),
          min = min(movies[[input$var_y]], na.rm=TRUE),
          max = max(movies[[input$var_y]], na.rm=TRUE)
        )
      }
    })
  }
)