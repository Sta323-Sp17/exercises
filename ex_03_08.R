library(dplyr)
library(shiny)
library(ggplot2)
library(purrr)

# Code book - http://www2.stat.duke.edu/~cr173/Sta323_Sp17/data/movies/movies.html

movies = read.csv("http://www.stat.duke.edu/~cr173/Sta323_Sp17/data/movies/movies.csv", 
                  stringsAsFactors = FALSE) %>% tbl_df()

cat_vars = (!map_lgl(movies, is.numeric)) %>% (function(x) names(movies)[x])

shinyApp(
  ui = fluidPage(
    sidebarLayout(
      sidebarPanel(
        selectInput("var_x", label = "x-axis variable", choices = names(movies), selected = "imdb_rating"),
        selectInput("var_y", label = "y-axis variable", choices = NA),
        sliderInput("year", label = "Theatrical release year filter", 
                    min = min(movies$thtr_rel_year), max = max(movies$thtr_rel_year), 
                    value = range(movies$thtr_rel_year), step = 1, sep = ""),
        selectInput("cat_var", label = "Catergorical variable to filter", choices = c("None",cat_vars)),
        conditionalPanel(
          "input.cat_var != 'None'",
          selectInput("cat_var_value", label = "Value to filter on", choices = NA)
        )            
      ),
      mainPanel(
        textOutput("n_movies"),
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
    
    observe({
      updateSelectInput(session, inputId = "cat_var_value", choices=sort(unique(movies[[input$cat_var]])))
    })
    
    
    movies_filtered = reactive({
      start_year = input$year[1]
      end_year   = input$year[2]
      
      m = movies %>% filter(thtr_rel_year >= start_year, thtr_rel_year <= end_year)
       
      if (input$cat_var != "None")
      {
        # Base R Solution
        #sub = m[[input$cat_var]] == input$cat_var_value
        #m = m[sub,]
        
        # Non-working dplyr
        #m = m %>% filter(input$cat_var == input$cat_var_value)
      
        # Workign dplyr  
        m = m %>% filter_(paste0(input$cat_var," == '", input$cat_var_value, "'"))   
      }
      
      return(m)
    })
      
      
    
    output$main_plot = renderPlot({
      
      if (input$var_y == "None")
      { 
        base = ggplot(movies_filtered(), aes_string(x=input$var_x))
        
        if (is.numeric(movies[[input$var_x]]))
        {
          base + geom_histogram()
        } else {
          base + geom_bar(aes_string(fill=input$var_x))
        }
      } else {
        if (is.numeric(movies[[input$var_x]]) & is.numeric(movies[[input$var_y]]))
        {
          ggplot(movies_filtered(), aes_string(x=input$var_x, y=input$var_y)) +
            geom_point()
        } 
        else if (!is.numeric(movies[[input$var_x]]) & is.numeric(movies[[input$var_y]]))
        {
          ggplot(movies_filtered(), aes_string(x=input$var_x, y=input$var_y)) +
            geom_boxplot()
        }
        else if (is.numeric(movies[[input$var_x]]) & !is.numeric(movies[[input$var_y]]))
        {
          ggplot(movies_filtered(), aes_string(y=input$var_x, x=input$var_y)) +
            geom_boxplot() + 
            coord_flip()
        } else {
          ggplot(movies_filtered(), aes_string(x=input$var_x, fill=input$var_y)) +
            geom_bar()
        }
      }
    })
    
    output$x_summary = renderTable({
      data.frame(
        mean = mean(movies_filtered()[[input$var_x]], na.rm=TRUE),
        med = median(movies_filtered()[[input$var_x]], na.rm=TRUE),
        iqr = IQR(movies_filtered()[[input$var_x]], na.rm=TRUE),
        min = min(movies_filtered()[[input$var_x]], na.rm=TRUE),
        max = max(movies_filtered()[[input$var_x]], na.rm=TRUE)
      )
    })
    
    output$y_summary = renderTable({
      if (input$var_y != "None")
      {
        data.frame(
          mean = mean(movies_filtered()[[input$var_y]], na.rm=TRUE),
          med = median(movies_filtered()[[input$var_y]], na.rm=TRUE),
          iqr = IQR(movies_filtered()[[input$var_y]], na.rm=TRUE),
          min = min(movies_filtered()[[input$var_y]], na.rm=TRUE),
          max = max(movies_filtered()[[input$var_y]], na.rm=TRUE)
        )
      }
    })
  }
)