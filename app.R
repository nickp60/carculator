
library(tidyverse)
library(shiny)

theme_set(theme_bw())


# Define UI for application that draws a histogram
uiplot_choices <- c("year", "miles", "miles_remaining", "cost", 
                    "cost_per_remaining_mile", "predicted_remaining_years", "average_lifespan")

ui <- fluidPage(

    # Application title
    titlePanel("cars"),
    p("Looking to find the best value car?  Make a google spreadsheet with some candidate car names, years, mileage, and estimated lifespan. Use this to calculate different value metrics"),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            shiny::textInput("url", label = "Spreadsheet URL", value = "https://docs.google.com/spreadsheets/d/1YZgLSlUSBICQ429X_RAVJkLmBMZMFkyqPG4LxH5RMoM/edit?usp=sharing"),
            shiny::numericInput("year", label = "Year", value = 2025),
            shiny::numericInput("miles", label="Miles", value=0),
            shiny::numericInput("average_lifespan",label = "Cars expected mileage lifespan",  value = 200000),
            shiny::numericInput("price", label="Price",  value = 30000),
            selectInput(inputId = "X", label = "X", choices = uiplot_choices, selected = "predicted_remaining_years"),
            selectInput(inputId = "Y", label = "Y", choices = uiplot_choices, selected = "cost_per_remaining_mile"),
            selectInput(inputId = "C", label = "Color", choices =uiplot_choices, selected = "cost"),
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           plotOutput("centPlot")
        )
    )
)
run_calcs <-function(df, input){
  df %>% 
    bind_rows(
      data.frame(
        "name"="this car",
        "miles"=input$miles,
        "year"=input$year,
        "cost"=input$price,
        "average_lifespan"=input$average_lifespan)) %>%
    mutate(
      miles_remaining=average_lifespan-miles,
      cost_per_remaining_mile=cost/miles_remaining,
      predicted_remaining_years=miles_remaining/14000
    ) 
}
server <- function(input, output) {
  df <- reactive(df <- gsheet::gsheet2tbl(input$url) %>% 
                   janitor::clean_names())
  output$distPlot <- renderPlot({
    ggplot(df() %>% run_calcs(input) %>%
             dplyr::rename(x=input$X, y=input$Y, color=input$C),
           aes(x=x, y=y, color=color, label=name)) + 
      geom_point() + ggrepel::geom_text_repel()+
      labs(x=input$X, y=input$Y, color=input$C)  + 
      scale_color_viridis_c(option="C")
    
      
    })
  output$centPlot <- renderPlot({
    ggplot(df() %>% run_calcs(input),
           aes(x=reorder(name, cost_per_remaining_mile), y=cost_per_remaining_mile, color=cost)) + 
      annotate("point", x="this car", y=input$price/(input$average_lifespan -input$miles), color="yellow" , size=4) +
      geom_point() +
      labs(y="Cost per remaining mile", x="Car")+
      theme(axis.text.x=element_text(angle=45, hjust=1))

    
  })
}

shinyApp(ui = ui, server = server)
