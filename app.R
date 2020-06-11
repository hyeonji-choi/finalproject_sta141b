library(keyring)
library(shiny)
library(shinythemes)
library(dplyr)
library(ggrepel)
library(tidyr)
library(shinycssloaders)
library(DT)
library(knitr)
library(tidyverse)
library(jsonlite)
library(httr)
library(lubridate)



###API

#usethis::edit_r_environ("project")
readRenviron(".Renviron")

#American Cuisine Data
r_american <- GET(
  "https://api.edamam.com/search?from=0&to=100",
  query = list(
    app_id = Sys.getenv("EDAMAM_ID"),
    app_key = Sys.getenv("EDAMAM_KEY"),
    q = "american"
  )
)
stop_for_status(r_american)
json_american <- content(r_american, as = "text", encoding = "UTF-8")
result_american <- fromJSON(json_american)

american <- result_american$hits %>%
  mutate(cuisinetype = "American") %>%
  mutate(calorieperyield = result_american$hits$recipe$calories / result_american$hits$recipe$yield)

american_df <- as.data.frame(american) 

american_df <- american_df %>%
  mutate(label = american_df$recipe$label) %>%
  mutate(url = american_df$recipe$url) %>%
  mutate(image = american_df$recipe$image) %>%
  mutate(source = american_df$recipe$source) %>%
  mutate(yield = american_df$recipe$yield) %>%
  mutate(calorie = american_df$recipe$calorie) %>%
  mutate(totaltime = american_df$recipe$totalTime) %>%
  mutate(health = american_df$recipe$healthLabels) %>%
  select(-recipe, -bookmarked, -bought)

#Asian Cuisine Data
r_asian <- GET(
  "https://api.edamam.com/search?from=0&to=100",
  query = list(
    app_id = Sys.getenv("EDAMAM_ID"),
    app_key = Sys.getenv("EDAMAM_KEY"),
    q = "asian"
  )
)
stop_for_status(r_asian)

json_asian <- content(r_asian, as = "text", encoding = "UTF-8")
result_asian <- fromJSON(json_asian)

asian <- result_asian$hits %>%
  mutate(cuisinetype = "Asian") %>%
  mutate(calorieperyield = result_asian$hits$recipe$calories / result_asian$hits$recipe$yield)

asian_df <- as.data.frame(asian) 

asian_df <- asian_df %>%
  mutate(label = asian_df$recipe$label) %>%
  mutate(url = asian_df$recipe$url) %>%
  mutate(image = asian_df$recipe$image) %>%
  mutate(source = asian_df$recipe$source) %>%
  mutate(yield = asian_df$recipe$yield) %>%
  mutate(calorie = asian_df$recipe$calorie) %>%
  mutate(totaltime = asian_df$recipe$totalTime) %>%
  mutate(health = asian_df$recipe$healthLabels) %>%
  select(-recipe, -bookmarked, -bought)


#Mediterranean Cuisine Data
r_mediterranean <- GET(
  "https://api.edamam.com/search?from=0&to=100",
  query = list(
    app_id = Sys.getenv("EDAMAM_ID"),
    app_key = Sys.getenv("EDAMAM_KEY"),
    q = "mediterranean"
  )
)
stop_for_status(r_mediterranean)
json_mediterranean <- content(r_mediterranean, as = "text", encoding = "UTF-8")
result_mediterranean <- fromJSON(json_mediterranean)

mediterranean <- result_mediterranean$hits %>%
  mutate(cuisinetype = "Mediterranean") %>%
  mutate(calorieperyield = result_mediterranean$hits$recipe$calories / result_mediterranean$hits$recipe$yield)

mediterranean_df <- as.data.frame(mediterranean) 

mediterranean_df <- mediterranean_df %>%
  mutate(label = mediterranean_df$recipe$label) %>%
  mutate(url = mediterranean_df$recipe$url) %>%
  mutate(image = mediterranean_df$recipe$image) %>%
  mutate(source = mediterranean_df$recipe$source) %>%
  mutate(yield = mediterranean_df$recipe$yield) %>%
  mutate(calorie = mediterranean_df$recipe$calorie) %>%
  mutate(totaltime = mediterranean_df$recipe$totalTime) %>%
  mutate(health = mediterranean_df$recipe$healthLabels) %>%
  select(-recipe, -bookmarked, -bought)


#Mexican Cuisine Data
r_mexican <- GET(
  "https://api.edamam.com/search?from=0&to=100",
  query = list(
    app_id = Sys.getenv("EDAMAM_ID"),
    app_key = Sys.getenv("EDAMAM_KEY"),
    q = "mexican"
  )
)
stop_for_status(r_mexican)
json_mexican <- content(r_mexican, as = "text", encoding = "UTF-8")
result_mexican <- fromJSON(json_mexican)

mexican <- result_mexican$hits %>%
  mutate(cuisinetype = "Mexican") %>%
  mutate(calorieperyield = result_mexican$hits$recipe$calories / result_mexican$hits$recipe$yield)

mexican_df <- as.data.frame(mexican) 

mexican_df <- mexican_df %>%
  mutate(label = mexican_df$recipe$label) %>%
  mutate(url = mexican_df$recipe$url) %>%
  mutate(image = mexican_df$recipe$image) %>%
  mutate(source = mexican_df$recipe$source) %>%
  mutate(yield = mexican_df$recipe$yield) %>%
  mutate(calorie = mexican_df$recipe$calorie) %>%
  mutate(totaltime = mexican_df$recipe$totalTime) %>%
  mutate(health = mexican_df$recipe$healthLabels) %>%
  select(-recipe, -bookmarked, -bought)

completeset$ingredients %>%
  count()


completeset<- rbind(american_df,asian_df,mediterranean_df,mexican_df)

cuisinelist <- completeset %>%
  select(cuisinetype) %>%
  unique()

##SHINY

#conditionalpanel <- different text input for diff input

##helpful links
#- https://www.youtube.com/watch?v=Gyrfsrd4zK0


ui <- fluidPage(
  navbarPage("What Should I Cook?", theme = shinytheme("united"),
             
             #Recipe Tab 
             tabPanel("At-Home Recipes", fluid = TRUE, icon = icon("book-open"),
                      #tags$style(button_color_css),
                      sidebarLayout(
                        sidebarPanel(
                          titlePanel("Recipes"),
                          
                          # Select cuisine type (dropdown menu)
                          selectInput(inputId = "cuisineType",
                                      label = "Select the Cuisine Type",
                                      choices = cuisinelist,
                                      width = "220px"),
                          
                          # time restriction
                          sliderInput(inputId = "timeMax",
                                      label = "How much time do you have?",
                                      min = 0, max = 200,
                                      value = NULL,
                                      width = "220px"),
                          
                          helpText("Enter time in minute"),
                          
                          # calorie restriction
                          
                          fluidRow(column(5,
                                          textInput(inputId = "calorieMin",
                                                    label = "Minimum calories:",
                                                    value = NULL,
                                                    width = "100px")
                          ),
                          column(5, ofset = 3,
                                 textInput(inputId = "calorieMax",
                                           label = "Maximum Calories:",
                                           value = NULL,
                                           width = "100px")
                          )),
                          
                          helpText("Enter your desired range of calories in a range from 0kcal to 600kcal.")
                          
                        ),
                        
                        mainPanel(
                          textOutput("recipeText2"),
                          htmlOutput("recipeImage2"),
                          DT::dataTableOutput("recipeTable")
                        )
                      )
             ),
             
             ##About tab
             tabPanel("About", fluid = TRUE, icon = icon("info-circle"),
                      h4(p("About the Project")),
                      h5(p("This project is intended to help users with their decision blah blah"))
             )
  ))

# Define server
server <- function(input, output, session) {
  
  ##reactive
  
  inputData <- reactive({
    req(input$cuisineType)
    completeset %>%
      filter(cuisinetype %in% input$cuisineType) %>%
      filter(calorieperyield >= input$calorieMin, calorieperyield <= input$calorieMax) %>%
      filter(totaltime <= input$timeMax)
  })
  
  top1 <- reactive({
    req(input$cuisineType)
    completeset %>%
      filter(cuisinetype == input$cuisineType) %>%
      filter(calorieperyield >= input$calorieMin, calorieperyield <= input$calorieMax) %>%
      filter(totaltime <= input$timeMax) %>%
      head(1)
  })
  
  ##recipe 1 text (textOutput)
  output$recipeText1 <- renderText({
    req(input$calorieMax)
    req(input$calorieMin)
    req(input$cuisineType)
    req(input$timeMax)
    "What you can cook with the least number of ingredients"
  })
  
  ##recipe 1 image (htmlOutput)
  imgurl <- reactive({
    inputData() %>%
      select(image) %>%
      head(1)
  })
  output$recipeImage1 <- renderUI({tags$img(src = imgurl())})
  
  ##recipe 2 text
  output$recipeText2 <- renderText({
    req(input$cuisineType)
    req(input$timeMax)
    req(input$calorieMin)
    req(input$calorieMax)
    title <- sprintf(
      "Recipe Spotlight for %s Cuisine under %s minutes and between %s kcal and %s kcal:",
      input$cuisineType,
      input$timeMax,
      input$calorieMin,
      input$calofirMax)
  })
  
  ##recipe 2 image
  
  #  output$inputData <- renderText({
  #   src <- inputData %>%
  #    select(image) %>%
  #   head(1)
  #c('<img src="',src,'">')
  #})
  
  ##recipe table (dataTableOutput)
  
  output$recipeTable <- DT::renderDataTable({
    DT::datatable(
      unique(inputData()),
      colnames = c("Name" = "label", "Time" = "totaltime", "Calorie per serving" = "calorieperyield"),
      rownames = FALSE,
      options = list(
        columnDefs = list(list(className = 'dt-center', targets = 5)),
        pageLength = 5,
        lengthMenu = c(5, 10, 15, 20)
      ))})
  
}

shinyApp(ui = ui, server = server)
