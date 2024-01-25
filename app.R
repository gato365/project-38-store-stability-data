library(shiny)
library(jsonlite)
library(lubridate)
library(tidyverse)
library(shinybusy)
library(shinyauthr)
library(config)
library(mongolite)
library(shinythemes)
library(shinyWidgets)
library(shinyjs)
library(shinymaterial)

## Connect to the database

username <- Sys.getenv("username")
password <- Sys.getenv("password")
data_base <- Sys.getenv("data_base")
cluster <- Sys.getenv("cluster")
url <- paste0("mongodb+srv://",username,":",password,"@",cluster,".rjzoaxj.mongodb.net/",data_base,"?retryWrites=true&w=majority")
mongo <- mongo(url = url,
               collection = "emans_info",
               db = "stability")







ui <- material_page(
  title = "Daily Tracker",
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
  
  shinyjs::useShinyjs(),
  
  material_row(
    material_column(
      width = 12,
      align = "center",
      material_card(
        title = h2("Daily Tracker"),
        depth = 3,
        
        material_row(
          material_column(
            width = 6,
            shinyWidgets::airDatepickerInput(
              inputId = "date",
              label = "Select date:",
              placeholder = "Select date",
              autoClose = TRUE,
              clearButton = TRUE,
              todayButton = TRUE,
              value = Sys.Date()
            )
          ),
          material_column(
            width = 6,
            shinyWidgets::pickerInput(
              inputId = "timeBlock",
              label = h5("What block are you measuring?"),
              choices = c("4 am", "7:45 am", "10:45 am", "12 pm", "3 pm", "6 pm", "8:30 pm"),
              options = list(`actions-box` = TRUE, `selected-text-format` = "count > 3", `count-selected-text` = "{0} blocks selected")
            )
          )
        ),
        
        conditionalPanel(
          condition = "input.timeBlock == '4 am'",
          material_row(
            material_column(
              width = 6,
              shinyWidgets::textInputIcon(
                inputId = "wakeUpTime",
                label = "Wake up time:",
                # icon = icon("alarm"),
                value = "HH:MM"
              )
            ),
            material_column(
              width = 6,
              shinyWidgets::numericInputIcon(
                inputId = "weight",
                label = "Weight:",
                icon = icon("weight"),
                value = 0
              )
            )
          )
        ),
        material_row(
          material_column(
            width = 4,
            shiny::selectInput(
              inputId = "mood",
              label = h5("Mood:"),
              choices = c("Positive", "Neutral", "Negative"),
              selected = "Neutral"
            )
          ),
          material_column(
            width = 4,
            shiny::selectInput(
              inputId = "goalOutcome",
              label = h5("Goal Outcome:"),
              choices = c("G+", "G", "G-"),
              selected = "G"
            )
          ),
          material_column(
            width = 4,
            shiny::selectInput(
              inputId = "foodQuality",
              label = h5("Food Quality:"),
              choices = c("High", "Medium", "Low"),
              selected = "Medium"
            )
          )
        ),
        
        material_row(
          material_column(
            width = 4,
            shinyWidgets::pickerInput(
              inputId = "drink",
              label = h5("Did you drink?"),
              choices = c("Yes", "No"),
              selected = "No",
              options = list(`style` = "btn-primary")
            )
          ),
          material_column(
            width = 4,
            shinyWidgets::pickerInput(
              inputId = "mj",
              label = h5("Did you MJ?"),
              choices = c("Yes", "No"),
              selected = "No",
              options = list(`style` = "btn-primary")
            )
          ),
          material_column(
            width = 4,
            shinyWidgets::pickerInput(
              inputId = "spendMoney",
              label = h5("Did you spend money?"),
              choices = c("Yes", "No"),
              selected = "No",
              options = list(`style` = "btn-primary")
            )
          )
        ),
        
        
        
        material_row(
          material_column(
            width = 12,
            shiny::textAreaInput(
              inputId = "notes",
              label = h4("Notes:"),
              value = "",height = '100px',width = '1000px',
              rows = 3
            )
          )
        ),
        
        
        material_row(
          material_column(
            width = 12,
            align = "center",
            shinyWidgets::actionBttn(
              inputId = "saveData",
              label = "Save Data",
              style = "float",
              color = "success",
              size = "lg",
              block = TRUE,
              icon = icon("save")
            )
          )
        )
      )
    )
  ),
  
  #   # Output the saved JSON
  verbatimTextOutput("jsonData")
)



# ui <- fluidPage(
#   titlePanel("Daily Tracker"),
#   
#   theme = shinythemes::shinytheme("darkly"),
#   # Display current date and time
#   textOutput("currentDateTime"),
#   
#   # Dropdown for time block selection
#   selectInput("timeBlock", "What block are you measuring?", 
#               choices = c("4 am", "7:45 am", "10:45 am", "12 pm", "3 pm", "6 pm", "8:30 pm")),
#   
#   # Conditional inputs based on the selected time block
#   conditionalPanel(
#     condition = "input.timeBlock == '4 am'",
#     textInput("wakeUpTime", "Wake up time:", value = "HH:MM"),
#     numericInput("weight", "Weight:", min = 0, value = 0)
#   ),
#   
#   # Remaining inputs
#   selectInput("goalOutcome", "What is your goal outcome?", choices = c("G+", "G", "G-")),
#   selectInput("foodOutcome", "What is your food outcome?", choices = c("M+", "M", "M-")),
#   selectInput("foodQuality", "What is your food quality outcome?", choices = c("L", "M", "H")),
#   radioButtons("drink", "Did you drink?", choices = c("Yes", "No")),
#   radioButtons("mj", "Did you MJ?", choices = c("Yes", "No")),
#   radioButtons("spendMoney", "Did you spend money outside when you are supposed to?", choices = c("Yes", "No")),
#   textAreaInput("notes", "Notes:", ""),
#   
#   # Button to save data
#   actionButton("saveData", "Save Data"),
#   
#   # Output the saved JSON
#   verbatimTextOutput("jsonData")
# )

server <- function(input, output, session) {
  
  
  # Display current date and time
  output$currentDateTime <- renderText({
    current_time <- Sys.time()
    formatted_time <- format(current_time, "%Y-%m-%d %H:%M:%S")
    paste("Current date and time:", formatted_time)
  })
  
  observeEvent(input$saveData, {
    # Create a list from inputs
    current_time <- Sys.time()
    formatted_time <- format(current_time, "%Y-%m-%d %H:%M:%S")
    inputData <- list(
      specificTime = formatted_time,
      timeBlock = input$timeBlock,
      wakeUpTime = input$wakeUpTime,
      weight = input$weight,
      goalOutcome = input$goalOutcome,
      foodQuality = input$foodQuality,
      drink = input$drink,
      mj = input$mj,
      spendMoney = input$spendMoney,
      notes = input$notes
      
      
    )
    
    
    # Convert the list to JSON
    jsonData <- jsonlite::toJSON(inputData, auto_unbox = TRUE, pretty = TRUE)
    
    # Insert the JSON data into the MongoDB collection
    result <- mongo$insert(jsonData,tls = FALSE)
    
    
    # Output the JSON data
    output$jsonData <- renderText({
      paste0(jsonData)
      
      
      
    })
  })
}

shinyApp(ui, server)





