

# Load libraries ----------------------------------------------------------


library(shiny)
library(dplyr)
library(stringr)
library(DT)


# Import data -------------------------------------------------------------





# App ---------------------------------------------------------------------


# Client-side
readRDS("df_thoughts_long")
readRDS("labels")

ui <- fluidPage(
  # Application title
  titlePanel("Topic explorer"),

  # Sidebar with a topic selector
  sidebarLayout(
    sidebarPanel(
      selectInput("topic_number",
        "Select topic number",
        choices = seq(1:70)
      ),
      textOutput("heading")
    ),

    # Show table with sample texts
    mainPanel(
      tableOutput("topic_table")
    )
  )
)

# Server-side
readRDS("df_thoughts_long")
readRDS("labels")

server <- function(input, output) {

  # define table
  output$topic_table <- renderTable({
    # get the right column based on input
    df_thoughts_long %>%
      # filter topic number
      filter(topic == input$topic_number) %>%
      # discard topic column
      select(-topic) %>%
      # add row number
      mutate(id = row_number()) %>%
      # move it to the front
      relocate(id, .before = title)
  })

  # get heading with topic frex terms
  output$heading <- renderText({
    paste0("Topic ", input$topic_number, ": ", str_c(labels$frex[eval(parse(text = input$topic_number)), 1:10], collapse = ", "))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
