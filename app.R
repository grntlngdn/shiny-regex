# Load packages ----
library(shiny)
library(tidyverse)

# User interface ----
ui = fluidPage(
  titlePanel("title panel"),
  sidebarLayout(
    sidebarPanel(
      fileInput("dataframe", "Data Frame"),
      textInput("var", "Variable/Column"),
      splitLayout(textInput("regex_in", "RegEx In", value = "^\\d*\\.? (.*)"),
        textInput("regex_out", "RegEx Out", value = "\\1")),
      # textOutput("test"),
      tableOutput("classes_tbl")
    ),
    mainPanel(
      htmlOutput("class_selector"),
      tableOutput("class_values_tbl")
    )
  )
)

# Server logic
server = function(input, output) {
  values = reactive({
    filePath = "rdas/payroll.rda"
    load(filePath)#input$dataframe$datapath)
    # df = str_replace(input$dataframe$name, "^(.*)\\..*", "\\1")
    # df = eval(parse(text = df)) #paste(df, '$', input$var, sep = '')))
    payroll#$team
  })
  classes = reactive({
    names = mutate(values(), original = team, team = str_replace(team, input$regex_in, input$regex_out)) 
    table = names %>%
      group_by(team) %>%
      summarise(values_count = n())
    table = inner_join(table, names, by = 'team')
    table
  })
  output$classes_tbl = renderTable({
    unique(select(classes(), team, values_count))
  })
  output$class_selector = renderUI({
    selectInput(inputId = "class",
    label = "Class:",
    choices = classes()$team)
  })
  output$class_values_tbl = renderTable({
    classes() %>% filter(team == input$class) %>%
      .$original
  })
  output$test = renderText({
    #input$dataframe$datapath
    # input$dataframe$datapath
  })
}

# Run the app
shinyApp(ui, server)