# Load packages ----
library(shiny)
library(tidyverse)


# User interface ----
ui = fluidPage(
  titlePanel("RegEx Tester"),
  sidebarLayout(
    sidebarPanel(
      fileInput("dataframe", "Data Frame"),
      selectInput("var", "Variable/Column:", character(0)),
      selectInput("class_name", "New value for class:", c("RegEx"), "RegEx"),
      conditionalPanel(
        condition = "input.class_name == 'RegEx'",
        splitLayout(
          textInput("regex_in", "RegEx In", value = "^\\d*\\.? (.*)$"),
          textInput("regex_out", "RegEx Out", value = "\\1"))),
      textOutput("test"),
      tableOutput("classes_tbl")
    ),
    mainPanel(
      conditionalPanel(
        condition = "input.class_name == 'RegEx'",
        selectInput("class", "Class:", character(0))),
      tableOutput("class_values_tbl"),
      conditionalPanel(
        condition = "input.class_name != 'RegEx'",
        checkboxGroupInput("values", "Member Values of Class:"))
    )
  )
)

# Server logic
server = function(input, output, session) {
  df = reactive({
    # loads a dataframe from and rda selected by the user
    #req(input$dataframe$datapath)
    filePath = "rdas/payroll.rda"
    load(filePath)#input$dataframe$datapath)
    # df = str_replace(input$dataframe$name, "^(.*)\\..*$", "\\1")
    # df = eval(parse(text = df)) #paste(df, '$', input$var, sep = '')))
    payroll#$team
  })
  observe({
    # gives choices of variable/columns from the data frame
    updateSelectInput(session, "var", choices = names(df()))
  })
  values = reactive({
    # sorts the chosen dataframe (df) by the values in the selected variable.
    #Also allows the input$var time to be setup before the rest of the app that uses it
    req(input$var)
    df()[order(df()[[input$var]]),] #should change order method to work better for numbers
  })
  observe({
    # gives choices of the new value for class as being defined directly by a RegEx
    # or as a value from the chosen column 
    updateSelectInput(session, "class_name", choices = c("RegEx",values()[[input$var]]))
  })
  observe({
    # gives choices of the member values of the class that will be replaced by the new value (Class Name)
    updateCheckboxGroupInput(session, "values", choices = unique(values()[[input$var]]), selected = input$class_name)
  })
  regex_in = reactive({
    # defines RegEx selecting values to be replaced
    if(input$class_name == 'RegEx'){
      return(input$regex_in)
    }
    # req(input$values)
    str_replace_all(input$values, "(\\W)","\\\\\\1") %>% # replace RegEx special characters with escaped
    paste('^',.,'$', collapse = '|', sep = '') # should modify this to eliminate input$class_name from list as it already is as desired
  })
  regex_out = reactive({
    # defines RegEx of string to replace regex_in
    
    if(input$class_name == 'RegEx'){
      return(input$regex_out)
    }
    input$class_name
  })
  classes = reactive({
    # performs the replacement to create the new value (in "new" column) and counts how many instances are in each class (have a given new value)
    validate(
      need(try(str_count(regex_in(), '\\(')==str_count(regex_in(), '\\)')), "Unmatched parenthesis; please match parenthesis to generate valid RegEx"),
      need(str_count(regex_in(), '\\(')>=str_count(regex_out(), '\\\\\\d'), "more group references than groups to be referenced"))
    values = values()
    names(values)[names(values) == input$var] = "original"
    c_names = mutate(values, new = str_replace(original, regex_in(), regex_out()))
    table = c_names %>%
      group_by(new) %>%
      summarise(values_count = n())
    table = inner_join(table, c_names, by = "new")
    table
  })
  output$classes_tbl = renderTable({
    # displays how many instances (rows) are in each class (have a given new value)
    unique(select(classes(), new, values_count))
  })
  observe({
    # gives choices of the classes/new values of which to display old values
    updateSelectInput(session, "class", choices = classes()$new)
  })
  output$class_values_tbl = renderTable({
    # displays old values of selected class
    if(input$class_name == 'RegEx'){
      class = input$class
    } else {
      class = input$class_name
    }
    classes() %>% filter(new == class) %>%
      .$original %>%
      unique()
  })
  output$test = renderText({
    str_count(regex_in(), '\\(')
  })
}

# Run the app
shinyApp(ui, server)