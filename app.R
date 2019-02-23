#options(shiny.reactlog = TRUE)
# Load packages ----
library(shiny)
library(tidyverse)

regExInput = function(id, label = "Replacement Step"){
  ns = NS(id)
  
  tagList(
    h5(label),
    selectInput(ns("class_name"), "New value for class:", c("RegEx"), "RegEx"),
    conditionalPanel(
      condition = "input.class_name == 'RegEx'",
      ns = ns,
      splitLayout(
        textInput(ns("regex_in"), "RegEx In", value = "^(.*)$"),#"^\\d*\\.? (.*)$"),
        textInput(ns("regex_out"), "RegEx Out", value = "\\1"))),
    conditionalPanel( # it's cluttered to have this element hear, but I wasn't sure how to put one element of module in different location. maybe have this as some kind of popup
      condition = "input.class_name != 'RegEx'",
      ns = ns,
      checkboxGroupInput(ns("values"), "Member Values of Class:"))
  )
}

regEx = function(input, output, session, values, var = "new"){
  rv = reactiveValues()
  observe({
    # gives choices of the new value for class as being defined directly by a RegEx
    # or as a value from the chosen column 
    # regex_vec()
    updateSelectInput(session, "class_name", choices = c("RegEx",values()[[var]]))
  })
  observe({
    # gives choices of the member values of the class that will be replaced by the new value (Class Name)
    updateCheckboxGroupInput(session, "values", choices = unique(values()[[var]]), selected = input$class_name)
  })
  regex_in = reactive({
    # defines RegEx selecting values to be replaced
    req(input$class_name)
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
  regex_vec = reactive({
    # performs validation on the regex components (otherwise slight asynchrony will cause app to crash) and returns vector of regex arguments for replace
    rv$values = values()
    validate(
      need(try(str_count(regex_in(), '\\(')==str_count(regex_in(), '\\)')), "Unmatched parenthesis; please match parenthesis to generate valid RegEx"),
      #This needs to be improved to sanitize input and avoid injection
      need(str_count(regex_in(), '\\(')>=str_count(regex_out(), '\\\\\\d'), "more group references than groups to be referenced"))
    c(regex_in(),regex_out())
  })
  updateSelectInput(session, "class_name", choices = c("RegEx",isolate(rv$values)[[var]]))
  # updateSelectInput(session, "class_name", choices = c("RegEx",isolate(values()[[var]])))
  return(regex_vec)
}

# User interface ----
ui = fluidPage(
  titlePanel("RegEx Tester"),
  sidebarLayout(
    sidebarPanel(
      fileInput("data_frame", "Data Frame"),
      selectInput("var", "Variable/Column:", character(0)),
      actionButton("add", "Add UI"),
      # regExInput("initial_RegEx", "Initial replacement step:"),
      textOutput("test"),
      tableOutput("classes_tbl")
    ),
    mainPanel(
      # conditionalPanel(
      #   condition = "input.class_name == 'RegEx'",
        selectInput("class", "Class:", character(0)),#),
      tableOutput("class_values_tbl")#,
    )
  )
)

# Server logic
server = function(input, output, session) {
  regex_values = reactiveValues()
  
  df = reactive({
    # loads a data frame from and rda selected by the user
    #req(input$data_frame$datapath)
    filePath = "rdas/payroll.rda"
    load(filePath)#input$data_frame$datapath)
    # df = str_replace(input$data_frame$name, "^(.*)\\..*$", "\\1")
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
    df = df()
    names(df)[names(df) == input$var] = "original"
    df[order(df[["original"]]),] %>% #should be changed so order method works better for numbers
      mutate(new = original)
  })
  observeEvent({
    # run once at start
    if(length(reactiveValuesToList(regex_values)) < 1){
      return(T)
    }
    # run again when most recently created input is no longer zero
    req(regex_values[[as.character(length(reactiveValuesToList(regex_values)))]]())
    if(!(identical(regex_values[[as.character(length(reactiveValuesToList(regex_values)))]](), c("^(.*)$", "\\1")))){
      regex_values[[as.character(length(reactiveValuesToList(regex_values)))]]()
    }
    }, {
      n = as.character(length(reactiveValuesToList(regex_values)) + 1)
      insertUI(
        selector = "#test",
        where = "beforeBegin",
        ui = regExInput(n, paste0("Replacement step ", n))
      )
      regex_values[[n]] = callModule(regEx, n, values = values)
      # cat(length(reactiveValuesToList(regex_values)))
  })
  # for(i in seq_along(isolate(regex_values))){
  #   regex_values[[as.character(i)]] = callModule()
  # }
  classes = reactive({
    # performs the replacement to create the new value (in "new" column) and counts how many instances are in each class (have a given new value)
      c_names = values()
      # i = 1
      for(i in 1:(length(reactiveValuesToList(regex_values)))){
        req(regex_values[[as.character(i)]]())
        c_names = mutate(c_names, new = str_replace(new, regex_values[[as.character(i)]]()[1], regex_values[[as.character(i)]]()[2]))
      }
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
    # is it worth it to make this automatically show the last input$class_names class when last RegEx was not custom. try it after adding insertUi components
    # if(input$class_name == 'RegEx'){
      class = input$class
    # } else {
    #   class = input$class_name
    # }
    classes() %>% filter(new == class) %>%
      .$original %>%
      unique()
  })
  output$test = renderText({
    length(reactiveValuesToList(regex_values))
    #toString(regex_values[[as.character(length(reactiveValuesToList(regex_values)))]]())
  })
}

# Run the app
shinyApp(ui, server)