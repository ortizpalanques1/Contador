# Libraries ####
library(shiny)
library(lubridate)
library(tidyverse)
library(rlang)
library(beepr)

#Functions#  
time_of_the_day <- function() {
  Sys.time()
}

cuenta_ciclo <- function(text, number_1, number_2){
  as.numeric(get(paste0(text,number_1))) - as.numeric(get(paste0(text,number_2)))
}


#Variables#
the_items <- c("Normal Items", "TnS", "MSR")
item_names <- factor(the_items, levels = c("Normal Items", "TnS", "MSR"))

ui <- fluidPage(
  
  h2("Start Time"),
  textOutput("time"),
  
  div(
    h2("Ordinary Item"),
    actionButton("plus","+1"),
    actionButton("minus", "-1"),
    textOutput("value_plus")
  ),
  
  div(
    h2("Escalate to TnS"),
    actionButton("escalation","+1"),
    actionButton("minusescalation", "-1"),
    textOutput("value_escalation")
  ),
  
  div(
    h2("Send to MSR"),
    actionButton("msr","+1"),
    actionButton("minusmsr", "-1"),
    textOutput("value_msr")
  ),
  
  div(
    h2("Total Number of Items"),
    textOutput("todo"),
    downloadButton("download", "Report")
  ),
  
  textOutput("title"),
  tableOutput("tabla")
  
  #plotOutput("plot00")
  
)

server <- function(input, output, session) {

  output$time <- time_of_the_day
  h = reactiveVal(0)
  
  x = reactiveVal(0)
  observeEvent(input$plus,{
    x(x()+1) # increment x by 1
  })
  observeEvent(input$minus,{
    x(x()-1)
  })
  output$value_plus = renderText(x())
  
  y = reactiveVal(0)
  observeEvent(input$escalation,{
    y(y()+1) # increment x by 1
  })
  observeEvent(input$minusescalation,{
    y(y()-1)
  })
  output$value_escalation = renderText(y())
  
  z = reactiveVal(0)
  observeEvent(input$msr,{
    z(z()+1) # increment x by 1
  })
  observeEvent(input$minusmsr,{
    z(z()-1)
  })
  output$value_msr = renderText(z())
  
  output$todo = renderText({
    el_todo <- sum( x(), y(), z())
    el_todo
  })
  
  # observer that invalidates every second. If timer is active, decrease by one.
  guardar <- reactiveValues()
  observe({
    invalidateLater(10000, session)
    isolate({
      
      #Valores iniciales#
      valor_0 <<- 0
      valores_0 <<- c(rep(0,3))
      
      #Recoger datos#
      el_que_es <- sum( x(), y(), z())
      aqui_estamos_todos <- c(x(), y(), z())
      
      #Inicializar contadores#
      h(h()+1)
      t <- h() - 1
      
      #Enviar valores al ambiente global#
      assign(paste0('valor_', h()), el_que_es, envir = .GlobalEnv)
      assign(paste0("valores_",h()), aqui_estamos_todos, envir = .GlobalEnv)
      
      #Obtener los valores del ciclo: diferencia entre los valores totales y los del ciclo anterior#
      el_que_sera <<- cuenta_ciclo("valor_", h(), t)
      los_que_seran <<- cuenta_ciclo("valores_", h(), t)
      assign(paste0("cada_hora_", t), los_que_seran, envir = .GlobalEnv)
      
      #Crear data frames#
      los_que_son <- data.frame("Items" = item_names, "Values" = as.integer(los_que_seran))
      resumen_diario_names <- c(ls(pattern = ("cada_hora_\\d+"), envir = .GlobalEnv))
      resumen_diario_names <- str_sort(resumen_diario_names, numeric = TRUE)
      resumen_diario <- data.frame(sapply(resumen_diario_names, get))
      if(exists("cada_hora_1") & exists("cada_hora_2")){
        resumen_diario <- resumen_diario[ , 2:ncol(resumen_diario)]
        resumen_diario <<- cbind(item_names, resumen_diario)
      }
      
      
      #Vectores con nombres de variables creadas en el ambiente global#
      toditos <- c(ls(pattern = ("valor_\\d+"), envir = .GlobalEnv))
      todos_todos <- c(ls(pattern = ("valores_\\d+"), envir = .GlobalEnv))
      toditos <- str_sort(toditos, numeric = TRUE)
      todos_todos <- str_sort(todos_todos, numeric = TRUE)
      toditos_values <<- sapply(toditos, get)
      todos_todos_values <<- sapply(todos_todos, get)
      
      output$title = renderText({
        paste0(paste0("Total items in hour ", t, " = ", el_que_sera))
      })
      output$tabla = renderTable({
        los_que_son
      })
      # output$plot00 <- renderPlot({
      #   este_plot <- ggplot(los_que_son, aes(x = Items, y = Values, fill = Items))+
      #     geom_col()+
      #     labs(title = paste0("Total items in hour ", h(), " = ", el_que_es))
      #   este_plot
      # })
      if(t >= 1){beep(2)}
    })
  })
  
  output$download <- downloadHandler(
   filename = function() {
     paste(Sys.Date(), 'Edited Table.csv', sep='')
   },
   content = function(file) {
     write.csv(resumen_diario, file)
   }
  )
}

shinyApp(ui, server)