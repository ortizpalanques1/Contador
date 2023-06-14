period <- c()
cierto <- TRUE
while(cierto == TRUE){
  Sys.sleep(2)
  alfa <- runif(1)
  period <- c(period, alfa)
  print(period)
}


alpha <- Sys.time()
i <- 1
while(i < 21){
  Sys.sleep(1)
  omega <- Sys.time()
  print(omega)
  i <- i+1
}



el_todo_f <- function(a, b, c){
  additive <- a + b + c
  return(additive)
}
el_todo_f(4, 5, 6)

the_items <- c("Normal Items", "TnS", "MSR")
item_names <- factor(the_items, levels = c("Normal Items", "TnS", "MSR"))

#Probar funcion
#Original
as.numeric(get(paste0("valor_",h()))) - as.numeric(get(paste0("valor_",t)))

cuenta_ciclo <- function(text, number_1, number_2){
  as.numeric(get(paste0(text,number_1()))) - as.numeric(get(paste0(text,number_2)))
  return()
}