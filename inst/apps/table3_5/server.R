server = function(input, output, session) {

    output$table5 <- DT::renderDataTable({
      
a<-rep(c(.005,.01,.05,.1), 4)
b<-round(rep(c(.995,.99,.95,.9),each = 4), digits = 3)
o.80<-c(2.86,2.84,2.76,2.72,2.84,2.81,2.73,2.68,2.76,2.73,2.62,2.56,2.72,2.68,2.56,2.48)
o.90<-c(3.12,3.10,3.03,3.00,3.10,3.07,3.00,2.96,3.03,3.00,2.91,2.85,3.00,2.96,2.85,2.79)
o.95<-c(3.36,3.34,3.28,3.25,3.34,3.31,3.25,3.21,3.28,3.25,3.16,3.11,3.25,3.21,3.11,3.06)
o.99<-c(3.85,3.83,3.77,3.75,3.83,3.81,3.75,3.72,3.77,3.75,3.68,3.64,3.75,3.72,3.64,3.59)

table.3.5<-data.frame(a,b,o.80,o.90,o.95,o.99)

colnames(table.3.5)<-c("a","b",
                       HTML("&alpha; = 0.80"),
                       HTML("&alpha; = 0.90"),
                       HTML("&alpha; = 0.95"),
                       HTML("&alpha; = 0.99"))

DT::datatable(table.3.5, escape = FALSE, options = list(pageLength = 4))
})
}