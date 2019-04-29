server = function(input, output, session) {
  
geng.t <- reactive({ signif(seq(min(input$range.geng), max(input$range.geng), length = 500), digits = 4)})
geng.p <- signif(seq(0, 1, length = 500), digits = 4) 
geng.C <- reactive({ pgeng(geng.t(), input$theta.geng, input$beta.geng, input$kappa.geng)})
geng.P <- reactive({ dgeng(geng.t(), input$theta.geng, input$beta.geng, input$kappa.geng)})
geng.R <- reactive({ 1-geng.C()})
geng.h <- reactive({ exp(log(geng.P())-log(geng.R()))})
geng.H <- reactive({ -1*log(1-pgeng(geng.t(), input$theta.geng, input$beta.geng, input$kappa.geng))})
geng.Q <- reactive({ qgeng(geng.p, input$theta.geng, input$beta.geng, input$kappa.geng)})
geng.df <- reactive({data.frame(Time  = geng.t(),
                                PROB  = geng.p, 
                                CDF   = geng.C(),
                                PDF   = geng.P(),
                                REL   = geng.R(),
                                haz   = geng.h(),
                                HAZ   = geng.H(), 
                                QUANT = geng.Q())})

  output$gengC <- renderMetricsgraphics({
  mjs_plot(geng.df(), x = Time, y = CDF, decimals = 4, top = 0) %>%
  mjs_line(area = TRUE) %>%
  mjs_labs(x_label = 'Time (t)', y_label = 'F(t)')%>%
  mjs_add_css_rule("{{ID}} .mg-active-datapoint { font-size: 20pt }")}) 
  
  output$gengP <- renderMetricsgraphics({
  mjs_plot(geng.df(), x = Time, y = PDF, decimals = 4) %>%
  mjs_line(area = TRUE) %>%
  mjs_labs(x_label = 'Time (t)', y_label = 'f(t)') %>%
  mjs_add_css_rule("{{ID}} .mg-active-datapoint { font-size: 20pt }")})
  
  output$gengR <- renderMetricsgraphics({
  mjs_plot(geng.df(), x = Time, y = REL, decimals = 4) %>%
  mjs_line(area = TRUE) %>%
  mjs_labs(x_label = 'Time (t)', y_label = 'S(t)') %>%
  mjs_add_css_rule("{{ID}} .mg-active-datapoint { font-size: 20pt }")})
  
  output$gengh <- renderMetricsgraphics({
  mjs_plot(geng.df(), x = Time, y = haz, decimals = 4) %>%
  mjs_line(area = TRUE) %>%
  mjs_labs(x_label = 'Time (t)', y_label = 'h(t)') %>%
  mjs_add_css_rule("{{ID}} .mg-active-datapoint { font-size: 20pt }")})
  
  output$gengH <- renderMetricsgraphics({
  mjs_plot(geng.df(), x = Time, y = HAZ, decimals = 4) %>%
  mjs_line(area = TRUE) %>%
  mjs_labs(x_label = 'Time (t)', y_label = 'H(t)') %>%
  mjs_add_css_rule("{{ID}} .mg-active-datapoint { font-size: 20pt }")})
  
  output$gengQ <- renderMetricsgraphics({
  mjs_plot(geng.df(), x = PROB, y = QUANT, decimals = 4) %>%
  mjs_line(area = TRUE) %>%
  mjs_labs(x_label = 'Probability', y_label = 'q(t)') %>%
  mjs_add_css_rule("{{ID}} .mg-active-datapoint { font-size: 20pt }")})
}