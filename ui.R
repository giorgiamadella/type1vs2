library(shiny)

shinyUI(pageWithSidebar(
  
  headerPanel("Type I vs. Type II error in NHST"),
  
  sidebarPanel(
    tags$head( tags$script(src="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_HTML-full", type = 'text/javascript'),
               tags$script( "MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}});", type='text/x-mathjax-config')
    ),
    p("The goal of this simulation is to demonstrate the trade-off between type I error $\\alpha$, the probability of rejecting the null hypothesis $H_0$ when it is true, and type II error $\\beta$, the probability of failing to reject the null hypothesis $H_0$ when it is false, under the Null Hypothesis Significance Testing framework."),
    
p("Reducing type I error by decreasing $\\alpha$ will increase type II error and vice versa. You can verify the validity of this claim by using the controls below to alter the value of $\\alpha$."),
    h4("Simulation parameters:"),
    sliderInput("alpha", 
                "Type I error ((\\alpha)):", 
                value = 0.05,
                min = 0.005, 
                max = 1),
  br(),
  helpText(a(href="https://github.com/tgouhier/type1vs2", target="_blank", "View code"))),
  
  mainPanel(
    plotOutput("plot", height="900px")
  )
))
