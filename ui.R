
# This is the user-interface definition of a Shiny web application.

library(shiny)

shinyUI(fluidPage(
  titlePanel("Tool for Assessing Risk of Epidemic given OCV Vaccination Coverage Estimates"),
  
    sidebarLayout(
      sidebarPanel(
        tags$head( tags$script(src="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_HTML-full", type = 'text/javascript'),
                   tags$script( "MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}});", type='text/x-mathjax-config')
        ),
#    column(4,
           h3("Required Input:"),
           wellPanel(
           numericInput("N",
                         label=helpText("Population Size"),
                        value=100000,
                        min=0
                        ),
           numericInput("N1",
                        label=helpText("Number vaccinated with 1-dose"),
                        value=5000,
                        min=0),
           numericInput("N2",
                        label=helpText("Number vaccinated with 2-doses"),
                        value=5000,
                        min=0)),
           #submitButton(text = "Submit"),          
           h3("Optional Input:"),
           wellPanel(                         
             numericInput("VE1",
                        label=helpText("1-dose Vaccine Efficacy [default based on point estimate from meta-analysis]"),
                        value=0.44,
                        min=0,
                        max=1),
           numericInput("VE2",
                        label=helpText("2-dose Vaccine Efficacy [default based on point estimate from meta-analysis]"),
                        value=0.73,
                        min=0,
                        max=1),
           numericInput("R.pes",
                        label=helpText("Reproductive Number [pessimistic]"),
                        value=2,
                        min=1                      
           ),
           numericInput("R.mod",
                        label=helpText("Reproductive Number [moderate]"),
                        value=1.5,
                        min=1),
           numericInput("R.opt",
                        label=helpText("Reproductive Number [optimisitic]"),
                        value=1.1,
                        min=1),
           sliderInput("pct.uncer",
                        label=helpText("Margin of Error [+/- percent of estimate] \n This dictates the width of the colored regions and is a bit adhoc for now"),
                        value=0.3,
                        min=0,
                        max=1)          
           ),
           "Source code can be found ",
           a("here", href="https://github.com/scottyaz/propvac"),
           br(),  
           "Brought to you by the",
           a(" Infectious Disease Dynamics Group",href="http://iddynamics.jhsph.edu/"),
           "at Johns Hopkins Bloomberg School of Public Health"
      ),
mainPanel(
  h3("Estimated Proportion Protected Compared to Vaccination Zones in 3 Different Scenarios"),
  plotOutput('my.plot'),
  "The interface between yellow and red in each corresponds to the estimate of 1-1/R:",
  tabPanel('Proportion Needed to Protect (point estimates)', tableOutput("table")),
  h5("Warning: The above output is based on basic epidemic theory and should only be used as a rough guide. \n")
  ))))