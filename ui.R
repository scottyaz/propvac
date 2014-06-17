
# This is the user-interface definition of a Shiny web application.

library(shiny)

shinyUI(fluidPage(
  titlePanel("Tool for OCV Vaccination"),
  
    sidebarLayout(
      sidebarPanel(
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
                        label=helpText("1-dose Vaccine Efficacy (default based on point estimate from meta-analysis)"),
                        value=0.44,
                        min=0,
                        max=1),
           numericInput("VE2",
                        label=helpText("2-dose Vaccine Efficacy (default based on point estimate from meta-analysis)"),
                        value=0.73,
                        min=0,
                        max=1),
           numericInput("R.pes",
                        label=helpText("Reproductive Number (pessimistic)"),
                        value=2,
                        min=1                      
           ),
           numericInput("R.mod",
                        label=helpText("Reproductive Number (moderate)"),
                        value=1.5,
                        min=1),
           numericInput("R.opt",
                        label=helpText("Reproductive Number (optimisitic)"),
                        value=1.1,
                        min=1                      
           ))
      ),
mainPanel(
  h3("Estimated Proportion Protected Compared to Vaccination Zones in 3 Different Scenarios"),
  plotOutput('my.plot'),
  h5("Warning: The above output is based on basic epidemic theory and should only be used as a rough guide. \n"),
  "Source code can be found at our ",
  a("GitHub page", href="link.com")
  ))))