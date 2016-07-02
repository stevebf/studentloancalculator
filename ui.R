# UI code for a shiny app to explore student loan repayments in the UK
# Steve Mainprize - July 2016

library(shiny)
library(ggplot2)

shinyUI(
  pageWithSidebar(
  headerPanel('UK Student Loan Calculator'),
  sidebarPanel(
    p("Change parameters here, and hit the \"Calculate\" button to see the impact.",style = "font-size:80%"),

    # This is the annual tuition fee charged by the university. Usually it's £9000 per year. 
    numericInput(
      inputId = "annualtuitionfee",
      label = "Annual Tuition Fee (£)",
      min = 0, max = 50000, value = 9000, step = 100, width = "200px"
    ),

    # This is what the student wants to borrow for living expenses.  It's capped according to
    # household income.
    numericInput(
      inputId = "annualmaintenance",
      label = "Annual Maintenance Loan (£)",
      min = 0, max = 50000, value = 3821, step = 100, width = "200px"
    ),
    
    # Number of years the course runs for. The app only allows the user to select 3, 4 or 5
    radioButtons(
      inputId = "years",
      label = "Years of course",
      choices = c(3,4,5), inline = TRUE
    ),
    
    # What annual salary the student might get when they graduate and start work.
    numericInput(
      inputId = "startingsalary",
      label = "Starting Salary (£/year)",
      min = 0, max = 70000, value = 25000, step = 100, width = "200px"
    ),
    
    # An estimate of how much the salary might increase per year.
    numericInput(
      inputId = "salaryincrease",
      label = "Annual Salary Increase (%)",
      min = 0, max = 100.0, value = 4.0, step = 0.1, width = "200px"
    ),
    
    # This is an official givernment measure of inflation. It determines the rate
    # at which the student is charged interest.
    numericInput(
      inputId = "rpi",
      label = "Retail Price Index (%)",
      min = 0, max = 100.0, value = 0.9, step = 0.1, width = "200px"
    ),
    
    # User must hit the calculate button to see the outcomes. If we just let shiny
    # be all reactive, then the charts redraw e.g. during typing a number
    actionButton("goButton", "Calculate"),
    width = 3
  ),
  mainPanel(
    p("This page shows the projected monthly outstanding debt and debt repayment for UK students",
      "with a student loan starting in 2016."),
    p(a("Documentation",href="readme.html"),"is available if you would like more details."),
    plotOutput("plot1", height=200),
    plotOutput("plot2", height=200),
    verbatimTextOutput("stats"))
))

