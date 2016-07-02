# Server code for a shiny app to explore student loan repayments in the UK
# Steve Mainprize - July 2016

shinyServer(
  function(input, output) {

  myloanmodel <- reactive({
    
    # Initialise variables

    # Debt brought forward and carried forward
    bf <- 0
    cf <- 0
    
    # Year and month in which course starts. Could be input items, but decided to hard-code
    # them to save space on the page
    startyearofcourse <- 2016
    date <- as.Date(paste(startyearofcourse,"-09-01",sep = ""))
    
    # Monthly interest rate, calculated from the annual RPI figure
    monthlyinterestrate <- (1 + (3 + input$rpi) / 100) ^ (1 / 12) - 1
    
    # Flags for remembering where we're up to in the status of the loan
    repaymentsstarted <- FALSE
    loanwrittenoff <- FALSE

    # How many months we need in the model    
    monthsneeded <- 12 * (as.numeric(input$years) + 1 + 30) - 5

    # Create a data frame. This will hold a month-by-month flow of debt based on the parameters    
    loanmodel <-
      data.frame(
        date = as.Date(rep(0,monthsneeded), origin = "1900-01-01"),
        age = numeric(monthsneeded),
        annualsalary = numeric(monthsneeded),
        bf = numeric(monthsneeded),
        receipts = numeric(monthsneeded),
        interestadded = numeric(monthsneeded),
        repayment = numeric(monthsneeded),
        cf = numeric(monthsneeded)
      )
    
    # Initialisation of variables complete
    
    # Start to calculate the flow of the loan model
    
    rowcount <- 0
    while (rowcount < monthsneeded) {
      
      # increment the row count
      rowcount <- rowcount + 1
      
      # record the date that this row represents
      loanmodel$date[rowcount] <- date
      
      # Set the row's "brought forward" to the previous row's "carried forward"
      loanmodel$bf[rowcount] <- cf

      # If it's the start of an academic year, add more debt to the pile      
      if ((format(date,"%m") == "09") &
          (rowcount < as.numeric(input$years) * 12)) {
        loanmodel$receipts[rowcount] <-
          input$annualtuitionfee + input$annualmaintenance
      }
      
      # Calculate the interest added in the current month
      loanmodel$interestadded[rowcount] <-
        (loanmodel$bf[rowcount] + loanmodel$receipts[rowcount]) * monthlyinterestrate
      
      # Repayments start in the April after the student has graduated.
      if ((format(date,"%m") == "04") &
          (rowcount >= as.numeric(input$years) * 12)) {
        repaymentsstarted <- TRUE
      }
      
      # Work out what the student is earning in the current month
      if (rowcount <= (as.numeric(input$years) * 12)) {
        # Hasn't started earning yet 
        loanmodel$annualsalary[rowcount] <- 0
      } else if (rowcount == (1 + as.numeric(input$years) * 12)) {
        # First month of getting paid
        loanmodel$annualsalary[rowcount] <- input$startingsalary
      } else if (format(date,"%m") == "09") {
        # Assumes that the student gets a pay rise every September, i.e. each anniversary
        # of starting work. Monthly pay packet this month is calculated by adding the assumed
        # annual increase to last month's pay packet.
        loanmodel$annualsalary[rowcount] <-
          round(loanmodel$annualsalary[(rowcount - 1)] * (100 + input$salaryincrease) / 100, digits =
                  -2)
      } else if (rowcount > (as.numeric(input$years) * 12)) {
        # The student gets what they got last month
        loanmodel$annualsalary[rowcount] <-
          loanmodel$annualsalary[(rowcount - 1)]
      }
      
      if (repaymentsstarted) {
        # Calculate their debt repayment this month, which is 9% of whatever their salary
        # is over 21000 (per annums converted to per month, obviously)
        if (loanmodel$annualsalary[rowcount]>21000) {
          loanmodel$repayment[rowcount] = 0.09 * (loanmodel$annualsalary[rowcount] -
                                                    21000) / 12
        }
      }
      
      # Work out what debt gets carried forward to next month
      loanmodel$cf[rowcount] <- loanmodel$bf[rowcount] +
        loanmodel$receipts[rowcount] +
        loanmodel$interestadded[rowcount] -
        loanmodel$repayment[rowcount]
      
      # If the debt carried forward to next month would be negative, then the student
      # has repaid the debt. Cap the repayment to the amount outstanding on the loan.
      if (loanmodel$cf[rowcount] < 0) {
        loanmodel$repayment[rowcount] = loanmodel$bf[rowcount] +
          loanmodel$receipts[rowcount] +
          loanmodel$interestadded[rowcount]
        loanmodel$cf[rowcount] = 0
      }
      
      cf <- loanmodel$cf[rowcount]
      
      # add a month to the date
      date <- seq(date, by = paste (1, "months"), length = 2)[2]
      
    }
    
    return(loanmodel)
    
  })
  
  # Draw a plot of the amount of debt outstanding
  output$plot1 <- renderPlot({
    input$goButton
    isolate(
      ggplot(data=myloanmodel(), aes(x=date, y=cf)) +
      ggtitle("Amount of Debt") +
      ylab("Pounds") +
      xlab("Year") +
      theme(legend.position = "none") +
      theme(plot.title = element_text(lineheight=.8, face="bold")) +
      geom_line(aes(group=1), colour="#D55E00")
      )
    })
  
  # Draw a plot of the monthly repayment
  output$plot2 <- renderPlot({
    input$goButton
    isolate(
    ggplot(data=myloanmodel(), aes(x=date, y=repayment)) +
      ggtitle("Monthly Repayment") +
      ylab("Pounds") +
      xlab("Year") +
      theme(legend.position = "none") +
      theme(plot.title = element_text(lineheight=.8, face="bold")) +
      geom_line(aes(group=1), colour="#0072B2")
    )
  })
  
  # Show text of the total amount the student borrowed, how much they repaid, and
  # how much was written off because they hit the 30 year maximum.
  output$stats <- renderPrint({
    input$goButton
    isolate(
      cat("amount borrowed: \t£",format(sum(myloanmodel()$receipts),nsmall=2,big.mark=",",small.mark=","),"\n",
        "amount repaid: \t\t£",format(sum(myloanmodel()$repayment),nsmall=2,big.mark=",",small.mark=","),"\n",
        "amount written off: \t£",format(tail(myloanmodel()$cf,n=1),nsmall=2,big.mark=",",small.mark=","),sep="")
    )
  })
  
}
)
