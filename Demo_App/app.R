#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

pgroups <- read_rds(p_groups_records)

# Define UI for application that looks at the individual player ratings from Pro Football Focus and how they lead to success.

ui <- navbarPage(
  "NFL Player Ratings",
  tabPanel( "Position Groups and Winning",
            fluidPage(
              titlePanel("Pro Football Focus Ratings and Winning Percentage"),
              sidebarLayout(
                sidebarPanel(
                  selectInput("xcol",
                     "X Variable", choices = c("QB Ratings" = qb_sum, "RB Ratings" = rb_sum, "WR Ratings" = wr_sum, "TE Ratings" 
                                               = te_sum, "OL Ratings" = ol_sum, "DL Ratings" = dl_sum, "LB Ratings" = lb_sum, 
                                               "DB Ratings" = db_sum)
                     ),
                  selectInput("ycol",
                     "Y Variable",
                     choices = c("Offense Rating" = `Overall Offense`, "Defense Rating" = `Overall Defense`, "Winning Percentage" = 
                                   Pct, "Pythagorean Expectation" = Pyt_Exp))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("Plot")
      )
              )
            )
  ),
  tabPanel("Ratings and the NFL Draft",
           fluidPage(
             titlePanel("When do teams decide to use the NFL Draft to replace a player"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("xselect" , 
                             "X Variable" , names(draft)
                             ),
                 selectInput("yselect",
                             "Y Variable",
                             names(draft)
                             )
               )
             )
           )
           ))


# Define server logic required to draw a histogram
server <- function(input, output) {
  selectedData <- reactive({
    pgroups[, c(input$xcol, input$ycol)]
  })
   
   output$Plot <- renderPlot({
      # generate bins based on input$bins from ui.R
      plot(selectedData(),)
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

