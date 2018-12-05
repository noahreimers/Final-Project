#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(devtools)
library(plotly)

pgroups <- read_rds("p_groups_records.rds")
draft_value <- read_rds("draft_value_ratings.rds")

## I am defining the choices for the first plot

x1_choices <- c("QB Ratings" = "qb_sum",
                "RB Ratings" = "rb_sum",
                "WR Ratings" = "wr_sum",
                "TE Ratings" = "te_sum",
                "OL Ratings" = "ol_sum",
                "DL Ratings" = "dl_sum",
                "LB Ratings" = "lb_sum",
                "DB Ratings" = "db_sum")

y1_choices <- c("Offense Rating" = "Overall_Offense",
                "Defense Rating" = "Overall_Defense",
                "Winning Percentage" = "Pct",
                "Pythagorean Expectation" = "Pyt_Exp")

x2_choices <- c("Position Group Yearly Rating" = "team_total",
                "Draft Pick Strength Index" = "draft_picks",
                "Value derived from those Picks (PFR)" = "value",
                "Value per Draft Pick Strength index" = "value_per",
                "Win Percentage over Period (2008-2012)" = "Win_Pct")

y2_choices <- c("Position Group Yearly Rating" = "team_total",
                "Draft Pick Strength Index" = "draft_picks",
                "Value derived from those Picks (PFR)" = "value",
                "Value per Draft Pick Strength index" = "value_per",
                "Win Percentage over Period (2008-2012)" = "Win_Pct")

p2_choices <- c("QB" = "QB",
                "RB" = "RB",
                "WR" = "WR",
                "TE" = "TE",
                "OL" = "OL",
                "DL" = "DL",
                "LB" = "LB",
                "DB" = "DB")

teams <- c("Arizona Cardinals" = "ARZ", 
           "Atlanta Falcons" = "ATL", 
           "Baltimore Ravens" = "BLT",
           "Buffalo Bills" = "BUF",
           "Carolina Panthers" = "CAR",
           "Chicago Bears" = "CHI",
           "Cincinnati Bengals" = "CIN",
           "Cleveland Browns" = "CLV",
           "Dallas Cowboys" = "DAL",
           "Denver Broncos" = "DEN",
           "Detroit Lions" = "DET",
           "Green Bay Packers" = "GB",
           "Houston Texans" = "HST",
           "Indianapolis Colts" = "IND",
           "Jacksonville Jaguars" = "JAX",
           "Kansas City Chiefs" = "KC",
           "Miami Dolphins" = "MIA",
           "Minnesota Vikings" = "MIN",
           "New England Patriots" = "NE",
           "New Orleans Saints" = "NO",
           "New York Giants" = "NYG",
           "New York Jets" = "NYJ",
           "Oakland Raiders" = "OAK",
           "Philadelphia Eagles" = "PHI",
           "Pittsburgh Steelers" = "PIT",
           "San Diego Chargers" = "SD",
           "Seattle Seahawks" = "SEA",
           "San Francisco 49ers" = "SF",
           "Saint Louis Rams" = "SL",
           "Tampa Bay Buccaneers" = "TB",
           "Tennessee Titans" = "TEN",
           "Washington Redskins" = "WAS")
all <- ("All")

# Define UI for application that looks at the individual player ratings from Pro Football Focus and how they lead to success.

ui <- navbarPage(
  "NFL Player Ratings",
  tabPanel( "Position Groups and Winning",
            fluidPage(
              titlePanel("Pro Football Focus Ratings and Winning Percentage"),
              sidebarLayout(
                sidebarPanel(
                  p("The X Variable allows you to select any specific position group and the corresponding rating from that
                    group for each team and each year between 2008 and 2012. The Y variable allows you to chose different
                    success rates like winning percentage, overall defensive ratings, overall offensive rating and the 
                    Pythagorean expectation of a team's winning percentage (based upon points scored and given up)"),
                  selectInput("xcol",
                     "X Variable", choices = x1_choices,
                     selected = "qb_ratings"
                     ),
                  selectInput("ycol",
                     "Y Variable",
                     choices = y1_choices,
                     selected = "Pct")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotlyOutput("Plot")
      )
              )
            )
  ),
  tabPanel("Ratings and the NFL Draft",
           fluidPage(
             titlePanel("PFF Ratings, Draft Picks and Value Derived from those picks"),
             sidebarLayout(
               sidebarPanel(
                 p("Draft Pick Strength Index adds all of draft picks used by each team on the specific position, a 1st round
                   draft pick is worth 7, a 2nd round worth 6, etc. (8-round). This provides a total amount and strength for the
                   draft picks used on that position. Position Group Yearly Rating is a collection of the ratings for each
                   position group on each team yearly (using PFF ratings). Value derived from those picks is a collection of 
                   the approximate value (provided by Pro Football Reference) of the players they drafted in each position group
                   over the period 2008-2012. Value per Draft Pick Strength Index simply divides the Value derived by the 
                   Draft Pick Strength Index. The Win Percentage Over Period simply matches each of these measures with the
                   team's winning percentage of the entire period."),
                 selectInput("xselect" , 
                             "X Variable" , choices = x2_choices,
                             selected = "draft_picks"
                             ),
                 selectInput("yselect",
                             "Y Variable",
                             choices = y2_choices,
                             selected = "value"
                             ),
                 selectInput("position",
                             "Position Group",
                             choices = p2_choices,
                             selected = "QB"
                             )
             ),
             mainPanel(
               plotlyOutput("plot2")
             )
           )
           )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  output$Plot <- renderPlotly({
    
      # Generating the first plot that I listed the inputs for above
    pgroups %>% 
      ggplot(aes_string(x = input$xcol, y = input$ycol)) + geom_point() + geom_smooth(method = "lm")
             }
  )

  output$plot2 <- renderPlotly({
    draft_value %>%
      filter(Pos == input$position) %>%
      ggplot(aes_string(x = input$xselect, y = input$yselect)) + geom_point() + 
      geom_smooth(method = "lm", color = "black", show.legend = TRUE)
      #stat_smooth(geom = "text", method = "lm", hjust = 0, parse = TRUE)
  })
   }

# Run the application 
shinyApp(ui = ui, server = server)

