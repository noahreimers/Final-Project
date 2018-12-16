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


## I start by loading in my two different datasets that I will need for the plots.

pgroups <- read_rds("p_groups_records.rds")
draft_value <- read_rds("draft_value_ratings.rds")


## I am defining the choices for the first plot, by writing out each of the variables that you will be able to select in the 
## dropdown area.

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

## Here I am doing the same thing for the second plot.

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

## Here I entered all the choices for when you filter by position. I also entered all the team names because I wanted to 
## be able to filter by both position and team separately, but the only way I could figure out how to do it was if they did them
## together and that yielded just one data point. Something to figure out.

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
                
                ## So here I am entering in the column where the information about the data point that is clicked will show up. I haven't
                ## fully figured out how best to do this, but this provides a base-level fix of displaying the dataset in a very ugly way.
                ## Still something that needs work. I want to be able to click or hover over each datapoint and see what team it is, that
                ## way you might be able to think through why it looks that way, or who the outliers are. I did this point selection in
                ## each of the plots.
                
                mainPanel(
                  column(width = 9,
                         plotOutput("Plot", height = 300,
                                    click = "Plot_click")
                  ),
                  column(width = 7, 
                         h4("Points near click"),
                         verbatimTextOutput("click_info")
                  )
                )
              )
            )
),

## Here I am reading in the second tab and describing what each of the variables means. I tinkered with the explanations of the
## variables a bit, and thought this was the best. Could need some more alterations.

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
             
             ## Here is where I am doing the same procedure as above to make it possible to see what team each data point 
             ## represents. This is particularly interesting for this plot because it shows the team that drafted and from there
             ## a big NFL fan like myself can deduce who the draft pick was on.
             
             mainPanel(
               column(width = 9,
                      plotOutput("plot2", height = 350,
                                 click = "plot2_click")
               ),
               column(width = 7,
                      h4("Points near Click"),
                      verbatimTextOutput("click_info2"))
             )
           )
         )
),
tabPanel("Takeaways",
         fluidPage(
           titlePanel(
             "Major Takeaways from the 1st two tabs"),
           p("When looking at the first tab 'Pro Football Focus Ratings
             and Winning Percentage' it is most helpful to keep the Y 
             Variable as Winning Percentage. From there, the x variable
             can be altered to see which positions performance correlates
             the most with winning. As you would expect, QB ratings 
             have the highest correlation with winning. Both the OL and 
             DL ratings don't appear to have a very high correlation. I 
             think that is a limitation of the analysis, and the fact 
             that I had to lump together all of the offensive linemen
             and defensive linemen. This graph reiterates the idea of
             how important passing has become in the NFL. The ratings with the running back position group (RB) 
             do not correlate highly with winning percentage. Large shifts in the NFL and importance of certain position
             groups around the passing game can be seen."),
           p(" When analyzing the second tab 'Ratings and the NFL Draft'
             the X Variable selected determines the total amount of 
             effort and picks a team has used to draft a player at 
             that position. The Y variable shows how much that team has
             derived from those picks. The good news for teams without
             a franchise quarterback is that it appears to show that 
             teams who spend more picks and effort to get a QB will 
             receive more value from those selections. The OL postiion group has the highest correlation between draft strength
             and value derived. The more effort and picks a team uses, the more value they will derive from their offensive linemen.
             The lack of correlation between Draft Pick Strength Index and Value
             for position groups like WR or DB highlights the difficulty
             that teams have had with selecting players in the NFL Draft
             at those positions.")
           
           )
           )
         )

## I tried to use Plotly instead so there would be easy hover functionality, but the tags in Plotly would only show the variables
## That were selected as the X or Y variables. I tried to use the paste function in a couple different ways to create the info. 
## that I wanted to display (Team), but had no luck in the couple things I tried. So I instead decided to go with the ggplot 
## click functionality to at least give the user a chance to see the Team.

# Define server logic used to generate each of the plots and the corresponding click output provided when the user clicks
## on any of the points.

server <- function(input, output) {
  output$Plot <- renderPlot({
    
    # Generating the first plot that I listed the inputs for above and including a minor line of best fit. I tried multiple
    # Different things to try to make the axes titles reactive and change when the user selects them but had no luck.
    
    pgroups %>% 
      ggplot(aes_string(x = input$xcol, y = input$ycol)) + geom_point() + geom_smooth(method = "lm")
  }
  )
  
  ## This is the code I used to print out the data point that the user clicks on. Even though it produces a very ugly output
  ## it is the only thing I could figure out to print at least some of the data.
  
  output$click_info <- renderPrint({
    nearPoints(pgroups, input$Plot_click, addDist = TRUE)
  })
  
  ## The code used to generate the plot for the draft and the value derived from those players. I am filtering on position here
  ## even though I could not achieve my end goal of being able to filter by both team and position. I tried to use Ms. 
  ## Christiansen's code pertaining to being able to select more than one position at a time, or select all, but ran into issues
  ## when implementing the code.
  
  output$plot2 <- renderPlot({
    draft_value %>%
      filter(Pos == input$position) %>%
      ggplot(aes_string(x = input$xselect, y = input$yselect)) + geom_point() + 
      geom_smooth(method = "lm", color = "black", show.legend = TRUE)
    #stat_smooth(geom = "text", method = "lm", hjust = 0, parse = TRUE)
  })
  output$click_info2 <- renderPrint({
    nearPoints(draft_value, input$plot2_click, addDist = TRUE)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)