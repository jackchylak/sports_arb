# Sports Odds Arbitrage Finder
# Jack Chylak

library(png)
library(httr)
library(shiny)
library(oddsapiR)
library(tidyverse)
library(schoolmath)
library(lubridate)
library(janitor)
library(bslib)

ODDS_API_KEY <- *Get Free Odds API Key from https://the-odds-api.com/* # Paste API Key Here

Sys.setenv(ODDS_API_KEY = ODDS_API_KEY)

sports <- c("Baseball: MLB", "Basketball: WNBA", "Americanfootball: NCAAF", "Americanfootball: NFL", "Basketball: NBA", "Icehockey: NHL")

convert_to_central_time <- function(unix_timestamp) {
  posix_time <- as.POSIXct(unix_timestamp, origin="1970-01-01", tz="UTC")
  central_time <- with_tz(posix_time, tzone="America/Chicago")
  formatted_time <- format(central_time, "%B %d, %Y, %I:%M%p")
  return(formatted_time)
}


ui <- tagList(
  tags$head(
    tags$style(HTML("
      .navbar-page {
        text-align: center;
        margin-bottom: 0;
      }
      #custom-title {
        text-align: center;
        font-size: 42px;
        margin-top: 10px;
        margin-bottom: 0;
        font-weight: bold;
      }
      #custom-name {
        position: absolute;
        top: 20px;
        right: 40px;
        font-weight: bold;
        font-size: 16px;
      }
      .navbar {
        justify-content: center;
      }
    "))
  ),
  
  div(id = "custom-title", "Sports Odds Arbitrage Finder"),
  div(id = "custom-name", "Jack Chylak"),
  
  navbarPage(
    theme = bs_theme(
      bg = "seashell",
      fg = "#120f40",
      primary = "#3498dc",
      base_font = font_google("Yantramanav")
    ),
    
    title = NULL,
    
    tabPanel("Arbitrage Opportunities",
             fluidRow(
               column(6, selectInput("selected_sport", "Choose a sport:", choices = sports)),
               column(6, numericInput("bet_max", "Enter maximum bet amount:", value = 200, min = 1))
             ),
             fluidRow(
               column(12,
                      hr(),
                      h4(paste("Requests Remaining:", toa_requests()[1])),
                      h3("Moneyline - Arbitrage Opportunities"),
                      tableOutput("h2h_arbs"),
                      h3("Totals - Arbitrage Opportunities"),
                      tableOutput("totals_arbs"),
                      h3("-"),
                      h6("*Arb Opportunities are much more common during live games because sportsbooks close the margins quickly!*"),
                      h3("-"),
                      h3("Moneyline - All games"),
                      tableOutput("h2h"),
                      h3("Totals - All games"),
                      tableOutput("totals")
               )
             )
    ),
    
    tabPanel("How this works",
             fluidPage(
               h2("How This Program Works:"),
               p('This program scrapes Betting Odds from 9 US Sportsbooks using the odds-api (the-odds-api.com) to find the probabilites of sport event outcomes and calculates how much money to place on each team and on which sportsbooks to earn a guaranteed profit, no matter the outcome!'),
               p("Using this tool to actually win bets is unlikely as the odds aren't scraped fast enough and, unless you can place the bet instantly using some sort of program, it's difficult to go and place the bet before the odds change."),
               p("See this infographic to learn more about the math behind this program!"),
               img(src = "https://raw.githubusercontent.com/jackchylak/sports_arb/main/sports_arb_infographic.png", alt = "Example Image", width = "100%")
             )
    )
  )
)



server <- function(input, output) {
  
  data <- reactive({
    sport_key <- make_clean_names(input$selected_sport) # API call to get odds
    toa_sports_odds(sport_key = sport_key,
                    regions = 'us',
                    markets = 'h2h,totals',
                    odds_format = 'american',
                    date_format = 'unix')
  })
  
  h2h_arbs <- reactive({
    h2h_data <- h2h()
    
    # Calculate Implied Probability from odds
    h2h_data$implied_prob <- ifelse(h2h_data$outcomes_price > 0, (100 / (h2h_data$outcomes_price + 100)), ((abs(h2h_data$outcomes_price) / (abs(h2h_data$outcomes_price) + 100))))
    
    h2h_data$arb1 <- ifelse(h2h_data$outcomes_price == h2h_data$odds_away_max & abs(h2h_data$odds_away_max) < h2h_data$odds_home_max & is.negative(h2h_data$odds_away_max), 1, 0)
    h2h_data$arb2 <- ifelse(h2h_data$outcomes_price == h2h_data$odds_home_max & abs(h2h_data$odds_away_max) < h2h_data$odds_home_max & is.negative(h2h_data$odds_away_max), 1, 0)
    h2h_data$arb3 <- ifelse(h2h_data$outcomes_price == h2h_data$odds_home_max & abs(h2h_data$odds_home_max) < h2h_data$odds_away_max & is.negative(h2h_data$odds_home_max), 1, 0)
    h2h_data$arb4 <- ifelse(h2h_data$outcomes_price == h2h_data$odds_away_max & abs(h2h_data$odds_home_max) < h2h_data$odds_away_max & is.negative(h2h_data$odds_home_max), 1, 0)
    h2h_data$arb5 <- ifelse((h2h_data$outcomes_price == h2h_data$odds_home_max) & (is.positive(h2h_data$odds_home_max) & is.positive(h2h_data$odds_away_max)), 1, 0)
    h2h_data$arb6 <- ifelse((h2h_data$outcomes_price == h2h_data$odds_away_max) & (is.positive(h2h_data$odds_home_max) & is.positive(h2h_data$odds_away_max)), 1, 0)
    
    h2h_arbs_data <- h2h_data %>%
      filter(arb1 == 1 | arb2 == 1 | arb3 == 1 | arb4 == 1 | arb5 == 1 | arb6 == 1) %>%
      select(bookmaker_last_update, commence_time, home_team, away_team, bookmaker, outcomes_name, outcomes_price, implied_prob) %>%
      group_by(home_team) %>%
      mutate(sum_of_probs = sum(implied_prob),
             bookmaker_last_update = convert_to_central_time(bookmaker_last_update),
             commence_time = convert_to_central_time(commence_time))
    
    if (nrow(h2h_arbs_data) != 0) {
      BET_MAX <- input$bet_max
      h2h_arbs_data$stake <- BET_MAX * (h2h_arbs_data$implied_prob / h2h_arbs_data$sum_of_probs)
      
      h2h_arbs_data$winnings <- ifelse(is.negative(h2h_arbs_data$outcomes_price), h2h_arbs_data$stake * (100 / abs(h2h_arbs_data$outcomes_price)), h2h_arbs_data$stake * (h2h_arbs_data$outcomes_price / 100))
      
      h2h_arbs_data <- h2h_arbs_data %>%
        group_by(home_team) %>%
        mutate(profit = ifelse(outcomes_name == home_team, winnings - stake[outcomes_name == away_team], winnings - stake[outcomes_name == home_team])) %>%
        rename("Game Start Time (CT)"=commence_time, "Bookmaker Last Update"=bookmaker_last_update, "Home Team"=home_team, "Away Team"=away_team, "Sportsbook"=bookmaker, "Winner"=outcomes_name, "Odds"=outcomes_price, "Implied Win Probability"=implied_prob, "Stake"=stake, "Winnings"=winnings) %>%
        mutate("Implied Win Probability"= paste0(as.character(round(`Implied Win Probability`*100), digits=0), "%")) %>%
        select(-sum_of_probs) %>%
        arrange(desc(profit))
    } else {
      h2h_arbs_data <- data.frame(x="Try another sport!") %>% rename("No Arb Opportunities found..."=x)
    }
    h2h_arbs_data
  })
  
  totals_arbs <- reactive({
    totals_data <- totals()
    
    totals_data$implied_prob <- ifelse(totals_data$outcomes_price > 0, (100 / (totals_data$outcomes_price + 100)), ((abs(totals_data$outcomes_price) / (abs(totals_data$outcomes_price) + 100))))
    
    totals_data$arb1 <- ifelse(is.positive(totals_data$odds_over_max) & totals_data$odds_over_max > abs(totals_data$odds_under_max) & is.negative(totals_data$odds_under_max) & (totals_data$odds_over_max == totals_data$outcomes_price | totals_data$odds_under_max == totals_data$outcomes_price), 1, 0)
    totals_data$arb2 <- ifelse(is.positive(totals_data$odds_under_max) & totals_data$odds_under_max > abs(totals_data$odds_over_max) & is.negative(totals_data$odds_over_max & (totals_data$odds_over_max == totals_data$outcomes_price | totals_data$odds_under_max == totals_data$outcomes_price)), 1, 0)
    totals_data$arb3 <- ifelse(is.positive(totals_data$odds_over_max) & is.positive(totals_data$odds_under_max) & (totals_data$odds_over_max == totals_data$outcomes_price | totals_data$odds_under_max == totals_data$outcomes_price), 1, 0)
    
    totals_arbs_data <- totals_data %>%
      filter(arb1 == 1 | arb2 == 1 | arb3 == 1) %>%
      select(bookmaker_last_update, commence_time, home_team, away_team, bookmaker, outcomes_price, outcomes_name, outcomes_point, implied_prob) %>%
      group_by(outcomes_point) %>%
      mutate(sum_of_probs = sum(implied_prob),
             bookmaker_last_update = convert_to_central_time(bookmaker_last_update),
             commence_time = convert_to_central_time(commence_time))
    
    if (nrow(totals_arbs_data) != 0) {
      BET_MAX <- input$bet_max
      totals_arbs_data$stake <- BET_MAX * (totals_arbs_data$implied_prob / totals_arbs_data$sum_of_probs)
      
      totals_arbs_data$winnings <- ifelse(is.negative(totals_arbs_data$outcomes_price), totals_arbs_data$stake * (100 / abs(totals_arbs_data$outcomes_price)), totals_arbs_data$stake * (totals_arbs_data$outcomes_price / 100))
      
      totals_arbs_data <- totals_arbs_data %>%
        group_by(outcomes_point) %>%
        mutate(profit = ifelse(outcomes_name == "Over", winnings - stake[outcomes_name == "Under"], winnings - stake[outcomes_name == "Over"])) %>%
        rename("Game Start Time (CT)"=commence_time, "Bookmaker Last Update"=bookmaker_last_update, "Home Team"=home_team, "Away Team"=away_team, "Sportsbook"=bookmaker, "Outcome"=outcomes_name, "Final Score"=outcomes_point, "Odds"=outcomes_price, "Implied Win Probability"=implied_prob, "Stake"=stake, "Winnings"=winnings, "Profit"=profit) %>%
        mutate("Implied Win Probability"= paste0(as.character(round(`Implied Win Probability`*100), digits=0), "%")) %>%
        select(-sum_of_probs) %>%
        arrange(desc(Profit))
    } else {
      totals_arbs_data <- data.frame(x="Try another sport!") %>% rename("No Arb Opportunities found..."=x)
    }
    totals_arbs_data
  })
  
  h2h <- reactive({
    d1 <- data()
    d1 %>% filter(market_key == "h2h") %>%
      select(bookmaker_last_update, commence_time, bookmaker_last_update, home_team, away_team, bookmaker, outcomes_name, outcomes_price) %>%
      group_by(home_team, commence_time) %>%
      mutate(odds_home_min = min(outcomes_price[home_team == outcomes_name]),
             odds_home_max = max(outcomes_price[home_team == outcomes_name]),
             odds_away_min = min(outcomes_price[away_team == outcomes_name]),
             odds_away_max = max(outcomes_price[away_team == outcomes_name])) %>%
      ungroup()
  })
    
  totals <- reactive({
    d1 <- data()
    d1 %>% filter(market_key == "totals") %>%
      group_by(outcomes_point, home_team) %>%
      mutate(odds_over_max = max(outcomes_price[outcomes_name == "Over"]),
             odds_under_max = max(outcomes_price[outcomes_name == "Under"]))
  })
  
  output$h2h_arbs <- renderTable({
    h2h_arbs()
  })
  
  output$totals_arbs <- renderTable({
    totals_arbs()
  })
  
  output$h2h <- renderTable({
    h2h() %>%
      mutate("Bookmaker Last Update"=convert_to_central_time(bookmaker_last_update),
             "Game Start Time (CT)"=convert_to_central_time(commence_time),
             "Implied Win Probability"=ifelse(outcomes_price > 0, (100/(outcomes_price+100)), ((abs(outcomes_price)/(abs(outcomes_price)+100))))) %>%
      mutate("Implied Win Probability"= paste0(as.character(round(`Implied Win Probability`*100), digits=0), "%")) %>%
      select(`Bookmaker Last Update`, `Game Start Time (CT)`, "Home Team"=home_team, "Away Team"=away_team, "Sportsbook"=bookmaker, "Winner"=outcomes_name, "Odds"=outcomes_price, `Implied Win Probability`)
  })
  
  output$totals <- renderTable({
    totals() %>%
      mutate("Bookmaker Last Update"=convert_to_central_time(bookmaker_last_update),
             "Game Start Time (CT)"=convert_to_central_time(commence_time)) %>%
      select(`Bookmaker Last Update`, `Game Start Time (CT)`, "Home Team"=home_team, "Away Team"=away_team, "Sportsbook"=bookmaker, "Outcome"=outcomes_name, "Score Total"=outcomes_point, "Odds"=outcomes_price)
    
  })
}

shinyApp(ui = ui, server = server)
