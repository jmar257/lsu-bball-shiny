library(shiny)
library(ggvis)
library(magrittr)
library(reshape2)
library(plyr)

pm <- read.csv('Individual Data.csv', 
               header = TRUE)
homevaway <- read.csv('Home vs Away.csv', 
                    header = TRUE)
homevaway$Player <- as.character(homevaway$Player)
pm$Player <- as.character(pm$Player)
player_choices <- unique(pm$Player)
pm$Conference <- as.character(pm$Conference)
conf_choices <- unique(pm$Conference)
advanced <- read.csv('Advanced Stats.csv', 
                           header = TRUE)
var_choices <-  c('minutes_played',  'usage_rate',  'true_shooting',	'effective_shooting_rate',	'turnover_rate', 'offensive_rebound_rate',	'defensive_rebound_rate',	'offensive_rating',	'defensive_rating')
advanced$Player <- as.character(advanced$Player)
basicstats <- read.csv('viz.csv', 
                       header = TRUE)
var_choices_basic <- c('Mins_Played', 'Gamescore', 'Game', 'Win', 'lsuscore', 'oppscore', 'home_away', 'conference', 'opponent', 'MP', 'FG', 'FGA', 'FG_Per', 'Two_Pointers', 'Two_Pointers_Attempted', 'Two_Point_Per', 'Three_Pointers', 'Three_Pointers_Attempted', 'Three_Point_Per', 'FT', 'FTA', 'Free_Throw_Per', 'ORB', 'DRB', 'TotalRB', 'AST', 'STL', 'BLK', 'TOV', 'PF', 'PTS', 'Total_shots')
var_choices_basic2 <- c('Mins_Played', 'Gamescore', 'Win', 'lsuscore', 'oppscore', 'MP', 'FG', 'FGA', 'FG_Per', 'Two_Pointers', 'Two_Pointers_Attempted', 'Two_Point_Per', 'Three_Pointers', 'Three_Pointers_Attempted', 'Three_Point_Per', 'FT', 'FTA', 'Free_Throw_Per', 'ORB', 'DRB', 'TotalRB', 'AST', 'STL', 'BLK', 'TOV', 'PF', 'PTS', 'Total_shots')
ha_var_choices <- c('AvgMinsplayed',  'PER', 'Gamescore')
basicstats$Player <- as.character(basicstats$Player)
home_choices <- c('Home', 'Away')



shinyUI(navbarPage('LSU Basketball Statistics',
    tabPanel("Gamescore by Playing Time",
  
  sidebarLayout(
    sidebarPanel(
    checkboxGroupInput(inputId = 'conf', label = 'Choose games:', choices = conf_choices, selected = c('Conference', 'Non-conference')),
    checkboxGroupInput(inputId = 'home', label = 'Choose location:', choices = home_choices, selected = c('Home', 'Away')),
    selectInput(inputId = 'Player', label = 'Choose a player:', choices = player_choices),
    helpText('Black line indicates an average gamescore (10) for a college player.'),
    radioButtons(inputId = 'model', label = 'Pick a line to show:', choices = c('LOESS' = 'loess', 'Linear' = 'lm'), selected = 'loess', inline = TRUE)
    #radioButtons(inputId = 'se', label = 'Show standard errors?', choices = c('Hide' = FALSE, 'Show' = TRUE), selected = FALSE, inline = TRUE)
    ),
    
    mainPanel(
      ggvisOutput("ggvis")
    )
    )),
  
#   tabPanel('Basic stats',
#     sidebarLayout(
#       sidebarPanel(
#         selectInput(inputId = 'stat', label = 'Choose a statistic:', choices = var_choices),
#         helpText('Players who are selected but do not appear on the graph have a value of 0 or N/A for the selected statistic'),
#         checkboxGroupInput(inputId = 'players', label = 'Choose player:', choices = player_choices, selected = c('Ben Simmons', 'Aaron Epps', 'Tim Quarterman', 'Craig Victor', 'Keith Hornsby', 'Josh Gray', 'Brandon Sampson', 'Darcy Malone'))),
#       
#     mainPanel(
#       ggvisOutput("bar")
#     )
#   )
#   ),
  
  tabPanel('Home vs Away',
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = 'hastat', label = 'Choose a statistic:', choices = ha_var_choices),
        checkboxGroupInput(inputId = 'haplayers', label = 'Choose player:', choices = player_choices, selected = c('Ben Simmons', 'Tim Quarterman', 'Craig Victor'))
        ),

      mainPanel(
        ggvisOutput("homeaway")
      )
    )
  ),
  
  
  tabPanel('Player Stats Over Season',
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = 'basicstat', label = 'Choose a statistic:', choices = var_choices_basic2),
        selectInput(inputId = 'basicplayers', label = 'Choose players:', choices = player_choices, selected = player_choices)),
      
      mainPanel(
        ggvisOutput("jacktabloo")
      )
    )
  ),
  
  navbarMenu("Shot Charts",
  
  tabPanel('By Shot',
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = 'scplayer', label = 'Choose player:', choices = player_choices, selected = 'Ben Simmons'),
        helpText("If plot does not appear, player has not taken any shots."),
        checkboxGroupInput(inputId = 'shot_type', label = 'Shot type to show:', choices = c('Jump Shot'= 'Jumper', 'Three Pointer' = 'Three', 'Layup', 'Dunk', 'Tip Shot' = 'TipShot'), selected = c('Jumper', 'Three', 'Layup', 'Dunk', 'TipShot')),
        checkboxGroupInput(inputId = 'assisted', label = 'Assisted:', choices = c('Yes'= 'yes', 'No' = 'no'), selected = c('yes', 'no'))
      ),
      
      mainPanel(
        plotOutput("plot5")
      )
    )
  ),
  
  tabPanel('Heat Map',
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = 'hmplayer', label = 'Choose player:', choices = player_choices, selected = 'Ben Simmons')
        ),
      
      mainPanel(
        plotOutput("plot6")
      )
    ))
  )
  )
)
