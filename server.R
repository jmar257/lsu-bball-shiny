library(shiny)
library(ggvis)
library(magrittr)
library(reshape2)
library(plyr)
library(tidyr)
library(rjson)
library(ggplot2)
library(grid)
library(jpeg)
library(RCurl)
library(png)

pm <- read.csv('Individual Data.csv', 
               header = TRUE)
advanced <- read.csv('Advanced Stats.csv', 
                     header = TRUE)
basicstats <- read.csv('viz.csv', 
                     header = TRUE)
homevaway <- read.csv('Home vs Away.csv', 
                      header = TRUE)

pm$gmsc_per_min <-pm$Gamescore / pm$Mins_Played
pm$Player <- as.character(pm$Player)
pm$Conference <- as.character(pm$Conference)
player_choices <- unique(pm$Player)
conf_choices <- unique(pm$Conference)
homevaway$Player <- as.character(homevaway$Player)
advanced$Player <- as.character(advanced$Player)
var_choices <-  c('minutes_played',  'usage_rate',  'true_shooting',  'effective_shooting_rate',	'turnover_rate', 'offensive_rebound_rate',	'defensive_rebound_rate',	'offensive_rating',	'defensive_rating')
basicstats$Player <- as.character(basicstats$Player)
var_choices_basic <- c('Mins_Played', 'Gamescore', 'Win', 'lsuscore', 'oppscore', 'MP', 'FG', 'FGA', 'FG_Per', 'Two_Pointers', 'Two_Pointers_Attempted', 'Two_Point_Per', 'Three_Pointers', 'Three_Pointers_Attempted', 'Three_Point_Per', 'FT', 'FTA', 'Free_Throw_Per', 'ORB', 'DRB', 'TotalRB', 'AST', 'STL', 'BLK', 'TOV', 'PF', 'PTS', 'Total_shots')

advancedmelted <- melt(advanced, measure.vars=c(var_choices))
advancedmelted$variable <- as.character(advancedmelted$variable)
homevawaymelted <- melt(homevaway)
homevawaymelted$variable <- as.character(homevawaymelted$variable)
#homevawaymelted$Player_HA <- paste(homevawaymelted$Player, " ", homevawaymelted$Home_Away)
basicstatsmelted <- melt(basicstats, id.vars = c('Player', 'Game', 'opponent', 'home_away', 'conference'))
basicstatsmelted$variable <- as.character(basicstatsmelted$variable)
basicstatsmelted$opponent <- as.character(basicstatsmelted$opponent)
basicstatsmelted$value <- as.numeric(basicstatsmelted$value)

shotchart1 <- read.csv('shot_data1.csv')
heatmap <- read.csv('heatmap_data.csv')

courtImg.URL <- "http://www.pyware.com/images/newproducts/NBABBALL1.jpg"
court <- rasterGrob(readJPEG(getURLContent(courtImg.URL)),
                    width=unit(1,"npc"), height=unit(1,"npc"))

# courtImg.URL2 <- "http://www.winnetkabullets.com/North_America_half_courtbw.jpg"
# court2 <- rasterGrob(readJPEG(getURLContent(courtImg.URL2)),
#                     width=unit(1,"npc"), height=unit(1,"npc"))

img <- readPNG("LSU_Halfcourt.png")
#img <- readJPEG("court.jpeg")
court2 <- rasterGrob(img,
                    width=unit(1,"npc"), height=unit(1,"npc"))

shinyServer(
  function(input, output) {
    
  dataInput <- reactive({
    playerdata <- pm[pm$Player == input$Player & pm$Conference %in% input$conf & pm$Home_Away %in% input$home,]
  })
    
  output$plot1 <- reactive({    
    dataInput() %>% 
      ggvis(~Mins_Played, ~Gamescore) %>%
      layer_points(fill = ~Home_Away, stroke := 'purple') %>%
      layer_lines(y = 10, fill := 'black') %>%
      #layer_smooths(stroke := 'purple', span = .99) #%>%
      layer_model_predictions(model = input$model, se = FALSE, stroke := 'purple') 
  }) %>% bind_shiny(c("ggvis"))
#   
#   dataInput2 <- reactive({
# 
#     advancedmelted <- advancedmelted[advancedmelted$variable == input$stat & advancedmelted$value > 0 & advancedmelted$Player %in% input$players,] 
#     })
#   
#   output$plot2 <- reactive({    
#     dataInput2() %>% ggvis(x = ~Player, y = ~value) %>%
#           layer_bars(fill = ~Player) %>%
#             add_axis("y", title = input$stat) %>%
#               add_axis('x', title='Player')
#     })  %>% bind_shiny(c("bar"))  
#   
  dataInput3 <- reactive({
    homevawaymelted <- homevawaymelted[homevawaymelted$variable == input$hastat & homevawaymelted$Player %in% input$haplayers,] 
    })
  
  output$plot3 <- reactive({    
    dataInput3() %>% 
      mutate(Player_HA = factor(paste(Player, " ", Home_Away))) %>%
      ggvis(x = ~Player_HA, y = ~value) %>%
      layer_bars(fill = ~Home_Away) %>%
      add_axis("y", title = input$hastat) %>%
      add_axis('x', title='Player')
  })  %>% bind_shiny(c("homeaway"))  
  
  
  dataInput4 <- reactive({
    vars <- basicstatsmelted[basicstatsmelted$Player == input$basicplayers & basicstatsmelted$variable == input$basicstat,] 
    })  
  
  output$plot4 <- reactive({    
    dataInput4() %>% ggvis(x = ~Game, y = ~value) %>%
      layer_lines(stroke := 'purple', strokeWidth := 3) %>%
      layer_points(fill = ~home_away , stroke := 'purple') %>%
      #layer_smooths(stroke := 'purple', span = .99) %>%
      add_axis("y", title = input$basicstat)
  })  %>% bind_shiny(c("jacktabloo")) 
  
#   dataInput5 <- reactive({
#     playerdata <- shotchart1[shotchart1$Player == input$basicplayers & basicstatsmelted$variable == input$basicstat,] 
#   }) 
  
  output$plot5 <- renderPlot({    
    playerData <- shotchart1[shotchart1$Player == input$scplayer & shotchart1$assist %in% input$assisted & shotchart1$shot_type %in% input$shot_type,] 
    
    ggplot(playerData, aes(x=V1_Fixed, y=V2)) + 
      annotation_custom(court2, 0, 100, 0, 100) +
      geom_point(aes(colour = class, alpha = .8), size = 4) +
      scale_colour_manual(name = 'Shot Chart', values = c('made' = 'darkgreen', 'missed' = 'red')) + 
      xlim(0, 100) +
      ylim(0, 100) +
      #geom_rug(alpha = .1) +
      coord_fixed() +
      ggtitle(paste('Shot Chart\n', unique(playerData$Player), sep = "")) +
      theme(line = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            legend.title = element_blank(),
            plot.title = element_text(size = 15, lineheight = 0.9, face = 'bold'))
  })
  
output$plot6 <- renderPlot({    
  hmData <- heatmap[heatmap$Player == input$hmplayer,] 
  
  ggplot(hmData, aes(x=x_var, y=y_var)) + 
    annotation_custom(court2, 0, 100, 0, 100) +
    geom_rect(aes(xmin=0, xmax=82, ymin=0, ymax=38, fill = Key), alpha = 0.4) +
    geom_rect(aes(xmin=82, xmax=100, ymin=0, ymax=38, fill = Right_Wing), alpha = 0.4) +
    geom_rect(aes(xmin=0, xmax=60, ymin=38, ymax=61.5, fill = Left_Wing), alpha = 0.4) +
    geom_rect(aes(xmin=60, xmax=100, ymin=38, ymax=61.5, fill = Straight_On), alpha = 0.4) +
    geom_rect(aes(xmin=0, xmax=82, ymin=61.5, ymax=100, fill = Right_Angle), alpha = 0.4) +
    geom_rect(aes(xmin=82, xmax=100, ymin=61.5, ymax=100, fill = Left_Angle), alpha = 0.4) +
    scale_fill_gradient(name = 'Shooting Percentage', limits=c(0, 1), low = "dodgerblue", high = "red") + 
    xlim(0, 100) +
    ylim(0, 100) +
    coord_fixed() +
    theme(line = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          legend.title = element_text(),
          plot.title = element_text(size = 15, lineheight = 0.9, face = 'bold'))
})

  
})


