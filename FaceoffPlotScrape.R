# This uses Manny's play by play scraper to plot faceoff data, 
# namely faceoffs taken and faceoff win % at each faceoff dot.
# I'm sure this code could be more efficient, so be gentle. 
# nabs and which_circle are functions written by Manny from Corsica. 
# Some of this is based on a tutorial on Matt Barlowe's site written by Jake Flancer. 

# To make this work for you, you'll need to look at the following:
# Use the appropriate season and gameID in lines 25-26.
# Use lines 110 to 113 to filter for team strength if necessary.
# use lines 159 to 171 to filter for a player or the entire team. 
# Run the code up to line 235. 
# Lastly, find the section after line 241 that is what you want: team, player, etc. Make the appropriate
# changes in the plot code, then run the two sections for the appropriate output. 

library(tidyverse)
library(tidyr)
library(stringr)

# Change the directory here to the one you have RinkFunction.R in
source('~/Desktop/R For Data Science/RinkTest/RinkFunction.R')

# Use appropriate season. GameID can be a single game or a group of games. Use a comma to separate
# multiple games. c(20010,2021) etc. You can also use : to get a consecutive group of games.
# c(20010:21000)
Season <- "20182019"
game_ID <- c(20010)

# API to get game ids. Change teamID to appropriate team. Change dates to correct time frame.
#https://statsapi.web.nhl.com/api/v1/schedule?teamId=17&startDate=2018-10-01&endDate=2019-07-10

## General Functions
# Numeric Absolute
nabs <- function(x) {
  
  ## Description
  # nabs() returns x after first converting it to class numeric via character
  # Its primary use is converting objects of class factor to numeric
  # It also provides a more concise wrapper for standard numeric conversion
  
  return(as.numeric(as.character(x)))
  
}

# Which Circle

st.which_circle <- function(x, y) {
  
  ## Description
  # which_circle() returns the faceoff circle number nearest to a location corresponding to a given \
  # set of coordinates
  
  circle <- 1*(nabs(x) <= -25 & nabs(y) > 0) +
    2*(nabs(x) <= -25 & nabs(y) < 0) +
    3*(nabs(x) < -2 & nabs(x) > -25 & nabs(y) > 0) +
    4*(nabs(x) < -2 & nabs(x) > -25 & nabs(y) < 0) +
    5*(nabs(x) > -2 & nabs(x) < 2) +
    6*(nabs(x) > 2 & nabs(x) < 25 & nabs(y) > 0) +
    7*(nabs(x) > 2 & nabs(x) < 25 & nabs(y) < 0) +
    8*(nabs(x) >= 25 & nabs(y) > 0) +
    9*(nabs(x) >= 25 & nabs(y) < 0)
  
  return(circle)
  
}

# filterPlayer
# This function filters for a specific player

filterPlayer <- function(name){
  
  faceoff_file <- temp_file %>% filter(event_player_1 == name | 
                                         event_player_2 == name)
  return(faceoff_file)
}

# This creates the file to store the faceoff data 
pbp_file <- ds.compile_games(games = game_ID,
                             season = Season,
                             pause = 2,
                             try_tolerance = 5,
                             agents = ds.user_agents)[[1]]

home_team <- pbp_file$home_team[1]
away_team <- pbp_file$away_team[1]
date <- pbp_file$game_date[1]
max_period <- max(pbp_file$game_period)

game_file <- pbp_file %>%
  filter(event_type %in% c('FAC')) %>%
  select(game_seconds,
         event_type,
         event_team,
         event_description,
         event_player_1,
         event_player_2,
         coords_x,
         coords_y,
         game_strength_state,
         home_team,
         away_team, 
         game_period
  ) %>%
  mutate(
    flip = ifelse(as.numeric(game_period)%%2 == 0,-1,1),
    x = nabs(coords_x)*flip*-1,
    y = nabs(coords_y)*flip*-1,
    DETwin  = ifelse(event_team == 'DET', 1,0),
    DETloss = ifelse(DETwin == 1,0,1),
    dotLocation = nabs(0),
    temp = ifelse(game_strength_state == '5v4' & home_team == 'DET' |
                    game_strength_state == '4v5' & away_team == 'DET',1,0),
    # the line above will filter for 5v4. The line below will filter for 5v5. Leave one commented out
    #temp = ifelse(home_team == 'DET' | away_team == 'DET',1,0),
    fixSwitch = 1
  )

# Above, we made the column "flip," which is equal to -1 for period 2. By doing this, we can 
# have all the coordinates reflect the same faceoff dots. We then multiplied both coordinates
# in every case by -1 because the default has the home team attacking to the left. 

game_file$dotLocation <- st.which_circle(game_file$x,game_file$y)

# Filters the list so it only has Detroit faceoffs

game_file <- game_file %>% filter(temp == '1')

# Put the data in a file that won't get changed
master_file <- game_file

# Some rinks plot their locations backwards. This fixes that. I'm sure there is a more elegant solution
# but for now, this works. 

game_file$fixSwitch <- ifelse((game_file$home_team == 'L.A' | 
                                 game_file$home_team == 'MTL' | 
                                 game_file$home_team == 'CAR' |
                                 game_file$home_team == 'FLA' |
                                 game_file$home_team == 'DAL' |
                                 game_file$home_team == 'VGK' |
                                 game_file$home_team == 'TOR' |
                                 game_file$home_team == 'NYR' |
                                 game_file$home_team == 'NYI' |
                                 game_file$home_team == 'PHI' |
                                 game_file$home_team == 'CHI' |
                                 game_file$home_team == 'WSH' |
                                 game_file$home_team == 'MIN' |
                                 game_file$home_team == 'WPG' |
                                 game_file$home_team == 'S.J' |
                                 game_file$home_team == 'COL'),-1,1)

# This next code will flip the coordinates for the above rinks.
game_file$x <- game_file$x * game_file$fixSwitch
game_file$y <- game_file$y * game_file$fixSwitch

# This uses the which_circle function to say which faceoff circle each faceoff is at.
game_file$dotLocation <- st.which_circle(game_file$x,game_file$y)

### From here on out, each section will take the game file and filter for different things

## Filter for a player. If you want to filter for a specific player, replace the player's name.

# Filter for Dylan Larkin
#faceoff_file <- filterPlayer('DYLAN.LARKIN')

# Filter for Andreas Athanasiou
#faceoff_file <- filterPlayer('ANDREAS.ATHANASIOU')

# Filter for Frans Nielsen
faceoff_file <- filterPlayer('FRANS.NIELSEN')

# If you want the whole team, uncomment this next line, and comment out any player lines. 
#faceoff_file <- game_file

# This creates a separate dataframe for each faceoff dot. 
faceoff_file_dot1 <- faceoff_file %>% filter(dotLocation == 1)
faceoff_file_dot2 <- faceoff_file %>% filter(dotLocation == 2)
faceoff_file_dot3 <- faceoff_file %>% filter(dotLocation == 3)
faceoff_file_dot4 <- faceoff_file %>% filter(dotLocation == 4)
faceoff_file_dot5 <- faceoff_file %>% filter(dotLocation == 5)
faceoff_file_dot6 <- faceoff_file %>% filter(dotLocation == 6)
faceoff_file_dot7 <- faceoff_file %>% filter(dotLocation == 7)
faceoff_file_dot8 <- faceoff_file %>% filter(dotLocation == 8)
faceoff_file_dot9 <- faceoff_file %>% filter(dotLocation == 9)

# FaceoffWinPerc# is the faceoff win percentage at each faceoff dot. 

FaceoffWinPerc1 <- sum(faceoff_file_dot1$DETwin == 1) / sum(faceoff_file_dot1$DETwin == 1 | 
                                                              faceoff_file_dot1$DETloss == 1) * 100
FaceoffWinPerc2 <- sum(faceoff_file_dot2$DETwin == 1) / sum(faceoff_file_dot2$DETwin == 1 | 
                                                              faceoff_file_dot2$DETloss == 1) * 100
FaceoffWinPerc3 <- sum(faceoff_file_dot3$DETwin == 1) / sum(faceoff_file_dot3$DETwin == 1 | 
                                                              faceoff_file_dot3$DETloss == 1) * 100
FaceoffWinPerc4 <- sum(faceoff_file_dot4$DETwin == 1) / sum(faceoff_file_dot4$DETwin == 1 | 
                                                              faceoff_file_dot4$DETloss == 1) * 100
FaceoffWinPerc5 <- sum(faceoff_file_dot5$DETwin == 1) / sum(faceoff_file_dot5$DETwin == 1 | 
                                                              faceoff_file_dot5$DETloss == 1) * 100
FaceoffWinPerc6 <- sum(faceoff_file_dot6$DETwin == 1) / sum(faceoff_file_dot6$DETwin == 1 | 
                                                              faceoff_file_dot6$DETloss == 1) * 100
FaceoffWinPerc7 <- sum(faceoff_file_dot7$DETwin == 1) / sum(faceoff_file_dot7$DETwin == 1 | 
                                                              faceoff_file_dot7$DETloss == 1) * 100
FaceoffWinPerc8 <- sum(faceoff_file_dot8$DETwin == 1) / sum(faceoff_file_dot8$DETwin == 1 | 
                                                              faceoff_file_dot8$DETloss == 1) * 100
FaceoffWinPerc9 <- sum(faceoff_file_dot9$DETwin == 1) / sum(faceoff_file_dot9$DETwin == 1 | 
                                                              faceoff_file_dot9$DETloss == 1) * 100

FaceoffTaken1 <- sum(faceoff_file_dot1$DETwin == 1 | faceoff_file_dot1$DETloss == 1)
FaceoffTaken2 <- sum(faceoff_file_dot2$DETwin == 1 | faceoff_file_dot2$DETloss == 1)
FaceoffTaken3 <- sum(faceoff_file_dot3$DETwin == 1 | faceoff_file_dot3$DETloss == 1)
FaceoffTaken4 <- sum(faceoff_file_dot4$DETwin == 1 | faceoff_file_dot4$DETloss == 1)
FaceoffTaken5 <- sum(faceoff_file_dot5$DETwin == 1 | faceoff_file_dot5$DETloss == 1)
FaceoffTaken6 <- sum(faceoff_file_dot6$DETwin == 1 | faceoff_file_dot6$DETloss == 1)
FaceoffTaken7 <- sum(faceoff_file_dot7$DETwin == 1 | faceoff_file_dot7$DETloss == 1)
FaceoffTaken8 <- sum(faceoff_file_dot8$DETwin == 1 | faceoff_file_dot8$DETloss == 1)
FaceoffTaken9 <- sum(faceoff_file_dot9$DETwin == 1 | faceoff_file_dot9$DETloss == 1)

# Creates a tibble with each faceoff dot and the player's win % at each dot. 
faceoff <- tibble('Location'=c(1,2,3,4,5,6,7,8,9),'x'=c(-68,-68,-17,-17,7,17,17,70,70),
                  'y'=c(31,-30,29,-30,2,29,-30,31,-30),'WinPerc'=c(FaceoffWinPerc1,FaceoffWinPerc2,
                                                                   FaceoffWinPerc3,FaceoffWinPerc4,
                                                                   FaceoffWinPerc5,FaceoffWinPerc6,
                                                                   FaceoffWinPerc7,FaceoffWinPerc8,
                                                                   FaceoffWinPerc9),
                  'Taken'=c(FaceoffTaken1,FaceoffTaken2,FaceoffTaken3,FaceoffTaken4,
                            FaceoffTaken5,FaceoffTaken6,FaceoffTaken7,FaceoffTaken8,
                            FaceoffTaken9)
                  
)
# This rounds the numbers to 3 decimal points. 
is.num <- sapply(faceoff, is.numeric)
faceoff[is.num] <- lapply(faceoff[is.num], round, 0)

# Gets rid of NaN results
faceoff <- na.omit(faceoff)

# Sets up the rink using RinkFunction.R
rink <- fun.draw_rink() + coord_fixed()

## Plot faceoff percentages for each location
## This is set up right now for Detroit 5v4 faceoffs. You'll need to change ggtitle and subtitle
## for your team and/or players. 

faceoff_plot <- rink +
  geom_text(data=faceoff, size=2, aes(x=x,y=y,label=paste(WinPerc,"%","\n",Taken,"taken"))) +
  ggtitle("  Detroit Red Wings 2018-19 5v4 Faceoff Win %", 
          subtitle = "  Detroit attacks to the right. No number means no faceoffs taken at that circle") +
  labs(caption = "by Peter Flynn @pflynnhockey   ")
theme(plot.title=element_text(size=14, color="black")) +
  theme(plot.subtitle=element_text(size=10, color="black"))

## These sections output a png plot. Change the source (~Desktop/" etc) to what makes sense for you.
# Full Team
png("~/Desktop/20182019Detroit5v4Faceoffs.png", units="px", width=1800, height=1957, res=300)
print(faceoff_plot)
dev.off()

# Dylan Larkin

faceoff_plot <- rink +
  geom_text(data=faceoff, size=2, aes(x=x,y=y,label=paste(WinPerc,"%","\n",Taken,"taken"))) +
  ggtitle("  Dylan Larkin 2018-19 5v4 Faceoff Win %", 
          subtitle = "  Detroit attacks to the right. No number means no faceoffs taken at that circle") +
  labs(caption = "by Peter Flynn @pflynnhockey   ")
theme(plot.title=element_text(size=14, color="black")) +
  theme(plot.subtitle=element_text(size=10, color="black"))

png("~/Desktop/20182019Larkin5v4Faceoffs.png", units="px", width=1800, height=1957, res=300)
print(faceoff_plot)
dev.off()

# Andreas Athanasiou

faceoff_plot <- rink +
  
  geom_text(data=faceoff, size=2, aes(x=x,y=y,label=paste(WinPerc,"%","\n",Taken,"taken"))) +
  ggtitle("  Andreas Athanasiou 2018-19 5v4 Faceoff Win %", 
          subtitle = "  Detroit attacks to the right. No number means no faceoffs taken at that circle") +
  labs(caption = "by Peter Flynn @pflynnhockey   ")
theme(plot.title=element_text(size=14, color="black")) +
  theme(plot.subtitle=element_text(size=10, color="black"))

png("~/Desktop/AA5v4.png", units="px", width=1800, height=1957, res=300)
print(faceoff_plot)
dev.off()

# Frans Nielsen

faceoff_plot <- rink +
  geom_text(data=faceoff, size=2, aes(x=x,y=y,label=paste(WinPerc,"%","\n",Taken,"taken"))) +
  ggtitle("  Frans Nielsen 2018-19 5v4 Faceoff Win %", 
          subtitle = "  Detroit attacks to the right. No number means no faceoffs taken at that circle") +
  labs(caption = "by Peter Flynn @pflynnhockey   ")
theme(plot.title=element_text(size=14, color="black")) +
  theme(plot.subtitle=element_text(size=10, color="black"))

png("~/Desktop/20182019Nielsen5v4.png", units="px", width=1800, height=1957, res=300)
print(faceoff_plot)
dev.off()
