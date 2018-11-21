# This uses Evolving Wild's data to plot faceoff data, 
# namely faceoffs taken and faceoff win % at each faceoff dot.

# As of November 21, 2018, this tool was for their Patreon patrons. If you aren't one, 
# you should consider it, not just for easy access to this data. 
# If you are, you use their tool to get the data you want. Make sure to set the event type to 'FAC'
# The code filters for that if you forget, but the starting file will be smaller if you remember. 

# I'm sure this code could be more efficient, so be gentle. 
# nabs and which_circle are functions written by Manny from Corsica. 
# Some of this is based on a tutorial on Matt Barlowe's site written by Jake Flancer. 

# To make this work for you, you'll need to look at the following:

# use lines 139-152 to filter for a player or the entire team. 
# Run the code up to line 216. 
# Lastly, find the section after line 220 that is what you want: team, player, etc. Make the appropriate
# changes in the plot code, then run the two sections for the appropriate output.

# For example, if you are plotting all faceoffs for the team, you would edit and run lines 220-231.

library(tidyverse)
library(tidyr)
library(stringr)
library(readr)

# Change the directory here to the one you have RinkFunction.R in
source('~/Desktop/R For Data Science/RinkTest/RinkFunction.R')

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

# This reads the file into pbp_file. You'll have to change this to what your filename is. Save it 
# in your working directory.
pbp_file <- read_csv("EW5v5FODetroit.csv")

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
    fixSwitch = 1
  )

# Above, we made the column "flip," which is equal to -1 for period 2. By doing this, we can 
# have all the coordinates reflect the same faceoff dots. We then multiplied both coordinates
# in every case by -1 because the default has the home team attacking to the left. 

# Put the data in a file that won't get changed (for testing)
master_file <- game_file

# Some rinks plot their locations backwards. This fixes that. 

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

# This will flip coordinates for rinks that do their coordinates backwards from Detroit

game_file$x <- game_file$x * game_file$fixSwitch
game_file$y <- game_file$y * game_file$fixSwitch
# This runs which_circle to input faceoff dot locations
game_file$dotLocation <- st.which_circle(game_file$x,game_file$y)

### From here on out, each section will take the master file and filtering for different things

## Filter for a player

# Filter for Dylan Larkin

#faceoff_file <- filterPlayer('DYLAN.LARKIN')

# Filter for Andreas Athanasiou
faceoff_file <- filterPlayer('ANDREAS.ATHANASIOU')

# Filter for Frans Nielsen
#faceoff_file <- filterPlayer('FRANS.NIELSEN')

# If you want the whole team, uncomment this next line, and comment out the above line. 
#faceoff_file <- temp_file

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

# This gets rid of NaN results
faceoff <- na.omit(faceoff)

# Prepares to draw rink
rink <- fun.draw_rink() + coord_fixed()

# Plot faceoff percentages for each location

faceoff_plot <- rink +
  geom_text(data=faceoff, size=2, aes(x=x,y=y,label=paste(WinPerc,"%","\n",Taken,"taken"))) +
  ggtitle("  Detroit Red Wings 2018-19 5v5 Faceoff Win %", 
          subtitle = "  Detroit attacks to the right. No number means no faceoffs taken at that circle") +
  labs(caption = "by Peter Flynn @pflynnhockey   ")
theme(plot.title=element_text(size=14, color="black")) +
  theme(plot.subtitle=element_text(size=10, color="black"))

# Full Team
png("~/Desktop/Detroit5v5FaceoffsTestEW.png", units="px", width=1800, height=1957, res=300)
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

png("~/Desktop/Larkin5v5Faceoffs.png", units="px", width=1800, height=1957, res=300)
print(faceoff_plot)
dev.off()

# Andreas Athanasiou

faceoff_plot <- rink +
  
  geom_text(data=faceoff, size=2, aes(x=x,y=y,label=paste(WinPerc,"%","\n",Taken,"taken"))) +
  ggtitle("  Andreas Athanasiou 2018-19 5v5 Faceoff Win %", 
          subtitle = "  Detroit attacks to the right. No number means no faceoffs taken at that circle") +
  labs(caption = "by Peter Flynn @pflynnhockey   ")
theme(plot.title=element_text(size=14, color="black")) +
  theme(plot.subtitle=element_text(size=10, color="black"))

png("~/Desktop/AA5v5.png", units="px", width=1800, height=1957, res=300)
print(faceoff_plot)
dev.off()

# Frans Nielsen

faceoff_plot <- rink +
  geom_text(data=faceoff, size=2, aes(x=x,y=y,label=paste(WinPerc,"%","\n",Taken,"taken"))) +
  ggtitle("  Frans Nielsen 2018-19 5v5 Faceoff Win %", 
          subtitle = "  Detroit attacks to the right. No number means no faceoffs taken at that circle") +
  labs(caption = "by Peter Flynn @pflynnhockey   ")
theme(plot.title=element_text(size=14, color="black")) +
  theme(plot.subtitle=element_text(size=10, color="black"))

png("~/Desktop/Nielsen5v5.png", units="px", width=1800, height=1957, res=300)
print(faceoff_plot)
dev.off()
