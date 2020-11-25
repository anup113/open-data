
# Required packages
#   tidyverse - manipulating data
#   devtools - allows to download packages from github
#   ggplot2 - data visualizaiton 

#################################################################################
# Place were libraries and imports here

library(tidyverse)
library(devtools)
library(ggplot2)
source('Pitch.R')
################################################################################



################################################################################
# Load the csv file
messi <- read.csv("messi_laliga_goals.csv")
################################################################################



################################################################################
# Distribution of goals based on the play type and shot technique used

ggplot(messi, aes(x=play_pattern_name, fill=shot_technique_name)) + 
  geom_bar() +
  scale_fill_brewer(palette='Set1')+
  theme_light() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
################################################################################

################################################################################
# Distribution of goals based on the minutes in the timer


mt <- ggplot(messi, aes(minute, fill=shot_technique_name)) + 
  geom_histogram(binwidth = 1, alpha = 0.5, color="black")

# use brewer color palettes
mt + scale_color_brewer(palette='Set1') +
  scale_fill_brewer(palette='Set1')+
  theme_classic()

# Observation - Messi has scored from every minute of the play from 2 to 93
# He has not scored yet scored in the first minute of the game in any
# mathches.

# His most active window is at the backend of the game 
################################################################################

################################################################################
# Distribution of goals based on the seconds in the timer

st <- ggplot(messi, aes(second, fill=shot_technique_name)) + 
  geom_histogram(binwidth = 1, alpha = 0.5, color="black")

# use brewer color palettes
st + scale_color_brewer(palette='Set1') +
  scale_fill_brewer(palette='Set1')+
  theme_classic()

# Observations - Messi has scored in every second from 0-59th 

# He has scored most number of goals in the 16th second 
################################################################################

################################################################################
# Visualizing all the shots that messi took which ended up as being goals

# load the location of the shot
x.locations <- messi$location_x
y.locations <- messi$location_y

# load where shot ended up in the net
x.shotend <- messi$shot_end_location_x
y.shotend <- messi$shot_end_location_y

# create a pitch plot
p <- plot_full_pitch('black')

# iterate over every shots and annotate that into the pitch 
for(i in 0:length(x.locations)){
  p <- p + annotate_shots(x.locations[i], y.locations[i], x.shotend[i], y.shotend[i])
}

# visualize the pitch with shots annotated
p + theme_void()
################################################################################

################################################################################
# Who was the opposition goal keeper when messi scored his goals

ggplot(messi, aes(x=opposition_gk_name, fill=shot_technique_name)) + 
  geom_bar() +
  scale_fill_brewer(palette='Set1')+
  theme_light() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Missing data for more than 50 goals 
################################################################################

