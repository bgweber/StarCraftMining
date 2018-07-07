# libraries 
library(ggplot2)
library(gganimate)
library(ggforce)
library(tweenr)
library(dplyr)

# video settings 
fps <- 24
nframes <- 5000

# load the replay data 
df <- read.csv("PvT.csv")

# lag the x and y coordinates to plot lines of movement 
df <-  df %>%
  group_by(unitID) %>%
  mutate(x1 = dplyr::lag(x, n = 1, default = NA),
         x2 = dplyr::lag(x, n = 2, default = NA),
         y1 = dplyr::lag(y, n = 1, default = NA), 
         y2 = dplyr::lag(y, n = 2, default = NA)) 

# set up graphics 
df$colour <- ifelse(df$pID == 0, 'RED', 'GREEN')
df$time <- df$frame / fps
df$ease = 'linear'
df$size = 5
df$alpha = 1

# Use tweenr
dt <- tween_elements(df, 'time', 'unitID', 'ease', nframes = nframes)

# Animate with gganimate
p <- ggplot(data = dt) + 
  geom_point(aes(x=0, y=0, size=10, alpha=0, frame = .frame)) + 
  geom_segment(aes(x=x1, y=y1, xend=x2, yend=y2, size=1, alpha=0.5, frame = .frame, color = colour)) + 
  geom_segment(aes(x=x, y=y, xend=x1, yend=y1, size=1, alpha=1, frame = .frame, color = colour)) + 
  geom_point(aes(x=x, y=y, size=size, alpha=alpha, frame = .frame, color = colour)) + 
  scale_size(guide = 'none') + 
  scale_colour_identity() + 
  scale_alpha(range = c(0, 1), guide = 'none') + 
  ggforce::theme_no_axes()

# make the video 
animation::ani.options(interval = 1/fps)
gganimate(p, "output.mp4", ani.width=640, ani.height=640 ,title_frame = FALSE)
