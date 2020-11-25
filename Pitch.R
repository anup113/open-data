
library(ggplot2)

# Pitch Dimensions for StatsBomb
pitch_statsbomb <- list(
  length = 120,
  width = 80,
  penalty_box_length = 18,
  penalty_box_width = 44,
  six_yard_box_length = 6,
  six_yard_box_width = 20,
  penalty_spot_distance = 12,
  goal_width = 8,
  origin_x = 0,
  origin_y = 0
)

# calculates pitch center
pitch_center <- function(spec) {
  list(x = spec$origin_x + spec$length/2,
       y = spec$origin_y + spec$width/2)
}

# annotates goal post at each half
annotate_goal <- function(colour, fill, spec) {
  midpoint <- pitch_center(spec)
  goal_depth <- 2
  
  list(
    ggplot2::annotate(
      geom = "rect",
      xmin = spec$origin_x + spec$length,
      xmax = spec$origin_x + spec$length + goal_depth,
      ymin = midpoint$y - spec$goal_width/2,
      ymax = midpoint$y + spec$goal_width/2,
      colour = colour,
      fill = fill
    ),
    ggplot2::annotate(
      geom = "rect",
      xmin = spec$origin_x - goal_depth,
      xmax = spec$origin_x,
      ymin = midpoint$y - spec$goal_width/2,
      ymax = midpoint$y + spec$goal_width/2,
      colour = colour,
      fill = fill
    )
  )
}

# creates a pitch 
theme_pitch <- function(color, fill, spec) {
  ggplot2::annotate(
    geom = "rect",
    xmin = spec$origin_x,
    xmax = spec$length,
    ymin = spec$origin_y,
    ymax = spec$width,
    color = color,
    fill = fill,
  )
}

# annotates penalty box
annotate_penalty_box <- function(colour, fill, spec) {
  midpoint <- pitch_center(spec)
  list(
    ggplot2::annotate(
      geom = "rect",
      xmin = spec$origin_x,
      xmax = spec$origin_x + spec$penalty_box_length,
      ymin = midpoint$y - spec$penalty_box_width/2,
      ymax = midpoint$y + spec$penalty_box_width/2,
      colour = colour,
      fill = fill
    ),
    ggplot2::annotate(
      geom = "rect",
      xmin = spec$origin_x + spec$length - + spec$penalty_box_length,
      xmax = spec$origin_x + spec$length,
      ymin = midpoint$y - spec$penalty_box_width/2,
      ymax = midpoint$y + spec$penalty_box_width/2,
      colour = colour,
      fill = fill
    )
  )
}

# annotates penalty box
annotate_six_yard_box <- function(colour, fill, spec) {
  midpoint <- pitch_center(spec)
  list(
    ggplot2::annotate(
      geom = "rect",
      xmin = spec$origin_x,
      xmax = spec$origin_x + spec$six_yard_box_length,
      ymin = midpoint$y - spec$six_yard_box_width/2,
      ymax = midpoint$y + spec$six_yard_box_width/2,
      colour = colour,
      fill = fill
    ),
    ggplot2::annotate(
      geom = "rect",
      xmin = spec$origin_x + spec$length - + spec$six_yard_box_length,
      xmax = spec$origin_x + spec$length,
      ymin = midpoint$y - spec$six_yard_box_width/2,
      ymax = midpoint$y + spec$six_yard_box_width/2,
      colour = colour,
      fill = fill
    )
  )
}


annotate_center_line <- function(color, spec){
  midpoint <- pitch_center(spec)
  ggplot2::annotate(
    geom = "rect",
    xmin = midpoint$x,
    xmax = midpoint$x,
    ymin = spec$origin_y,
    ymax = spec$width,
    color = color,
  )
}

annotate_center_circle <- function(color, fill, spec) {
  midpoint <- pitch_center(spec)
  ggplot2::annotation_custom(
    grob = grid::circleGrob(gp = grid::gpar(col  = color, fill = fill, lwd  = 1.5)),
    xmin = midpoint$x - spec$penalty_spot_distance,
    xmax = midpoint$x + spec$penalty_spot_distance,
    ymin = midpoint$y - spec$penalty_spot_distance,
    ymax = midpoint$y + spec$penalty_spot_distance
  )
}



goal <- annotate_goal("black", "white", pitch_statsbomb)
penalty <- annotate_penalty_box("black", "white", pitch_statsbomb)
six_yard <- annotate_six_yard_box("black", "white", pitch_statsbomb)
center_line <- annotate_center_line("black", pitch_statsbomb)
center_circle <- annotate_center_circle("black", "white", pitch_statsbomb)

ggplot() + theme_pitch("black", "white", pitch_statsbomb) + 
  penalty + 
  goal +
  six_yard +
  center_circle +
  center_line

# Full Pitch funciton 
plot_full_pitch <- function(color){
  ggplot() + theme_pitch(color, "white", pitch_statsbomb) + 
    annotate_penalty_box(color, "white", pitch_statsbomb) + 
    annotate_goal(color, "white", pitch_statsbomb) +
    annotate_six_yard_box(color, "white", pitch_statsbomb) +
    annotate_center_circle(color, "white", pitch_statsbomb) +
    center_line
}

# Anotate the lines 
annotate_shots <- function(location_x, location_y, shot_end_x, shot_end_y ){
  ggplot2::annotate(
    geom = "segment",
    x = location_x,
    xend = shot_end_x,
    y = location_y,
    yend = shot_end_y,
    color = 'red',
  )
}
