# 1 Import Libraries --------------------------------------------------------

library(tidyverse)
library(DescTools)
library(padr)
library(ggplot2)
library(patchwork)
library(randomForest)
library(xgboost)
library(AUC)
library(MLmetrics)
library(ModelMetrics)
library(caret)
library(zoo)
library(lme4)
library(ggplot2)
library(png)
library(grid)
library(raster)
library(magick)
library(gt)

# 2 NHL Rink Functions (not used in this paper) ------------------------------------------------------


create_circle = function(center = c(0, 0), npoints = 500, diameter = 1, start = 0, end = 2, color = '#c8012e', fill = NA){
  pts = seq(start * pi, end * pi, length.out = npoints)
  data.frame(x = center[1] + ((diameter/2) * cos(pts)),
             y = center[2] + ((diameter/2) * sin(pts)))
}

make_faceoff_detail = function(center){
  data.frame(
    xmin = c(center[1] - 2, center[1] - 2, center[1] + 2, center[1] + 2, center[1] - 2, center[1] - 2, center[1] + 2, center[1] + 2, center[1] - ((5 + (9/12)) / 2), center[1] + ((5 + (9/12)) / 2), center[1] - ((5 + (9/12)) / 2), center[1] + ((5 + (9/12)) / 2)),
    xmax = c(center[1] - 6, center[1] - 6, center[1] + 6, center[1] + 6, center[1] - (2 + (2/12) ), center[1] - (2 + (2/12) ), center[1] + (2 + (2/12) ), center[1] + (2 + (2/12)), center[1] - ((5 + (7/12)) / 2), center[1] + ((5 + (7/12)) / 2), center[1] - ((5 + (7/12)) / 2), center[1] + ((5 + (7/12)) / 2)),
    ymin = c(center[2] - (.75 + (2/12)), center[2] + .75, center[2] - (.75 + (2/12)), center[2] + .75, center[2] - .75, center[2] + .75, center[2] - .75, center[2] + .75, center[2] - (15 - (2/12)), center[2] - (15 - (2/12)), center[2] + (15 - (2/12)), center[2] + (15 - (2/12))),
    ymax = c(center[2] - .75, center[2] + (.75 + (2/12)), center[2] - .75, center[2] + (.75 + (2/12)), center[2] - 3.75, center[2] + 3.75, center[2] - 3.75, center[2] + 3.75, center[2] - 17, center[2] - 17, center[2] + 17, center[2] + 17)
  )
}

make_faceoff_dot = function(spot){
  center = c(0, 0)
  
  dot = create_circle(center = center, diameter = 2, start = -1/2, end = 1/2)
  dot = rbind(dot, data.frame(x = 0, y = 1 - (4/12)))
  dot = rbind(dot, create_circle(center = center, diameter = 2 - (4/12), start = 1/2, end = -1/2))
  dot = rbind(dot, data.frame(x = 0, y = -1))
  dot = rbind(dot, data.frame(x = -dot$x, y = dot$y))
  
  dot_fill = create_circle(center = center, diameter = 2 - (3.99/12), start = acos(7/10)/pi, end = .5 + (acos(7/10)/pi))
  dot_fill = rbind(dot_fill, data.frame(x = rev(dot_fill$x), y = -rev(dot_fill$y)))
  
  dot$x = dot$x + spot[1]
  dot$y = dot$y + spot[2]
  
  dot_fill$x = dot_fill$x + spot[1]
  dot_fill$y = dot_fill$y + spot[2]
  
  return(list(dot = dot, dot_fill = dot_fill))
}

blocked_center_line = function(center){
  data.frame(
    xmin = -3/12,
    xmax = 3/12,
    ymin = center - 5/12,
    ymax = center + 5/12
  )
}



draw_nhl = function(){
  #nhl_logo = readPNG('League Logos/nhl.png')
  #nhl_logo = rasterGrob(nhl_logo, interpolate = TRUE)
  
  logo_borders = data.frame(
    xmin = sqrt(225 - (15/sqrt(2)) ^ 2) + 2,
    xmax = sqrt(225 - (15/sqrt(2)) ^ 2) + 2,
    ymin = sqrt(225 - (15/sqrt(2)) ^ 2) + 2,
    ymax = sqrt(225 - (15/sqrt(2)) ^ 2) + 2
  )
  
  boards = rbind(
    data.frame(x = 0, y = 42.5),
    data.frame(x = 72, y = 42.5),
    create_circle(center = c(72, 14.5), diameter = 56, start = 1/2, end = 0),
    create_circle(center = c(72, -14.5), diameter = 56, start = 0, end = -1/2),
    data.frame(x = 72, y = -42.5),
    data.frame(x = 0, y = -42.5),
    data.frame(x = 0, y = -42.5 - (2/12)),
    data.frame(x = 72, y = -42.5 - (2/12)),
    create_circle(center = c(72, -14.5), diameter = 56 + (4/12), start = -1/2, end = 0),
    create_circle(center = c(72, 14.5), diameter = 56 + (4/12), start = 0, end = 1/2),
    data.frame(x = 72, y = 42.5 + (2/12)),
    data.frame(x = 0, y = 42.5 + (2/12)),
    data.frame(x = 0, y = 42.5)
  )
  
  boards = rbind(boards, data.frame(x = -boards$x, y = boards$y))
  
  # Create center circle
  center_circle = create_circle(center = c(0, 0), diameter = 30, start = -1/2, end = 1/2)
  center_circle = rbind(center_circle, data.frame(x = 0, y = 15 - (2/12)))
  center_circle = rbind(center_circle, create_circle(center = c(0, 0), diameter = 30 - (4/12), start = 1/2, end = -1/2))
  center_circle = rbind(center_circle, data.frame(x = -center_circle$x, y = center_circle$y))
  
  # Create faceoff circles
  faceoff_circles_l = create_circle(center = c(0, 0), diameter = 30, start = -1/2, end = 1/2)
  faceoff_circles_l = rbind(faceoff_circles_l, data.frame(x = 0, y = 7 - (2/12)))
  faceoff_circles_l = rbind(faceoff_circles_l, create_circle(center = c(0, 0), diameter = 30 - (4/12), start = 1/2, end = -1/2))
  faceoff_circles_l = rbind(faceoff_circles_l, data.frame(x = 0, y = -7))
  faceoff_circles_l = rbind(faceoff_circles_l, data.frame(x = -faceoff_circles_l$x, y = faceoff_circles_l$y))
  
  faceoff_circles_l$x = faceoff_circles_l$x - 69
  faceoff_circles_l$y = faceoff_circles_l$y - 22
  
  faceoff_circles_l = rbind(faceoff_circles_l, data.frame(x = faceoff_circles_l$x, y = -faceoff_circles_l$y))
  faceoff_circles_r = data.frame(x = -faceoff_circles_l$x, y = faceoff_circles_l$y)
  
  # Make faceoff spots
  center_dot = create_circle(center = c(0, 0), diameter = 1)
  
  l_l_dot = make_faceoff_dot(c(-69, -22))$dot
  l_l_dot_fill = make_faceoff_dot(c(-69, -22))$dot_fill
  
  h_l_dot = make_faceoff_dot(c(-69, 22))$dot
  h_l_dot_fill = make_faceoff_dot(c(-69, 22))$dot_fill
  
  l_nz_l_dot = make_faceoff_dot(c(-20, -22))$dot
  l_nz_l_dot_fill = make_faceoff_dot(c(-20, -22))$dot_fill
  
  h_nz_l_dot = make_faceoff_dot(c(-20, 22))$dot
  h_nz_l_dot_fill = make_faceoff_dot(c(-20, 22))$dot_fill
  
  l_nz_r_dot = make_faceoff_dot(c(20, -22))$dot
  l_nz_r_dot_fill = make_faceoff_dot(c(20, -22))$dot_fill
  
  h_nz_r_dot = make_faceoff_dot(c(20, 22))$dot
  h_nz_r_dot_fill = make_faceoff_dot(c(20, 22))$dot_fill
  
  l_r_dot = make_faceoff_dot(c(69, -22))$dot
  l_r_dot_fill = make_faceoff_dot(c(69, -22))$dot_fill
  
  h_r_dot = make_faceoff_dot(c(69, 22))$dot
  h_r_dot_fill = make_faceoff_dot(c(69, 22))$dot_fill
  
  # Make faceoff details
  faceoff_details = rbind(
    make_faceoff_detail(center = c(-69, -22)),
    make_faceoff_detail(center = c(-69, 22)),
    make_faceoff_detail(center = c(69, -22)),
    make_faceoff_detail(center = c(69, 22))
  )
  
  # Create red line
  red_line = data.frame(xmin = -.5, xmax = .5, ymin = -42.5, ymax = 42.5)
  red_line_detail = blocked_center_line(center = c(seq(-40.5, -1, by = 5), 0))
  red_line_detail = rbind(red_line_detail, data.frame(xmin = red_line_detail$xmin, xmax = red_line_detail$xmax, ymin = -red_line_detail$ymax, ymax = -red_line_detail$ymin))
  
  # Create referee's crease
  ref_crease = create_circle(center = c(0, -42.5), diameter = 20, start = 0, end = 1)
  ref_crease = rbind(ref_crease, create_circle(center = c(0, -42.5), diameter = 20 - (4/12), start = 1, end = 0))
  
  # Create blue line
  blue_line = data.frame(xmin = -26, xmax = -25, ymin = -42.5, ymax = 42.5)
  blue_line = rbind(
    blue_line, 
    data.frame(xmin = -blue_line$xmin, xmax = -blue_line$xmax, ymin = -blue_line$ymin, ymax = -blue_line$ymax)
  )
  
  # Create goal line
  goal_line = data.frame(xmin = 89 - (1/12), xmax = 89 + (1/12), ymin = -36.77, ymax = 36.77)
  goal_line = rbind(goal_line, data.frame(xmin = -89 - (1/12), xmax = -89 + (1/12), ymin = -36.77, ymax = 36.77))
  #goal_line = rbind(goal_line, data.frame(xmin = -goal_line$xmax, xmax = -goal_line$xmin, ymin = goal_line$ymin, ymax = goal_line$ymax))
  
  # Create goal crease
  crease = data.frame(
    x = c(-89, (-83 - (2/12)) - ((1.5 * seq(-4, 4, length = 100)^2) / (4^2)), -89, -89),
    y = c(-4 + (2/12), seq(-4 + (2/12), 4 - (2/12), length = 100), 4 - (2/12), -4 + (2/12))
  )
  
  crease_outline = data.frame(
    x = c(-89, -83 - ((1.5 * seq(-4, 4, length = 100)^2) / (4^2)), -89, -89, -85, -85, -85 + (2/12), -85 + (2/12), rev((-83 - (2/12)) - ((1.5 * seq(-4, 4, length = 100)^2) / (4^2))),-85 + (2/12), -85 + (2/12), -85, -85,-89, -89),
    y = c(-4, seq(-4, 4, length = 100), 4, 4 - (2/12), 4 - (2/12), 4 - (7/12), 4 - (7/12), 4 - (2/12), seq(4 - (2/12), -4 + (2/12), length = 100), -4 + (2/12), -4 + (7/12), -4 + (7/12), -4 + (2/12), -4 + (2/12), -4)
  )
  
  # Create goal
  goal = data.frame(x = -89, y = 3)
  goal = rbind(goal, create_circle(center = c(-89 - (20/12), 2), diameter = (40/12), start = 1/3, end = 1))
  goal = rbind(goal, create_circle(center = c(-89 - (20/12), -2), diameter = (40/12), start = 1, end = 5/3))
  goal = rbind(goal, data.frame(x = -89, y = -3))
  goal = rbind(goal, data.frame(x = -89, y = -3 - (2.375/12)))
  goal = rbind(goal, create_circle(center = c(-89 - (20/12), -2), diameter = (40/12) + (4.75/12), start = 5/3, end = 1))
  goal = rbind(goal, create_circle(center = c(-89 - (20/12), 2), diameter = (40/12) + (4.75/12), start = 1, end = 1/3))
  goal = rbind(goal, data.frame(x = -89, y = 3 + (2.375/12)))
  
  goal_fill = data.frame(x = -89, y = 3)
  goal_fill = rbind(goal_fill, create_circle(center = c(-89 - (20/12), 2), diameter = (40/12), start = 1/3, end = 1))
  goal_fill = rbind(goal_fill, create_circle(center = c(-89 - (20/12), -2), diameter = (40/12), start = 1, end = 5/3))
  goal_fill = rbind(goal_fill, data.frame(x = -89, y = -3))
  
  # Create restricted areas
  left_restricted_area = data.frame(
    x = c(-89, -100, -100, -89, -89),
    y = c(11 - (1/12), 14 - (1/12), 14 + (1/12), 11 + (1/12), 11 - (1/12))
  )
  left_restricted_area = rbind(left_restricted_area, data.frame(x = left_restricted_area$x, y = -left_restricted_area$y))
  
  right_restricted_area = data.frame(
    x = c(89, 100, 100, 89, 89),
    y = c(11 - (1/12), 14 - (1/12), 14 + (1/12), 11 + (1/12), 11 - (1/12))
  )
  right_restricted_area = rbind(right_restricted_area, data.frame(x = right_restricted_area$x, y = -right_restricted_area$y))
  
  
  # Make rink
  ggplot() +
    coord_fixed() +
    theme(
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    ) +
    geom_polygon(data = boards, aes(x, y), fill = '#ffcb05') +
    #annotation_custom(nhl_logo, xmin = -logo_borders$xmin, xmax = logo_borders$xmax, ymin = -logo_borders$ymin, ymax = logo_borders$ymax) +
    geom_polygon(data = center_circle, aes(x, y), fill = '#0033a0') +
    geom_polygon(data = faceoff_circles_l, aes(x, y), fill = '#c8102e') +
    geom_polygon(data = faceoff_circles_r, aes(x, y), fill = '#c8102e') +
    geom_polygon(data = l_l_dot, aes(x, y), fill = '#c8102e') +
    geom_polygon(data = h_l_dot, aes(x, y), fill = '#c8102e') +
    geom_polygon(data = l_nz_l_dot, aes(x, y), fill = '#c8102e') +
    geom_polygon(data = h_nz_l_dot, aes(x, y), fill = '#c8102e') +
    geom_polygon(data = l_nz_r_dot, aes(x, y), fill = '#c8102e') +
    geom_polygon(data = h_nz_r_dot, aes(x, y), fill = '#c8102e') +
    geom_polygon(data = l_r_dot, aes(x, y), fill = '#c8102e') +
    geom_polygon(data = h_r_dot, aes(x, y), fill = '#c8102e') +
    geom_polygon(data = l_l_dot_fill, aes(x, y), fill = '#c8102e') +
    geom_polygon(data = h_l_dot_fill, aes(x, y), fill = '#c8102e') +
    geom_polygon(data = l_nz_l_dot_fill, aes(x, y), fill = '#c8102e') +
    geom_polygon(data = h_nz_l_dot_fill, aes(x, y), fill = '#c8102e') +
    geom_polygon(data = l_nz_r_dot_fill, aes(x, y), fill = '#c8102e') +
    geom_polygon(data = h_nz_r_dot_fill, aes(x, y), fill = '#c8102e') +
    geom_polygon(data = l_r_dot_fill, aes(x, y), fill = '#c8102e') +
    geom_polygon(data = h_r_dot_fill, aes(x, y), fill = '#c8102e') +
    geom_rect(data = faceoff_details, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = '#c8102e') +
    geom_rect(data = red_line, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = '#c8102e') +
    geom_rect(data = red_line_detail, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = '#ffffff') +
    geom_polygon(data = center_dot, aes(x, y), fill = '#0033a0') +
    geom_polygon(data = ref_crease, aes(x, y), fill = '#c8102e') +
    geom_rect(data = blue_line, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = '#0033a0') +
    geom_polygon(data = crease, aes(x, y), fill = '#0088ce') +
    geom_polygon(data = crease_outline, aes(x, y), fill = '#c8102e') +
    geom_polygon(data = crease, aes(-x, y), fill = '#0088ce') +
    geom_polygon(data = crease_outline, aes(-x, y), fill = '#c8102e') +
    geom_polygon(data = left_restricted_area, aes(x, y), fill = '#c8102e') +
    geom_polygon(data = right_restricted_area, aes(x, y), fill = '#c8102e') +
    geom_polygon(data = goal, aes(x, y), fill = '#c8102e') +
    geom_polygon(data = goal, aes(-x, y), fill = '#c8102e') +
    geom_polygon(data = goal_fill, aes(x, y), fill = '#939598') +
    geom_polygon(data = goal_fill, aes(-x, y), fill = '#939598') +
    geom_rect(data = goal_line, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = '#c8102e')
}


# 3 Data Prep ---------------------------------------------------------------


# load data
nwhl <- read.csv("https://github.com/bigdatacup/Big-Data-Cup-2021/raw/main/hackathon_nwhl.csv")
nwhl_positions <- read.csv("https://raw.githubusercontent.com/danmorse314/bdc21/main/nwhl%20stats.csv")[,c(1:4)]
nwhl <- left_join(nwhl, nwhl_positions, by = c("Player" = "name"))

# create seconds remaining in period and event ids
nwhl <- nwhl %>%
  separate(Clock, into = c("Minutes", "Seconds"), sep = ":") %>%
  mutate(Minutes = as.numeric(Minutes),
         Seconds = as.numeric(Seconds),
         seconds_remaining_in_period = Minutes*60 + Seconds,
         event_id = row_number())

# create game IDs
gameIds <- nwhl %>%
  dplyr::select(game_date, Home.Team, Away.Team) %>%
  unique() %>%
  mutate(gameId = c(1:n()))
nwhl <- left_join(nwhl, gameIds, by = c("game_date", "Home.Team", "Away.Team"))



# feature engineering
df_main <- nwhl %>%
  mutate(side = if_else(Home.Team == Team, "Home", "Away"),
         side_prev = lag(side),
         side_prev2 = lag(side, 2),
         time_diff = lag(seconds_remaining_in_period) - seconds_remaining_in_period,
         time_diff = if_else(time_diff < 0, 0, time_diff),
         time_diff2 = lag(seconds_remaining_in_period, 2) - lag(seconds_remaining_in_period),
         time_diff2 = if_else(time_diff2 < 0, 0, time_diff2),
         x_prev = lag(X.Coordinate),
         y_prev = lag(Y.Coordinate),
         prev_event = lag(Event),
         x_prev2 = lag(X.Coordinate, 2),
         y_prev2 = lag(Y.Coordinate, 2),
         prev_event2 = lag(Event, 2),
         #pos_change = if_else(lag(Team) != Team & lag(gameId) == gameId, 1, 0),
         skater_adv = if_else(side == "Home", Home.Team.Skaters - Away.Team.Skaters, Away.Team.Skaters - Home.Team.Skaters),
         differential = if_else(side == "Home", Home.Team.Goals - Away.Team.Goals, Away.Team.Goals - Home.Team.Goals),
         shot_dist = sqrt((X.Coordinate - 189)**2 + (Y.Coordinate - 42.5)**2),
         shot_angle = asin(abs(Y.Coordinate - 42.5)/shot_dist)*180/3.14) %>%
  group_by(gameId, Team, Period) %>%
  mutate(is_shot = if_else(Event == "Shot", 1, 0),
         is_shot = if_else(is.na(is_shot), 0, is_shot),
         is_goal = if_else(Event == "Goal", 1, 0),
         is_goal = if_else(is.na(is_goal), 0, is_goal),
         shots_period_cumsum = cumsum(is_shot),
         Detail.1 = as.factor(Detail.1),
         Detail.2 = as.factor(Detail.2),
         Detail.3 = as.factor(Detail.3),
         Detail.4 = as.factor(Detail.4),
         side = as.factor(side),
         side_prev = as.factor(side_prev),
         side_prev2 = as.factor(side_prev2),
         prev_event = as.factor(prev_event),
         prev_event2 = as.factor(prev_event2),
         is_goal = as.factor(is_goal)) %>%
  mutate(position = case_when(
    Player %in% c("Samantha Davis",
                  "Mary Parker",
                  "Neve Van Pelt",
                  "Taylor Wasylk",
                  "Nicole Guagliardo",
                  "Mackenzie Lancaster",
                  "Maddie Bishop",
                  "Rachael Ade",
                  "Maeve Reilly",
                  "Sarah Hughson",
                  "Madison Packer",
                  "Paige Voight",
                  "Lynn Astrup") ~ "F",
    Player %in% c("Megan Delay",
                  "Taylor Marchin",
                  "Maggie LaGue",
                  "Laurel Hill",
                  "Rebecca Morse",
                  "Kiira Dosdall",
                  "Sammy Kolowrat",
                  "Maddie Rowe",
                  "Winny Brodt-Brown",
                  "Sara Bustad",
                  "Chelsey Brodt-Rosenthal") ~ "D",
    TRUE ~ position
  ))


df_shots <- df_main %>%
  filter(Event %in% c("Shot", "Goal"))



# 4 xG Model  ---------------------------------------------------------------------

# remove unneccesary columns
df_shotsMod <- df_shots %>%
  ungroup() %>%
  dplyr::select(-c(game_date, Minutes, Seconds, Home.Team, Away.Team, Home.Team.Skaters,
                   Home.Team.Goals, Away.Team.Goals, Away.Team.Skaters, Team, Player,
                   Player.2, Event, gameId, X.Coordinate.2, Y.Coordinate.2, is_shot,
                   Detail.2, number, team_city, position)) %>%
  mutate(Detail.1 = as.factor(as.character(Detail.1)),
         Detail.3 = as.factor(as.character(Detail.3)),
         Detail.4 = as.factor(as.character(Detail.4)))

# store event IDs
df_shots_id <- df_shotsMod$event_id

# remove event IDs
df_shotsMod <- df_shotsMod %>%
  dplyr::select(-event_id)

train_ <- sample(1:nrow(df_shotsMod), .7*nrow(df_shotsMod))
train <- df_shotsMod[train_,]
test <- df_shotsMod[-train_, -23]
test_y <- df_shotsMod[-train_, 23]

table(train$is_goal)
table(test_y)

rf <- randomForest(x = train[, -23], y = train$is_goal, ntree = 1000, do.trace = T, importance = T,
                   sampsize = c(50, 50), strata = train$is_goal, cutoff = c(.65, .35))

saveRDS(rf, "rf_fit.rds")


#confusion matrix
rf$confusion

#model accuracy
pred <- predict(rf, test)
pred_comb <- as.data.frame(cbind(pred, test_y)) %>%
  mutate(acc = if_else(pred == test_y$is_goal, 1, 0)) %>%
  group_by(is_goal) %>%
  summarise(correct = sum(acc == 1),
            total = n(),
            acc = sum(acc)/n())
pred_comb


# variable importance
rf_imp <- as.data.frame(importance(rf)[,4])
rf_imp$vars <- rownames(rf_imp)
rf_imp <- rf_imp %>%
  ggplot(., aes(reorder(vars, `importance(rf)[, 4]`), `importance(rf)[, 4]`)) +
  geom_bar(stat = "identity", width = .5) +
  xlab("") +
  ylab("Mean Decrease Gini") +
  labs(title = "Variable Importance for xG Random Forest Model") +
  coord_flip() +
  theme_minimal()

ggsave("rf_imp.png", rf_imp, width = 8, height = 3.5)

# naive prediction
# predicting the mean of goal scored
naive_probs <- as.matrix(data.frame(`0` = rep(.96, length(test_y$is_goal)),
                                    `1` = rep(.04, length(test_y$is_goal))))

pred_prob <- predict(rf, test, type = "prob")

# calculate log-loss
# RF is much MUCH better than naive
LogLoss(naive_probs, as.numeric(as.character(test_y$is_goal)))
LogLoss(pred_prob, as.numeric(as.character(test_y$is_goal)))

# predict on full data
pred_rf_full <- as.data.frame(predict(rf, df_shotsMod, type = "prob")[,2]) %>%
  cbind(df_shotsMod) %>%
  cbind(df_id) %>%
  rename(xG = `predict(rf, df_shotsMod, type = "prob")[, 2]`,
         event_id = df_id) %>%
  dplyr::select(event_id, xG) %>%
  left_join(df_shots, by = "event_id") 

# 5 Shooting Random Effects ------------------------------------------------

# plays where a pass immediately precedes a shot
df_passers <- df_main %>%
  ungroup() %>%
  mutate(passer = if_else(lag(Event) == "Play", lag(Player), "NULL"),
         pass_shot = if_else(lag(Event) == "Play", 1, 0)) %>%
  filter(Event == "Shot") %>%
  dplyr::select(event_id, passer, pass_shot)

# df used for GLMM
df_rand <- pred_rf_full %>%
  left_join(df_passers) %>%
  mutate(pass_shot = as.factor(pass_shot)) %>%
  dplyr::select(event_id, Player, passer, pass_shot, xG, position) %>%
  mutate(position = as.factor(position)) %>%
  fill_by_value(passer, value = "NULL") %>%
  fill_by_value(pass_shot, value = 0) 

# number of shots by each player
n_games <- df_rand %>%
  group_by(Player) %>%
  summarise(n = n()) %>%
  arrange(n)

# GLMM
rand_fit <- lmer(xG ~ (1 | Player) + (1 | passer) + pass_shot, data = df_rand)
summary(rand_fit)

# code from Michael Lopez https://github.com/statsbylopez/NFL_Fumbles to get random effects and confidence intervals
randoms <- ranef(rand_fit, condVar = TRUE)$Player
qq <- attr(ranef(rand_fit, condVar = TRUE)$Player, "postVar") 
rand.interc <- randoms[,1]
#sd.interc=1*sqrt(qq[,,1:length(qq)])
ranefs <- data.frame(Intercepts = randoms[,1],
                     sd.interc = 2*sqrt(qq[,,1:length(qq)]),
                     lev.names = rownames(randoms))

ranefs$lev.names <- factor(ranefs$lev.names, levels = ranefs$lev.names[order(ranefs$Intercepts)])
ranefs <- ranefs[order(ranefs$Intercepts),]

ranefs <- ranefs %>%
  mutate(min = Intercepts - sd.interc,
         max = Intercepts + sd.interc) %>%
  left_join(n_games, by = c("lev.names" = "Player"))

# add positions back in 
shooter_effects <- ranefs %>%
  left_join(nwhl_positions, by = c("lev.names" = "name"))

# mean intercept for forwards
f_mean <- shooter_effects %>%
  filter(position == "F") %>%
  summarise(m = mean(Intercepts))
f_mean <- f_mean$m

# mean intercept for defenders
d_mean <- shooter_effects %>%
  filter(position == "D") %>%
  summarise(m = mean(Intercepts))
d_mean <- d_mean$m


# plot top and bottom 10 shooters
top30_players <- shooter_effects %>%
  filter(n >= 5, position == "F") %>%
  arrange(-Intercepts) %>%
  head(10) %>%
  ggplot(., aes(reorder(lev.names, Intercepts), Intercepts)) + 
  geom_hline(yintercept=0, color = "lightgrey") +
  geom_hline(yintercept=f_mean) +
  geom_errorbar(aes(ymin = min, ymax = max), 
                width = 0, color = "black", alpha = .5) +
  geom_point(pch = 16) + 
  labs(title = "Random Effects of Goal Scoring Ability by Player",
       subtitle = "Top 10 Forwards")+
  guides(size=FALSE,shape=FALSE) +
  theme_bw() + 
  ylim(-.2, .3) +
  xlab(NULL) + 
  ylab("") + 
  theme(axis.text = element_text(face = "bold", size = 6)) +
  coord_flip()

bottom30_players <- shooter_effects %>%
  filter(n >= 5, position == "F") %>%
  arrange(-Intercepts) %>%
  tail(10) %>%
  ggplot(., aes(reorder(lev.names, Intercepts), Intercepts)) + 
  geom_hline(yintercept=0, color = "lightgrey") +
  geom_hline(yintercept=f_mean) +
  geom_errorbar(aes(ymin = min, ymax = max), 
                width = 0, color = "black", alpha = .5) +
  geom_point(pch = 16) + 
  labs(subtitle = "Bottom 10 Forwards",
       caption = paste0("Forwards with 5 or more shots\nBlack line represents mean effect among forwards (", round(f_mean,3), ")")) +
  guides(size = FALSE, shape = FALSE) +
  theme_bw() +
  ylim(-.2, .3) +
  xlab(NULL) + 
  ylab("Random Effect for xG") + 
  theme(axis.text = element_text(face = "bold", size = 6)) +
  coord_flip()

# shooter_effects %>%
#   filter(n >= 5, position == "F") %>%
#   arrange(-Intercepts) %>%
#   ggplot(., aes(reorder(lev.names, Intercepts), Intercepts, color = position)) + 
#   geom_hline(yintercept=f_mean) +
#   
#   geom_errorbar(aes(ymin = min, ymax = max), 
#                 width = 0, color = "black", alpha = .5) +
#   geom_point(pch = 16) + 
#   labs(subtitle = "Bottom 10 Players",
#        caption = "Forwards with 5 or more shots")+
#   guides(size = FALSE, shape = FALSE) +
#   theme_bw() +
#   ylim(-.2, .25) +
#   xlab(NULL) + 
#   ylab("Random Effect for xG") + 
#   theme(axis.text = element_text(face = "bold", size = 6)) +
#   coord_flip()

# patchwork
top_bot_ranefs <- top30_players / bottom30_players

# save
ggsave("top_bot_ranefs.png", top_bot_ranefs, width = 8, height = 3.5)


# tables for write-up
knitr::kable(df_shots %>%
               filter(Event == "Goal") %>%
               group_by(Player) %>%
               summarise(g = n()) %>%
               arrange(-g) %>%
               head(10))
knitr::kable(df_shots %>%
               group_by(Player) %>%
               summarise(s = n()) %>%
               arrange(-s) %>%
               head(10))

# Random Effects for Passers (not used) -----------------------------------------------

# This Section is not used for the paper 
# skip to Section 6

randoms <- ranef(rand_fit, condVar = TRUE)$passer
qq <- attr(ranef(rand_fit, condVar = TRUE)$passer, "postVar") #The [[1]] is the offensive RE
rand.interc <- randoms[,1]

ranefs <- data.frame(Intercepts=randoms[,1],
                     sd.interc=2*sqrt(qq[,,1:length(qq)]),
                     lev.names=rownames(randoms))

ranefs$lev.names <- factor(ranefs$lev.names, levels = ranefs$lev.names[order(ranefs$Intercepts)])
ranefs <- ranefs[order(ranefs$Intercepts),]

ranefs %>%
  mutate(min = Intercepts - sd.interc,
         max = Intercepts + sd.interc) %>%
  arrange(-Intercepts) %>%
  ggplot(., aes(reorder(lev.names, Intercepts), Intercepts)) + 
  geom_hline(yintercept=0) +
  geom_errorbar(aes(ymin = min, ymax = max), 
                width = 0, color = "black", alpha = .5) +
  geom_point(pch = 16) + 
  labs(title = "Random Effects of Goal Scoring Ability by Player",
       subtitle = "Top 10 Players")+
  guides(size=FALSE,shape=FALSE) +
  theme_bw() + 
  ylim(-.2, .25) +
  xlab(NULL) + 
  ylab("") + 
  theme(axis.text = element_text(face = "bold", size = 6)) +
  coord_flip()




df_shots %>%
  filter(Event == "Goal") %>%
  group_by(Player) %>%
  summarise(g = n()) %>%
  arrange(-g)


# as.data.frame(cbind(ranefs$Intercepts, goals_per_player$g, goals_per_player$s)) %>%
#   rename(ranef = V1,
#          goals = V2,
#          shots = V3) %>%
#   ggplot(., aes(shots, goals)) +
#   geom_point() +
#   geom_smooth(method = "lm")
# 
# goals_per_player <- df_shots %>%
#   group_by(Player) %>%
#   summarise(g = sum(Event =="Goal"),
#             s = n())

# 6 Passing Model -----------------------------------------------------------

# get individual shooter effects
df_player_xG <- shooter_effects %>%
  dplyr::select(Intercepts, lev.names)

# add xG
df_passes <- left_join(df_main, dplyr::select(pred_rf_full, event_id, xG))

#add shooter effects
df_passes <- left_join(df_passes, df_player_xG, by = c("Player" = "lev.names"))


# correlation of intercepts to xG
t <- df_passes %>%
  dplyr::select(Intercepts, xG) %>%
  na.omit()

# t <- df_passes %>%
#   mutate(xG_adj = xG - Intercepts) %>%
#   dplyr::select(Intercepts, xG_adj) %>%
#   na.omit()

# R-squared
cor(t$Intercepts, t$xG)**2

# pad data with extra rows for missing game seconds

# nwhl_padded <- df_passes %>%
#   group_by(gameId, Period) %>%
#   pad_int("seconds_remaining_in_period") %>%
#   arrange(gameId, Period, -seconds_remaining_in_period)


# get future and previous xG, time since last pass and shot
nwhl_padded_home <- df_passes %>%
  filter(side == "Home") %>%
  group_by(gameId, Period) %>%
  pad_int("seconds_remaining_in_period") %>%
  arrange(gameId, Period, -seconds_remaining_in_period) %>%
  mutate(xG_adj = xG - Intercepts) %>%
  fill_by_value(xG_adj, 0) %>%
  mutate(xG_prev10s = lag(rollapply(xG_adj, 10, sum, partial = T, align = "right")),
         xG_n10s = rollapply(xG_adj, 10, sum, partial = T, align = "left"),
         is_shot_home = if_else((Event == "Shot" | Event == "Goal"), 1, 0),
         shot_group = cumsum(is_shot_home == 1)) %>%
  group_by(shot_group, gameId, Period) %>%
  mutate(time_since_last_shot = first(seconds_remaining_in_period) - seconds_remaining_in_period) %>%
  dplyr::select(-c(is_shot_home, shot_group)) %>%
  ungroup() %>%
  mutate(is_pass = if_else(Event == "Play", 1, 0),
         pass_group = cumsum(is_pass == 1)) %>%
  group_by(pass_group, gameId, Period) %>%
  mutate(time_since_last_pass = first(seconds_remaining_in_period) - seconds_remaining_in_period) %>%
  ungroup() %>%
  dplyr::select(-c(is_pass, pass_group))

nwhl_padded_away <- df_passes %>%
  filter(side == "Away") %>%
  group_by(gameId, Period) %>%
  pad_int("seconds_remaining_in_period") %>%
  arrange(gameId, Period, -seconds_remaining_in_period) %>%
  mutate(xG_adj = xG - Intercepts) %>%
  fill_by_value(xG_adj, 0) %>%
  mutate(xG_prev10s = lag(rollapply(xG_adj, 10, sum, partial = T, align = "right")),
         xG_n10s = rollapply(xG_adj, 10, sum, partial = T, align = "left"),
         is_shot_home = if_else((Event == "Shot" | Event == "Goal"), 1, 0),
         shot_group = cumsum(is_shot_home == 1)) %>%
  group_by(shot_group, gameId, Period) %>%
  mutate(time_since_last_shot = first(seconds_remaining_in_period) - seconds_remaining_in_period) %>%
  dplyr::select(-c(is_shot_home, shot_group)) %>%
  ungroup() %>%
  mutate(is_pass = if_else(Event == "Play", 1, 0),
         pass_group = cumsum(is_pass == 1)) %>%
  group_by(pass_group, gameId, Period) %>%
  mutate(time_since_last_pass = first(seconds_remaining_in_period) - seconds_remaining_in_period) %>%
  ungroup() %>%
  dplyr::select(-c(is_pass, pass_group))

nwhl_padded <- rbind(nwhl_padded_home, nwhl_padded_away) 

# select only pass plays
df_passes <- nwhl_padded %>%
  filter(Event %in% c("Play", "Incomplete Play"))

# get features
df_passesMod <- df_passes %>%
  ungroup() %>%
  dplyr::select(event_id, seconds_remaining_in_period, Period, X.Coordinate, Y.Coordinate, x_prev, y_prev, x_prev2,
         y_prev2, Detail.1, X.Coordinate.2, Y.Coordinate.2, time_diff, time_diff2, side, side_prev, 
         side_prev2, prev_event, prev_event2, skater_adv, differential, shots_period_cumsum,
         time_since_last_shot, xG_n10s, xG_prev10s) %>%
  mutate(Detail.1 = as.factor(as.character(Detail.1)))

# store ID
df_passes_id <- df_passesMod$event_id

# pull out ID
df_passesMod <- df_passesMod %>%
  dplyr::select(-event_id)

# train/test split
train_ <- sample(1:nrow(df_passesMod), .7*nrow(df_passesMod))
train <- df_passesMod[train_,]
test <- df_passesMod[-train_, -23]
test_y <- df_passesMod[-train_, 23]

# rf
rf_passes <- randomForest(x = train[, -23], y = train$xG_n10s, ntree = 500, do.trace = T,
                          importance = T, mtry = 4)

pred_rf_passes <- predict(rf_passes, test)

# naive prediction is mean xG in next 10 seconds
naive_xG10s <- c(rep(mean(df_passesMod$xG_n10s), length(test_y$xG_n10s)))

RMSE(naive_xG10s, test_y$xG_n10s)
RMSE(pred_rf_passes, test_y$xG_n10s)

saveRDS(rf_passes, "rf_passes_fit")

# variable importance
rf_imp_passes <- as.data.frame(importance(rf_passes))
rf_imp_passes <- cbind(rownames(rf_imp_passes), rf_imp_passes)
rf_imp_passes <- rf_imp_passes %>%
  rename(vars = `rownames(rf_imp_passes)`) %>%
  ggplot(., aes(reorder(vars, IncNodePurity), IncNodePurity)) +
  geom_bar(stat = "identity", width = .5) +
  xlab("") +
  ylab("IncNodePurity") +
  labs(title = "Variable Importance for xG Next 10 Seconds Model") +
  coord_flip() +
  theme_minimal()

ggsave("rf_imp_passes.png", rf_imp_passes, width = 8, height = 3.5)


# predict on full data and join to main data  
pred_rf_passes <- as.data.frame(predict(rf_passes, df_passesMod)) %>%
  cbind(df_passesMod) %>%
  cbind(df_passes_id) %>%
  rename(xG_n10s_pred = `predict(rf_passes, df_passesMod)`,
         event_id = df_passes_id) %>%
  dplyr::select(event_id, xG_n10s_pred) %>%
  left_join(df_passes, by = "event_id") 


# 7 Passing Random Effects --------------------------------------------------

# get N passes
n_passes <- pred_rf_passes %>%
  group_by(Player) %>%
  summarise(n_passes = n())

# random effects
rand_fit <- lmer(xG_n10s_pred ~ (1 | Player) + (1 | Player.2), data = pred_rf_passes)
summary(rand_fit)

# same code as in Section 5
randoms <- ranef(rand_fit, condVar = TRUE)$Player
qq <- attr(ranef(rand_fit, condVar = TRUE)$Player, "postVar")
rand.interc <- randoms[,1]

ranefs <- data.frame(Intercepts=randoms[,1],
                     sd.interc=2*sqrt(qq[,,1:length(qq)]),
                     lev.names=rownames(randoms))

ranefs$lev.names <- factor(ranefs$lev.names, levels = ranefs$lev.names[order(ranefs$Intercepts)])
ranefs <- ranefs[order(ranefs$Intercepts),]

ranefs <- ranefs %>%
  mutate(min = Intercepts - sd.interc,
         max = Intercepts + sd.interc) %>%
  left_join(n_passes, by = c("lev.names" = "Player"))

passer_effects <- ranefs %>%
  left_join(nwhl_positions, by = c("lev.names" = "name"))


top10_passers <- passer_effects %>%
  filter(n_passes >= 20) %>%
  arrange(-Intercepts) %>%
  head(10) %>%
  ggplot(., aes(reorder(lev.names, Intercepts), Intercepts)) + 
  geom_hline(yintercept=0) +
  geom_errorbar(aes(ymin = min, ymax = max), 
                width = 0, color = "black", alpha = .5) +
  geom_point(pch = 16) + 
  labs(title = "Random Effects of Playmaking Ability by Player",
       subtitle = "Top 10 Players")+
  guides(size=FALSE,shape=FALSE) +
  theme_bw() + 
  ylim(-.2, .25) +
  xlab(NULL) + 
  ylab("") + 
  theme(axis.text = element_text(face = "bold", size = 6)) +
  coord_flip()

bottom10_passers <- passer_effects %>%
  filter(n_passes >= 20) %>%
  arrange(-Intercepts) %>%
  tail(10) %>%
  ggplot(., aes(reorder(lev.names, Intercepts), Intercepts)) + 
  geom_hline(yintercept=0) +
  geom_errorbar(aes(ymin = min, ymax = max), 
                width = 0, color = "black", alpha = .5) +
  geom_point(pch = 16) + 
  labs(subtitle = "Bottom 10 Players",
       caption = "Players with 20 or more passes")+
  guides(size = FALSE, shape = FALSE) +
  theme_bw() +
  ylim(-.2, .25) +
  xlab(NULL) + 
  ylab("Random Effect for xG in Next 10 Seconds") + 
  theme(axis.text = element_text(face = "bold", size = 6)) +
  coord_flip()

top_bot_passers <- top10_passers / bottom10_passers

ggsave("top_bot_passers.png", top_bot_passers, width = 8, height = 3.5)



# 8 Buffalo Beauts Lines ----------------------------------------------------

# get plays involving Autumn MacDougall
Autumn_MacDougall <- df_main %>%
  # mutate(Position = case_when(
  #   Player.2 %in%c("Brooke Stacey",
  #                  "Cassidy MacPherson",
  #                  "Erin Ghren",
  #                  "Hunter Accursi",
  #                  "Jordan Juron",
  #                  "Kristin Lewicki",
  #                  "Neve Van Pelt",
  #                  "Taylor Wasylk",
  #                  "Autumn MacDougall") ~ "F",
  #   TRUE ~ "D"
  # )) %>%
  filter(Player %in% c("Autumn MacDougall") |
         Player.2 %in% c("Autumn MacDougall"),
         Event %in% c("Play", "Incomplete Play"))
  
# gets number of passing interactions each player had with Autumn MacDougall
# adds each player's shooter and passer effects
# puts it all into a {gt} table
am_teammates <- rbind(Autumn_MacDougall %>%
  filter(Player != "Autumn MacDougall") %>%
  left_join(dplyr::select(shooter_effects, lev.names, Intercepts), by = c("Player" = "lev.names")) %>%
  left_join(dplyr::select(passer_effects, lev.names, Intercepts), by = c("Player" = "lev.names")) %>%
  rename(shooter_effect = Intercepts.x,
         passer_effect = Intercepts.y) %>%
  group_by(position, Player) %>%
  summarise(n = n(),
            shooter_effect = round(mean(shooter_effect), 2),
            passer_effect = round(mean(passer_effect), 2)) %>%
  arrange(-n) ,
Autumn_MacDougall %>%
  filter(Player.2 != "Autumn MacDougall") %>%
  left_join(dplyr::select(shooter_effects, lev.names, Intercepts), by = c("Player.2" = "lev.names")) %>%
  left_join(dplyr::select(passer_effects, lev.names, Intercepts), by = c("Player.2" = "lev.names")) %>%
  rename(shooter_effect = Intercepts.x,
         passer_effect = Intercepts.y) %>%
  group_by(position, Player.2) %>%
  summarise(n = n(),
            shooter_effect = round(mean(shooter_effect), 2),
            passer_effect = round(mean(passer_effect), 2)) %>%
  arrange(-n) %>%
  rename(Player = Player.2)) %>%
  filter(position == "F") %>%
  group_by(Player) %>%
  summarise(`N` = sum(n),
            `Shooter Effect` = round(mean(shooter_effect), 2),
            `Passer Effect` = round(mean(passer_effect), 2)) %>%
  arrange(-`N`) %>%
  gt() %>%
  tab_header(
    title = md("**Players Who Passed To or From Autumn MacDougall**")
  ) %>% 
  tab_source_note(md("*Only forwards shown*<br>Autumn MacDougall: Shooter Effect = 0.17, Passer Effect = 0.02")) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(everything())
  )  %>% 
  cols_align(align = "center",
             columns = c(2, 3, 4)) %>% 
  data_color(
    columns = vars(`N`),
    colors = scales::col_numeric(
      c("white", "#BECBE0", "#5B9ED9"),
      domain = NULL
    )
  ) %>% 
  data_color(
    columns = vars(`Shooter Effect`),
    colors = scales::col_numeric(
      c("white", "#BECBE0", "#5B9ED9"),
      domain = NULL
    )
  ) %>% 
  data_color(
    columns = vars(`Passer Effect`),
    colors = scales::col_numeric(
      c("white", "#BECBE0", "#5B9ED9"),
      domain = NULL
    )
  )


gtsave(am_teammates, "am_teammates.png")



