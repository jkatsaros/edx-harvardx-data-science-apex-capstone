##########################################################
# edX HarvardX: Data Science - Apex Legends Capstone
# Analysis R Script
# Jason Katsaros
##########################################################

if (!require(jsonlite))
  install.packages("jsonlite", repos = "http://cran.us.r-project.org")
if (!require(tidyverse))
  install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if (!require(caret))
  install.packages("caret", repos = "http://cran.us.r-project.org")
if (!require(ggplot2))
  install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if (!require(ggforce))
  install.packages("ggforce", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if (!require(ggimage))
  install.package("ggimage", repos = "http://cran.us.r-project.org")
if (!require(gganimate))
  install.packages("gganimate", repos = "http://cran.us.r-project.org")
if (!require(gridExtra))
  install.packages("gridExtra", repos = "http://cran.us.r-project.org")

library(jsonlite)
library(tidyverse)
library(caret)
library(ggplot2)
library(ggforce)
library(ggimage)
library(gganimate)
library(gridExtra)

# Helper function to account for any NaN encountered within a data.frame
is.nan.data.frame <- function(df)
  do.call(cbind, lapply(df, is.nan))

# Download the json file to the "data" directory
json_file <- "data/apex.json"
if(!file.exists(json_file))
  download.file(
    paste0(
      "https://raw.githubusercontent.com/",
      "bluelightgit/apex-zone-predict-machine-learning/",
      "main/zone_datas/zones_data.json"
    ),
    json_file
  )
# Download the png file to the "images" directory
worlds_edge_image_file <- "images/worlds_edge_map.png"
if(!file.exists(worlds_edge_image_file))
  download.file(
    paste0(
      "https://static.wikia.nocookie.net/",
      "apexlegends_gamepedia_en/images/",
      "4/4f/World%27s_Edge_MU4.png/",
      "revision/latest/scale-to-width-down/800?cb=20230513185802"
    ),
    worlds_edge_image_file,
    mode = "wb"
  )
# Download the png file to the "images" directory
storm_point_image_file <- "images/storm_point_map.png"
if(!file.exists(storm_point_image_file))
  download.file(
    paste0(
      "https://static.wikia.nocookie.net/",
      "apexlegends_gamepedia_en/images/",
      "5/56/Storm_Point_MU1.png/",
      "revision/latest/scale-to-width-down/800?cb=20220511235629"
    ),
    storm_point_image_file,
    mode = "wb"
  )

# Read and parse the json data
json_data <- fromJSON(json_file)

# Transform the json data into a tibble
apex_data <- as_tibble(json_data)

# Save the apex data to an RData file
save(apex_data, file = "rda/apex_data.rda")

# Copy the apex data to a new variable for manipulation
apex_data_tidy <- apex_data
# Bring the x and y coordinates of the ring out from a nested data frame
# and into their own columns in the data set
apex_data_tidy <- apex_data_tidy %>%
  mutate(x = center$x, y = center$y) %>%
  select(-center)
# Rename "we" to "World's Edge" and "sp" to "Storm Point" respectively
# Make the "map" column values factors
apex_data_tidy <- apex_data_tidy %>%
  mutate(map = as.factor(ifelse(map == "we", "World's Edge", "Storm Point")))
# Make the "stage" and "gameID" column values factors
apex_data_tidy <- apex_data_tidy %>%
  mutate(stage = as.factor(stage), gameID = as.factor(gameID))

# Save the tidy apex data to an RData file
save(apex_data_tidy, file = "rda/apex_data_tidy.rda")

# Copy the tidy apex data to a new variable for further manipulation
apex_data_long <- apex_data_tidy

# Create and populate a new variable with 0
distance_between <- rep(0.0, nrow(apex_data_long))

# For each ring (and the ring to follow),
# if the current ring is not either
# 1. the ring that does not display (ring 0)
# 2. or the last ring of the match (ring 5)
# then calculate the distance between the two ring stages
for (current_ring in 1:nrow(apex_data_long)) {
  if (
    apex_data_long[current_ring,]$stage != 0 |
    apex_data_long[current_ring,]$stage != 5
  ) {
    distance_between[current_ring] <-
      sqrt(
        (
          apex_data_long[current_ring + 1,]$x -
            apex_data_long[current_ring,]$x
        )^2 +
          (
            apex_data_long[current_ring + 1,]$y -
              apex_data_long[current_ring,]$y
          )^2
      )
  }
}

# Transform any NaN into 0
distance_between[is.nan(distance_between)] <- 0

# Add the new information to the apex_data_long data set
apex_data_long <- apex_data_long %>%
  mutate(distance_between = distance_between)

# Create and populate a new variable with 0
angle_between <- rep(0.0, nrow(apex_data_long))

# For each ring (and the ring to follow),
# if the current ring is not either
# 1. the ring that does not display (ring 0)
# 2. or the last ring of the match (ring 5)
# then calculate the angle between the two ring stages
for (current_ring in 1:nrow(apex_data_long)) {
  if (
    apex_data_long[current_ring,]$stage != 0 |
    apex_data_long[current_ring,]$stage != 5
  ) {
    angle_between[current_ring] <-
      atan(
        (
          apex_data_long[current_ring + 1,]$y -
            apex_data_long[current_ring,]$y
        ) /
          (
            apex_data_long[current_ring + 1,]$x -
              apex_data_long[current_ring,]$x
          )
      )
  }
}

# Transform any NaN into 0
angle_between[is.nan(angle_between)] <- 0

# Add the new information to the apex_data_long data set
apex_data_long <- apex_data_long %>%
  mutate(angle_between = angle_between)

# Save the long apex data to an RData file
save(apex_data_long, file = "rda/apex_data_long.rda")

# Copy the tidy apex data to a new variable for further manipulation
apex_data_wide <- apex_data_tidy
# Make a wide representation of the data set
# by making each row represent one whole game
# with all ring stage coordinates represented by their own columns
apex_data_wide <- apex_data_wide %>%
  pivot_wider(
    names_from = stage,
    values_from = c(x, y, radius)
  ) %>%
  mutate(
    distance_from_center_1 =
      sqrt((x_1 - mean(apex_data_wide$x))^2 + (y_1 - mean(apex_data_wide$y))^2),
    distance_between_1_and_2 =
      sqrt((x_2 - x_1)^2 + (y_2 - y_1)^2),
    angle_between_1_and_2 =
      atan((y_2 - y_1) / (x_2 - x_1)),
    distance_from_center_2 =
      sqrt((x_2 - mean(apex_data_wide$x))^2 + (y_2 - mean(apex_data_wide$y))^2),
    distance_between_2_and_3 =
      sqrt((x_3 - x_2)^2 + (y_3 - y_2)^2),
    angle_between_2_and_3 =
      atan((y_3 - y_2) / (x_3 - x_2)),
    distance_from_center_3 =
      sqrt((x_3 - mean(apex_data_wide$x))^2 + (y_3 - mean(apex_data_wide$y))^2),
    distance_between_3_and_4 =
      sqrt((x_4 - x_3)^2 + (y_4 - y_3)^2),
    angle_between_3_and_4 =
      atan((y_4 - y_3) / (x_4 - x_3)),
    distance_from_center_4 =
      sqrt((x_4 - mean(apex_data_wide$x))^2 + (y_4 - mean(apex_data_wide$y))^2),
    distance_between_4_and_5 =
      sqrt((x_5 - x_4)^2 + (y_5 - y_4)^2),
    angle_between_4_and_5 =
      atan((y_5 - y_4) / (x_5 - x_4)),
    distance_from_center_5 =
      sqrt((x_5 - mean(apex_data_wide$x))^2 + (y_5 - mean(apex_data_wide$y))^2),
  )

apex_data_wide[is.nan(apex_data_wide)] <- 0

# Save the wide apex data to an RData file
save(apex_data_wide, file = "rda/apex_data_wide.rda")

# Summarize the data set
apex_data_long %>%
  group_by(map, stage) %>% # Only compare rings within the same map
  summarize(
    x_min = min(apex_data_long$x),
    y_min = min(apex_data_long$y),
    x_max = max(apex_data_long$x),
    y_max = max(apex_data_long$y),
    radius_average = mean(radius),
    radius_standard_deviation = sd(radius),
    radius_min = min(radius),
    radius_max = max(radius),
    area_average = mean(pi * radius^2),
    area_standard_deviation = sd(pi * radius^2),
    area_min = min(pi * radius^2),
    area_max = max(pi * radius^2),
    distance_between_average = mean(distance_between),
    distance_between_standard_deviation = sd(distance_between),
    distance_between_min = min(distance_between),
    distance_between_max = max(distance_between),
    angle_between_average = mean(angle_between),
    angle_between_standard_deviation = sd(angle_between),
    angle_between_min = min(angle_between),
    angle_between_max = max(angle_between)
  )

# Visualize ring movement across all games on World's Edge
apex_data_long %>%
  filter(map == "World's Edge") %>%
  ggplot(aes(x0 = x, y0 = y, r = radius, group = gameID, color = stage)) +
  geom_circle() +
  ggtitle("World's Edge Rings") +
  transition_states(stage)

# Visualize ring movement across each game on World's Edge
apex_data_long %>%
  filter(map == "World's Edge") %>%
  ggplot(aes(x0 = x, y0 = y, r = radius, group = gameID, color = stage)) +
  facet_wrap(~gameID) +
  geom_circle() +
  ggtitle("World's Edge Rings (Per Game)") +
  transition_states(stage)

# Visualize ring movement across all games on Storm Point
apex_data_long %>%
  filter(map == "Storm Point") %>%
  ggplot(aes(x0 = x, y0 = y, r = radius, group = gameID, color = stage)) +
  geom_circle() +
  ggtitle("Storm Point Rings") +
  transition_states(stage, transition_length = 2, state_length = 1)

# Visualize ring movement across each game on Storm Point
apex_data_long %>%
  filter(map == "Storm Point") %>%
  ggplot(aes(x0 = x, y0 = y, r = radius, group = gameID, color = stage)) +
  facet_wrap(~gameID) +
  geom_circle() +
  ggtitle("Storm Point Rings (Per Game)") +
  transition_states(stage)

# Create training and testing partitions of the wide apex data
# These sets are separate from the y train and test data sets
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
# set.seed(1) # if using R 3.5 or earlier
x_test_index <- createDataPartition(apex_data_wide$x_5, times = 1, p = 0.5, list = FALSE)
x_train_set <- apex_data_wide[-x_test_index,]
x_test_set <- apex_data_wide[x_test_index,]

# Create training and testing partitions of the wide apex data
# These sets are separate from the x train and test data sets
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
# set.seed(1) # if using R 3.5 or earlier
y_test_index <- createDataPartition(apex_data_wide$y_5, times = 1, p = 0.5, list = FALSE)
y_train_set <- apex_data_wide[-y_test_index,]
y_test_set <- apex_data_wide[y_test_index,]

# Generalized Linear Model
# Train a model to predict the x coordinate
# of the final stage of the ring in an Apex Legends match
x_model <- train(
  x_5 ~ x_0 + x_1 + x_2 + x_3 + x_4,
  data = x_train_set,
  method = "glm"
)

# Make predictions for x_5 in the x_test_set based on the x_model
x_predictions <- predict(x_model, newdata = x_test_set)

# Since I'm not looking to be exact,
# evaluate the accuracy of the predictions on a sliding scale
glm_x_accuracy_within_250_meters <-
  mean(
    x_predictions <= x_test_set$x_5 + 250 &
      x_predictions >= x_test_set$x_5 - 250
  )
glm_x_accuracy_within_100_meters <-
  mean(
    x_predictions <= x_test_set$x_5 + 100 &
      x_predictions >= x_test_set$x_5 - 100
  )
glm_x_accuracy_within_50_meters <-
  mean(
    x_predictions <= x_test_set$x_5 + 50 &
      x_predictions >= x_test_set$x_5 - 50
  )
glm_x_accuracy_within_25_meters <-
  mean(
    x_predictions <= x_test_set$x_5 + 25 &
      x_predictions >= x_test_set$x_5 - 25
  )

# Train a model to predict the y coordinate
# of the final stage of the ring in an Apex Legends match
y_model <- train(
  y_5 ~ y_0 + y_1 + y_2 + y_3 + y_4,
  data = y_train_set,
  method = "glm"
)

# Make predictions for y_5 in the y_test_set based on the y_model
y_predictions <- predict(y_model, newdata = y_test_set)

# Since I'm not looking to be exact, evaluate the accuracy of the predictions on a sliding scale
glm_y_accuracy_within_250_meters <-
  mean(
    y_predictions <= y_test_set$y_5 + 250 &
      y_predictions >= y_test_set$y_5 - 250
  )
glm_y_accuracy_within_100_meters <-
  mean(
    y_predictions <= y_test_set$y_5 + 100 &
      y_predictions >= y_test_set$y_5 - 100
  )
glm_y_accuracy_within_50_meters <-
  mean(
    y_predictions <= y_test_set$y_5 + 50 &
      y_predictions >= y_test_set$y_5 - 50
  )
glm_y_accuracy_within_25_meters <-
  mean(
    y_predictions <= y_test_set$y_5 + 25 &
      y_predictions >= y_test_set$y_5 - 25
  )

# Print and save the accuracy results to a results variable
accuracy_results <- data.frame(
  method = "glm",
  x_250 = glm_x_accuracy_within_250_meters,
  x_100 = glm_x_accuracy_within_100_meters,
  x_50 = glm_x_accuracy_within_50_meters,
  x_25 = glm_x_accuracy_within_25_meters,
  y_250 = glm_y_accuracy_within_250_meters,
  y_100 = glm_y_accuracy_within_100_meters,
  y_50 = glm_y_accuracy_within_50_meters,
  y_25 = glm_y_accuracy_within_25_meters
)
accuracy_results %>% knitr::kable()

# Save the results to a RData file
save(accuracy_results, file = "rda/accuracy_results.rda")

rmse_results <- data.frame(
  method = "glm",
  x_RMSE = x_model$results$RMSE,
  y_RMSE = y_model$results$RMSE
)
rmse_results %>% knitr::kable()

# Save the results to a RData file
save(rmse_results, file = "rda/rmse_results.rda")

# Plot the fitted against the residuals of the x_model
# to see if there are any trends in the variance
# This is a test to see if there are any problems with the x_model
x_model$finalModel %>%
  ggplot(aes(.fitted, .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  xlab("Final Model Fitted Values") +
  ylab("Final Model Residuals") +
  ggtitle("Fitted Values versus Residuals of X in GLM Model")
# Plot the fitted against the residuals of the y_model
# to see if there are any trends in the variance
# This is a test to see if there are any problems with the y_model
y_model$finalModel %>%
  ggplot(aes(.fitted, .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  xlab("Final Model Fitted Values") +
  ylab("Final Model Residuals") +
  ggtitle("Fitted Values versus Residuals of Y in GLM Model")

# Generalized Linear Model With Stepwise Feature Selection
# Train a model to predict the x coordinate
# of the final stage of the ring in an Apex Legends match
x_model <- train(
  x_5 ~ x_0 + x_1 + x_2 + x_3 + x_4,
  data = x_train_set,
  method = "glmStepAIC",
  trace = 0 # Suppress verbose output
)

# Make predictions for x_5 in the x_test_set based on the x_model
x_predictions <- predict(x_model, newdata = x_test_set)

# Since I'm not looking to be exact,
# evaluate the accuracy of the predictions on a sliding scale
glmStepAIC_x_accuracy_within_250_meters <-
  mean(
    x_predictions <= x_test_set$x_5 + 250 &
      x_predictions >= x_test_set$x_5 - 250
  )
glmStepAIC_x_accuracy_within_100_meters <-
  mean(
    x_predictions <= x_test_set$x_5 + 100 &
      x_predictions >= x_test_set$x_5 - 100
  )
glmStepAIC_x_accuracy_within_50_meters <-
  mean(
    x_predictions <= x_test_set$x_5 + 50 &
      x_predictions >= x_test_set$x_5 - 50
  )
glmStepAIC_x_accuracy_within_25_meters <-
  mean(
    x_predictions <= x_test_set$x_5 + 25 &
      x_predictions >= x_test_set$x_5 - 25
  )

# Train a model to predict the y coordinate
# of the final stage of the ring in an Apex Legends match
y_model <- train(
  y_5 ~ y_0 + y_1 + y_2 + y_3 + y_4,
  data = y_train_set,
  method = "glmStepAIC",
  trace = 0 # Suppress verbose output
)

# Make predictions for y_5 in the y_test_set based on the y_model
y_predictions <- predict(y_model, newdata = y_test_set)

# Since I'm not looking to be exact,
# evaluate the accuracy of the predictions on a sliding scale
glmStepAIC_y_accuracy_within_250_meters <-
  mean(
    y_predictions <= y_test_set$y_5 + 250 &
      y_predictions >= y_test_set$y_5 - 250
  )
glmStepAIC_y_accuracy_within_100_meters <-
  mean(
    y_predictions <= y_test_set$y_5 + 100 &
      y_predictions >= y_test_set$y_5 - 100
  )
glmStepAIC_y_accuracy_within_50_meters <-
  mean(
    y_predictions <= y_test_set$y_5 + 50 &
      y_predictions >= y_test_set$y_5 - 50
  )
glmStepAIC_y_accuracy_within_25_meters <-
  mean(
    y_predictions <= y_test_set$y_5 + 25 &
      y_predictions >= y_test_set$y_5 - 25
  )

# Print and save the accuracy results to a results variable
accuracy_results <- bind_rows(
  accuracy_results,
  data.frame(
    method = "glmStepAIC",
    x_250 = glmStepAIC_x_accuracy_within_250_meters,
    x_100 = glmStepAIC_x_accuracy_within_100_meters,
    x_50 = glmStepAIC_x_accuracy_within_50_meters,
    x_25 = glmStepAIC_x_accuracy_within_25_meters,
    y_250 = glmStepAIC_y_accuracy_within_250_meters,
    y_100 = glmStepAIC_y_accuracy_within_100_meters,
    y_50 = glmStepAIC_y_accuracy_within_50_meters,
    y_25 = glmStepAIC_y_accuracy_within_25_meters
  )
)
accuracy_results %>% knitr::kable()

# Save the results to a RData file
save(accuracy_results, file = "rda/accuracy_results.rda")

rmse_results <- bind_rows(
  rmse_results,
  data.frame(
    method = "glmStepAIC",
    x_RMSE = x_model$results$RMSE,
    y_RMSE = y_model$results$RMSE
  )
)
rmse_results %>% knitr::kable()

# Save the results to a RData file
save(rmse_results, file = "rda/rmse_results.rda")

# Plot the fitted against the residuals of the x_model
# to see if there are any trends in the variance
# This is a test to see if there are any problems with the x_model
x_model$finalModel %>%
  ggplot(aes(.fitted, .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  xlab("Final Model Fitted Values") +
  ylab("Final Model Residuals") +
  ggtitle("Fitted Values versus Residuals of X in GLMSTEPAIC Model")
# Plot the fitted against the residuals of the y_model
# to see if there are any trends in the variance
# This is a test to see if there are any problems with the y_model
y_model$finalModel %>%
  ggplot(aes(.fitted, .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  xlab("Final Model Fitted Values") +
  ylab("Final Model Residuals") +
  ggtitle("Fitted Values versus Residuals of Y in GLMSTEPAIC Model")

# Cubist Regression
# Train a model to predict the x coordinate
# of the final stage of the ring in an Apex Legends match
x_model <- train(
  x_5 ~ x_0 + x_1 + x_2 + x_3 + x_4,
  data = x_train_set,
  method = "cubist"
)

# Make predictions for x_5 in the x_test_set based on the x_model
x_predictions <- predict(x_model, newdata = x_test_set)

# Since I'm not looking to be exact,
# evaluate the accuracy of the predictions on a sliding scale
cubist_x_accuracy_within_250_meters <-
  mean(
    x_predictions <= x_test_set$x_5 + 250 &
      x_predictions >= x_test_set$x_5 - 250
  )
cubist_x_accuracy_within_100_meters <-
  mean(
    x_predictions <= x_test_set$x_5 + 100 &
      x_predictions >= x_test_set$x_5 - 100
  )
cubist_x_accuracy_within_50_meters <-
  mean(
    x_predictions <= x_test_set$x_5 + 50 &
      x_predictions >= x_test_set$x_5 - 50
  )
cubist_x_accuracy_within_25_meters <-
  mean(
    x_predictions <= x_test_set$x_5 + 25 &
      x_predictions >= x_test_set$x_5 - 25
  )

# Train a model to predict the y coordinate
# of the final stage of the ring in an Apex Legends match
y_model <- train(
  y_5 ~ y_0 + y_1 + y_2 + y_3 + y_4,
  data = y_train_set,
  method = "cubist"
)

# Make predictions for y_5 in the y_test_set based on the y_model
y_predictions <- predict(y_model, newdata = y_test_set)

# Since I'm not looking to be exact,
# evaluate the accuracy of the predictions on a sliding scale
cubist_y_accuracy_within_250_meters <-
  mean(
    y_predictions <= y_test_set$y_5 + 250 &
      y_predictions >= y_test_set$y_5 - 250
  )
cubist_y_accuracy_within_100_meters <-
  mean(
    y_predictions <= y_test_set$y_5 + 100 &
      y_predictions >= y_test_set$y_5 - 100
  )
cubist_y_accuracy_within_50_meters <-
  mean(
    y_predictions <= y_test_set$y_5 + 50 &
      y_predictions >= y_test_set$y_5 - 50
  )
cubist_y_accuracy_within_25_meters <-
  mean(
    y_predictions <= y_test_set$y_5 + 25 &
      y_predictions >= y_test_set$y_5 - 25
  )

# Print and save the accuracy results to a results variable
accuracy_results <- bind_rows(
  accuracy_results,
  data.frame(
    method = "cubist",
    x_250 = cubist_x_accuracy_within_250_meters,
    x_100 = cubist_x_accuracy_within_100_meters,
    x_50 = cubist_x_accuracy_within_50_meters,
    x_25 = cubist_x_accuracy_within_25_meters,
    y_250 = cubist_y_accuracy_within_250_meters,
    y_100 = cubist_y_accuracy_within_100_meters,
    y_50 = cubist_y_accuracy_within_50_meters,
    y_25 = cubist_y_accuracy_within_25_meters
  )
)
accuracy_results %>% knitr::kable()

# Save the results to a RData file
save(accuracy_results, file = "rda/accuracy_results.rda")

rmse_results <- bind_rows(
  rmse_results,
  data.frame(
    method = "cubist",
    x_RMSE = x_model$results$RMSE,
    y_RMSE = y_model$results$RMSE
  )
)
rmse_results %>% knitr::kable()

# Save the results to a RData file
save(rmse_results, file = "rda/rmse_results.rda")

# Plot the fitted against the residuals of the x_model
# to see if there are any trends in the variance
# This is a test to see if there are any problems with the x_model
# Since "cubist" comes from a different library
# I have to create my own data frame for plotting
data.frame(
  fitted = x_predictions,
  resid = x_predictions - x_test_set$x_5
) %>%
  ggplot(aes(fitted, resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  xlab("Final Model Fitted Values") +
  ylab("Final Model Residuals") +
  ggtitle("Fitted Values versus Residuals of X in CUBIST Model")
# Plot the fitted against the residuals of the y_model
# to see if there are any trends in the variance
# This is a test to see if there are any problems with the y_model
# Since "cubist" comes from a different library
# I have to create my own data frame for plotting
data.frame(
  fitted = y_predictions,
  resid = y_predictions - y_test_set$y_5
) %>%
  ggplot(aes(fitted, resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  xlab("Final Model Fitted Values") +
  ylab("Final Model Residuals") +
  ggtitle("Fitted Values versus Residuals of Y in CUBIST Model")