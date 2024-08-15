
# List of required packages
required_packages <- c("tidyverse", "naniar", "imputeTS", "forecast", 
                       "patchwork", "randomForest")

# Function to check and install missing packages
install_missing_packages <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new_packages)) install.packages(new_packages, dependencies = TRUE)
}

# Install any missing packages
install_missing_packages(required_packages)

# Load the packages
lapply(required_packages, require, character.only = TRUE)


# Load data and set seed  -------------------------------------------------
pm_per_station <- read.csv("pm_per_station.csv")
district_data <- read.csv("district_data.csv")
set.seed(42)
dir.create("plots")

# Preprocessing -----------------------------------------------------------
## save coordinates
coordinates <- pm_per_station %>%
  dplyr::select(stationID, Xcoor, Ycoor) %>%
  unique()

## long to wide format
pm_per_station_wide <- pm_per_station %>%
  dplyr::select(date, stationID, PM2.5) %>%
  pivot_wider(names_from = stationID,
              values_from = PM2.5,
              names_prefix = "Station_")

## Sorting columns by station number
stations <- names(pm_per_station_wide)
stations <- grep("Station", stations, value = TRUE) 

extract_number <- function(station_name) {
  as.numeric(gsub("Station_", "", station_name))
}

station_numbers <- sapply(stations, extract_number)
station_order <- stations[order(station_numbers)]

ordered_names <- c("date", station_order)

pm_per_station_wide <- pm_per_station_wide %>%
  dplyr::select(all_of(ordered_names))

## data with stations only
pm_per_station_wide_nodate <- pm_per_station_wide %>% dplyr::select(-"date")

# Overview Missings -------------------------------------------------------
org_names <- colnames(pm_per_station_wide_nodate)

colnames(pm_per_station_wide_nodate) <-
  gsub("[^0-9]", "", colnames(pm_per_station_wide_nodate))

# Generate the missing value plot without changing column names
vis_miss_plot <- vis_miss(pm_per_station_wide_nodate)

# Extract the data used in the vis_miss plot
plot_data <- ggplot_build(vis_miss_plot)$data[[1]]

# Calculate the percentages of missing values per station
missing_percentages <-
  prop.table(table(plot_data$variable, is.na(plot_data$value)), 1) * 100

# Extract just the column names (station numbers) from your data
station_names <- colnames(pm_per_station_wide_nodate)

# Update the plot with manual control over the x-axis labels
vis_miss_plot +
  labs(x = "AQM-Station ID", y = "Day Index") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 18),
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 20),
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 20),
    plot.title = element_text(size = 26)
  ) +
  scale_x_discrete(labels = station_names)

# Save the updated plot
ggsave("plots/missingvalues.png",
       width = 10,
       height = 10)

colnames(pm_per_station_wide_nodate) <- 
  org_names

# Visualization Linear Interpolation Station 1  ---------------------------

data <-
  data.frame(
    index = 1:length(pm_per_station_wide$Station_1),
    Station_1 = pm_per_station_wide$Station_1
  )
data$Station1_interp = zoo::na.approx(data$Station_1, na.rm = FALSE)
data$Station1_interp = ifelse(is.na(data$Station_1), data$Station1_interp, NA)

ggplot(data, aes(x = index)) +
  geom_line(aes(y = Station_1, color = "Given PM2.5"), size = 1, 
            show.legend = FALSE) + 
  geom_line(aes(y = Station1_interp, color = "Interpolated PM2.5"), size = 1, 
            show.legend = FALSE) +  
  geom_point(aes(y = Station_1, color = "Given PM2.5"), size = 0, 
             show.legend = TRUE) +  
  geom_point(aes(y = Station1_interp, color = "Interpolated PM2.5"), size = 0, 
             show.legend = TRUE) +  
  labs(
    x = "Day Index",
    y = "PM2.5 Values",
    color = "Origin"  
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 18), 
    axis.text.y = element_text(size = 18),
    axis.title = element_text(size = 20),
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 20),
    plot.title = element_text(size = 20),
    legend.position = "bottom" 
  ) +
  scale_color_manual(values = c("Given PM2.5" = "black", 
                                "Interpolated PM2.5" = "blue")) +
  guides(color = guide_legend(override.aes = list(shape = 15, size = 6, 
                                                  fill = "black"))) +
  ylim(0, 100)

ggsave("plots/LinImp_Stat1.png", width = 10, height = 10)

# RF ----------------------------------------------------------------------
## simple train-test split -----------------------------------------------
train_size <- 0.8
rmse_values <- numeric(ncol(pm_per_station_wide_nodate))
names(rmse_values) <- names(pm_per_station_wide_nodate)
rmse_values_mean <- numeric(ncol(pm_per_station_wide_nodate))

# Step 1: linear interpolation
imp_int_full <- na_interpolation(pm_per_station_wide)

# Step 2: RF
for (i in 1:ncol(pm_per_station_wide_nodate)){
  
  NA_indices <- (is.na(pm_per_station_wide_nodate[,i])) 
  Stat_full <- imp_int_full[!NA_indices,]
  
  # train-test split of full cases of respective column
  current_var <- names(pm_per_station_wide_nodate)[i]
  train_index <- sample(1:nrow(Stat_full), train_size * nrow(Stat_full))
  train_data <- Stat_full[train_index, ]
  test_data <- Stat_full[-train_index, ]
  
  # mean value imputation and calculate RMSE
  mean_value <- colMeans(train_data[, current_var], na.rm = TRUE)
  rmse <- sqrt(colMeans((mean_value - test_data[,current_var])^2))
  rmse_values_mean[i] <- rmse
  
  # fit random forest on train data
  formula_flex <- as.formula(paste(current_var, "~ ."))
  rf_model <- randomForest(formula_flex, data = train_data)
  
  # predict on test data and calculate rmse
  predictions_test <- predict(rf_model, newdata = test_data)
  rmse <- sqrt(colMeans((predictions_test - test_data[,current_var])^2)) 
  rmse_values[i] <- rmse
  
  # predict originally missing values in the respective column and impute
  predictions_na <- predict(rf_model, newdata = imp_int_full[NA_indices,])
  imp_int_full[NA_indices, current_var] <- predictions_na 
  }

rmse_df_rf <- data.frame(RMSE = rmse_values)
colMeans(rmse_df_rf)

rmse_df_mean <- data.frame(RMSE = rmse_values_mean)
colMeans(rmse_df_mean)

## final training on full data ---------------------------------------------
# Step 1: linear interpolation
imp_int_full <- na_interpolation(pm_per_station_wide)

# Initialize a list to store importance tables (variable importance)
importance_list <- list()

# Step 2: RF
for (i in 1:ncol(pm_per_station_wide_nodate)) {
  NA_indices <- (is.na(pm_per_station_wide_nodate[, i]))
  Stat_full <- imp_int_full[!NA_indices, ]
  
  # train rf
  current_var <- names(pm_per_station_wide_nodate)[i]
  formula_flex <- as.formula(paste(current_var, "~ ."))
  rf_model <-
    randomForest(formula_flex, data = Stat_full, importance = TRUE)
  
  # predict originally missing values in the respective column and impute
  predictions_na <-
    predict(rf_model, newdata = imp_int_full[NA_indices, ])
  
  # RF-imputed data is used for the respective column
  imp_int_full[NA_indices, current_var] <- predictions_na
  
  # Save the importance table in the list, ordered by %IncMSE
  importance_list[[current_var]] <-
    rf_model$importance[order(-rf_model$importance[, "%IncMSE"]), ]
  
  # Plot in ggplot
  data <-
    data.frame(index = 1:nrow(imp_int_full),
               current_var = imp_int_full[, current_var])
  data$interp <- data[, current_var]
  data[!NA_indices, "interp"] <- NA
  data[NA_indices, current_var] <- NA
  
  p <- ggplot(data, aes(x = index)) +
    geom_line(aes(y = data[, current_var], color = "Given PM2.5"), size = 1, 
              show.legend = FALSE) +
    geom_line(aes(y = data[, "interp"], color = "RF Imputed PM2.5"), size = 1,
              show.legend = FALSE) +
    geom_point(aes(y = data[, current_var], color = "Given PM2.5"), size = 0, 
              show.legend = TRUE) +
    geom_point(aes(y = data[, "interp"], color = "RF Imputed PM2.5"), size = 0,
              show.legend = TRUE) +
    labs(x = "Day Index", y = "PM 2.5 Values", color = "Origin") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 18),
      axis.text.y = element_text(size = 18),
      axis.title = element_text(size = 20),
      legend.text = element_text(size = 18),
      legend.title = element_text(size = 20),
      plot.title = element_text(size = 20),
      legend.position = "bottom"
    ) +
    scale_color_manual(values = c("Given PM2.5" = "black",
                                  "RF Imputed PM2.5" = "blue")) +
    guides(color = guide_legend(override.aes = list(shape = 15, size = 6, 
                                                    fill = "black")))+
    coord_cartesian(ylim = c(0, 200))
  
  print(p)
  
  ggsave(
    paste0("plots/RF_Stat", current_var, ".png"),
    p,
    width = 10,
    height = 10
  )
}

vis_miss(imp_int_full)
importance_list[[1]]

# Postprocessing ----------------------------------------------------------
# change to long format and add coordinates
imp_int_full_long <- imp_int_full %>%
  pivot_longer(
  cols = starts_with("Station"), 
  names_to = "stationID",
  values_to = "PM2.5", 
  names_prefix = "Station_"
) %>%
  mutate(stationID = as.numeric(stationID))

imp_int_full_long <- left_join(imp_int_full_long, coordinates, by = "stationID")

# save imputed data
write.csv(imp_int_full_long, file = "pm_per_station_imputed.csv",
          row.names = FALSE)




