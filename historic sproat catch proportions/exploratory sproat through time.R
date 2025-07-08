#Exploring the timing of Sproat:

#1) Load Libraries

library(ggplot2)
library(dplyr)


#2) Load the data: (note that this came from soxsum2025)
file_path <- "C:/Users/HAMILTONMI/Desktop/sproat_timing.csv"
data <- read.csv(file_path)
#look at it to make sure it looks correct:
head(data)

#3) Create the plots:
#Using the %Sproat from the test fishery (outside and inside average):

overall_avg <- data %>%
  group_by(Statweek) %>%
  summarise(overall_avg = mean(Average, na.rm = TRUE))


#Normalize the year range to be able to make older years more transparent:
year_range <- range(data$Year)
data <- data %>%
  mutate(Year_norm = (Year - year_range[1]) / diff(year_range))

#Plot the points: (2019 - 2025)
ggplot(data, aes(x = Statweek, y = Average, color = factor(Year), group = Year, alpha = Year_norm)) +
  # Make them all lines with a thickness of 1 (so we can see them easier)
  geom_line(linewidth = 1) +
  
  geom_line(data = overall_avg, aes(x = Statweek, y = overall_avg), 
            color = "black", linewidth = 1.2, inherit.aes = FALSE) +
  
  # control transparency range and hide the legend:
  scale_alpha(range = c(0.3, 1), guide = "none") +  
  labs(x = "Statweek", y = "Average", color = "Year",
       title = "Average by Statweek and Year with Overall Average") +
  theme_minimal()
