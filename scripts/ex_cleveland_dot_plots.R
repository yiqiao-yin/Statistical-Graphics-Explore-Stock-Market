# Read Me:
# This script is a walk through of using Cleveland plot.

# Source:
# https://uc-r.github.io/cleveland-dot-plots

# Install
devtools::install_github("hadley/ggplot2")

# Library
library(readxl)         # for reading in Excel data
library(dplyr)          # for data manipulation
library(tidyr)          # for data shaping
library(ggplot2)        # for generating the visualizations
library(gridExtra)      # for arrange grid plots

# Get data
path <- "C:/Users/JARVIS/OneDrive/STATS GR5293 - Statistical Graphics/4. Final Project/data"
supermarket <- read_excel(paste0(path,"/Supermarket Transactions.xlsx"), sheet = "Data")
head(supermarket)

# Basic Dot Plot
city_rev <- supermarket %>%
  group_by(City) %>%
  summarise(Revenue = sum(Revenue, na.rm = TRUE)) %>%
  arrange(Revenue) %>%
  mutate(City = factor(City, levels = .$City))
ggplot(city_rev, aes(City, Revenue)) +
  geom_bar(stat = "identity") +
  coord_flip()
ggplot(city_rev, aes(Revenue, City)) +
  geom_point()

# Comparing Multiple Points of Information
city_gender_rev <- supermarket %>%
  group_by(City, Gender) %>%
  summarise(Revenue = sum(Revenue, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(City = factor(City, levels = city_rev$City))
head(city_gender_rev)
P1 <- ggplot(city_gender_rev, aes(City, Revenue, fill = Gender)) +
  geom_bar(stat = "identity") +
  coord_flip()
P2 <- ggplot(city_gender_rev, aes(City, Revenue, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip()
P3 <- ggplot(city_gender_rev, aes(City, Revenue, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  facet_wrap(~ Gender)
grid.arrange(P1,P2,P3, nrow=1)
ggplot(city_gender_rev, aes(Revenue, City)) +
  geom_point(aes(color = Gender))
ggplot(city_gender_rev, aes(Revenue, City)) +
  geom_line(aes(group = City)) +
  geom_point(aes(color = Gender))

