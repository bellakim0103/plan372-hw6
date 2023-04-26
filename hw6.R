library(tidyverse)
library(dplyr)

data = read_csv("RDS-2016-0005/Data/TS3_Raw_tree_data.csv")

View(data)

# QUESTION 1.
# Use a regular expression to create separate columns for the city name and state abbreviation.

#  [:alpha:] - all alphabetic characters (A-Z, a-z, and locale-specific letters)
#  [:lower:] - lowercase letters
#  [:upper:] - uppercase letters
#  [:digit:] - digits 0-9
#  [:space:] - whitespace (spaces, tabs, etc.)
#  [:alnum:] - Alphabetic and numeric characters
#  [:punct:] - Punctuation

all(str_detect(data$City, "[:alnum:]+[:punct:] [:alnum:]+$"))
str_match(data$City, "([:alnum:]+)([:punct:]) ([:alnum:]+)$")

data[,c("City", "comma", "State")] = str_match(data$City, "([:alnum:]+)([:punct:]) ([:alnum:]+)$")[,2:4]
data = subset(data, select = -c(comma))
data

# How many records are there in each state?
table(data$State)


# QUESTION 2.
# Filter the dataset to only North Carolina and South Carolina.
data = filter(data, State %in% c("NC","SC"))

# What cities did they collect data from in North and South Carolina?
unique(data$City)

# QUESTION 3. 
# Write a regular expression to extract the genus.
all(str_detect(data$ScientificName, "[:alpha:]+ [:alpha:]+"))
str_match(data$ScientificName, "([:alpha:]+) ([:alpha:]+)")

data[,c("Genus")] = str_match(data$ScientificName, "([:alpha:]+) ([:alpha:]+)")[,2]
data

# What genus of trees has the largest crown diameter in North and South Carolina?
Crown_diameter = group_by(data, Genus) %>%
  summarize(mean_diameter=mean(`AvgCdia (m)`))

# EXTRA CREDIT
# Are there differences in the average age of the different genera of trees in the dataset?
Crown_diameter = group_by(data, Genus) %>%
  summarize(mean_diameter=mean(`AvgCdia (m)`), mean_age=mean(Age))

ggplot(Crown_diameter, aes(x=mean_diameter, y=mean_age, group=1)) +
  geom_point() +
  geom_line()

# Recommend a genera that produces a large crown quickly.
fast_growth = filter(data, Genus %in% c("Betula", "Carya", "Acer", "Platanus", "Quercus"))

ggplot(fast_growth, aes(x=Age, y=`AvgCdia (m)`, group=Genus, color=Genus)) +
  geom_line() +
  xlim(0,60) +
  ylim(0,40)

platanus = filter(data, Genus %in% c("Platanus"))
ggplot(platanus, aes(x=Age, y=`AvgCdia (m)`, group=1)) +
  geom_line() +
  xlim(0,60) +
  ylim(0,40)

acer = filter(data, Genus %in% c("Acer"))
ggplot(acer, aes(x=Age, y=`AvgCdia (m)`, group=1)) +
  geom_line() +
  xlim(0,60) +
  ylim(0,40)

betula = filter(data, Genus %in% c("Betula"))
ggplot(betula, aes(x=Age, y=`AvgCdia (m)`, group=1)) +
  geom_line() +
  xlim(0,60) +
  ylim(0,40)

carya = filter(data, Genus %in% c("Carya"))
ggplot(carya, aes(x=Age, y=`AvgCdia (m)`, group=1)) +
  geom_line() +
  xlim(0,60) +
  ylim(0,40)

quercus = filter(data, Genus %in% c("Quercus"))
ggplot(quercus, aes(x=Age, y=`AvgCdia (m)`, group=1)) +
  geom_line() +
  xlim(0,60) +
  ylim(0,40)


# Refine your regular expression to also extract the species as well, as a separate column. 
all(str_detect(data$ScientificName, "[:alpha:]+ [:alpha:]+"))
str_match(data$ScientificName, "([:alpha:]+) ([:alpha:]+)")

data[,c("Species")] = str_match(data$ScientificName, "([:alpha:]+) ([:alpha:]+)")[,3]

grouped = group_by(data, Genus) %>%
  summarize(distinct_species = n_distinct(Species))
