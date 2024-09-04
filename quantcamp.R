# Intro to R quant camp
# Some first commands

# basic math calculations

# Note: to execute the code you can highlight the line and click run
# you can also just have your cursor on the line and click run
# if you're on a Mac, you can also use keyboard shortcut command+return

8+90
8-90
80/10 #divide
80*10 # multiply


# assign some values to objects

x <- 90
# or 
x = 90

x

age <- 37

this_decimal <- 8.7687745
pi

## Different variable types

# numeric 
number_of_people_in_room <- 35

# logical
is_it_friday <- FALSE # capitals 

# character

this_department <- "Sociology"

# characters are case sensitive

this_department_2 <- "sociology"

# check to see whether this_department and this_department_2 are equal

this_department==this_department_2

# factors 

my_drink <- as.factor("coffee")
my_drink

# check the variable type
# use functions is.XX()

# for eg check whether is_it_friday is a logical
is.logical(is_it_friday)

# is age numeric
is.numeric(age)

# is department numeric
is.numeric(this_department)


# install package

install.packages("tidyverse")

# load in the package
library(tidyverse)


# different types of objects in R

# vectors: group of values of the same type

ages <- c(89, 12, 3, 98, 78, 45, 34)
ages

# check the length
length(ages)

# tibbles

my_data <- tibble(name = c("Monica", "Hugo", "Edward", "Rohan"),
                  age = c(37, 3, 5, 38))

my_data


## functions

mean(ages)
min(ages)
median(age)


# check the dimensions of a dataset

dim(my_data)

# paste

names <- c("Monica", "Rohan")
our_ages <- c(37, 38)

paste(names, our_ages)
paste(names, our_ages, sep = ", ")

### read in the shelter data

shelter_data <- read_csv(file = "shelter.csv")

colnames(shelter_data)

# select some columns

select(shelter_data, 
       occupancy_date, 
       organization_name, 
       occupancy_rate_beds, 
       occupancy_rate_rooms)


shelter_data |>
  select(occupancy_date, organization_name, occupancy_rate_beds, occupancy_rate_rooms)

sdr <- shelter_data |>
  select(occupancy_date, organization_name, occupancy_rate_beds, occupancy_rate_rooms)

# arrange the data by bed occupancy rate

sdr |> 
  arrange(occupancy_rate_beds)

# mutate: making a new column

sdr2 <- sdr |> 
  mutate(is_occupancy_lt_50 = occupancy_rate_beds<50)

sdr |> 
  mutate(column_of_3 = 3)


# filter out all the missing values in bed occupancy

sdr |> 
  filter(!is.na(occupancy_rate_beds)) # ! = NOT

# summarize

sdr |> 
  summarize(mean_bed_occupance = mean(occupancy_rate_beds, na.rm = TRUE))

# or equivalently

sdr |> 
  filter(!is.na(occupancy_rate_beds)) |> 
  summarize(mean_occ = mean(occupancy_rate_beds))

# filter just to look at "City of Toronto" shelters

sdr |> 
  filter(organization_name == "City of Toronto")

# group_by

ave_rate_day <- sdr |> 
  group_by(occupancy_date) |> 
  summarise(mean_bed_occupancy = mean(occupancy_rate_beds, na.rm = TRUE))


## plotting


ave_rate_day

ggplot(data = ave_rate_day, aes(occupancy_date, mean_bed_occupancy)) +
  geom_line(color = "#15149c") + # adds a line
  labs(title = "Average bed occupancy rate by day, 2024",
       x = "Day",
       y = "Rate (%)")+
  theme_light()


# histogram

ggplot(data = ave_rate_day, aes(mean_bed_occupancy))+
  geom_histogram(color = "navy", fill = "lavender")+
  labs(title = "Distribution of average bed occupancy rates, 2024",
       x = "Rate (%)")

# shelter data

shelter_data |> 
  select(occupancy_date, capacity_actual_bed, occupancy_rate_beds) |> 
  drop_na() |>  # filter out all missing values
  group_by(occupancy_date) |> 
  summarize(ave_beds = mean(capacity_actual_bed),
            ave_rate = mean(occupancy_rate_beds))  |> 
  pivot_longer(-occupancy_date) |> 
  ggplot(aes(occupancy_date, value, color = name)) +
  geom_line()+
  facet_wrap(~name, scales = "free_y")


shelter_data |> 
  select(occupancy_date, capacity_actual_bed, occupancy_rate_beds) |> 
  drop_na() |>  # filter out all missing values
  group_by(occupancy_date) |> 
  summarize(ave_beds = mean(capacity_actual_bed),
            ave_rate = mean(occupancy_rate_beds)) |> 
  ggplot(aes(occupancy_date, ave_rate, size = ave_beds))+
  geom_point()





