# make git commits into a tree because why not

library(rvest)
library(tidyverse)



# GET COMMIT COUNTS BY DAY ------------------------------------------------


# grab commits from github
# thanks to Rohan for the rvest code; he is much better at webscraping than me
ghpage <- read_html("https://github.com/MJAlexander")

mon_contributions <-
  tibble(
    count = ghpage %>%
      html_nodes('svg') %>%
      html_nodes('g') %>%
      html_nodes('rect') %>%
      html_attr('data-count') %>% 
      as.integer(),
    date = ghpage %>%
      html_nodes('svg') %>%
      html_nodes('g') %>%
      html_nodes('rect') %>%
      html_attr('data-date') %>% 
      lubridate::as_date()
  )

# just take the most 365 most recent observation
mon_contributions <- mon_contributions[(nrow(mon_contributions) - 364):nrow(mon_contributions),]

# assign commit colors 
# if you've been more productive than me you probably need to change cut offs

commit_colors <- tibble(commit_count = c(0, 1, 5, 8, 10),
                        color = c("#39D353",
                          "#26A741",
                          "#026C32",
                          "#0D4429",
                          "#2D333B"))

# add to contributions tibble
mon_contributions <- mon_contributions %>% 
  mutate(color = commit_colors$color[findInterval(mon_contributions$count, commit_colors$commit_count)])



# MAKE THE SQUARES FOR THE TREE -------------------------------------------



# empty tibble
d <- tibble(n = rep(1:26, times = 1:26), 
            x_left = NA, 
            x_right = NA,
            y_low = NA,
            y_up = NA)

# start position
start_x_left <- -1
start_x_right <- start_x_left + 1
start_y_lower <- -1
start_y_upper <- start_y_lower + 1
d$x_left[1] <- start_x_left
d$x_right[1] <- start_x_right
d$y_low[1] <-  start_y_lower
d$y_up[1] <- start_y_upper

# fill in the rest of the tree
for(i in 2:nrow(d)){
  if(d$n[i]!=d$n[i-1]){
    d$x_left[i] <- d$x_left[i-(d$n[i]-1)] -0.75
    d$y_low[i] <- d$y_low[i-1] - 1.5
  }
  else{
    d$x_left[i] <- d$x_left[i-1] + 1.5
    d$y_low[i] <- d$y_low[i-1] 
  }
  
  d$x_right[i] <- d$x_left[i]+1
  d$y_up[i] <- d$y_low[i]+1
}


# add colors 

d <- bind_cols(d, mon_contributions[1:(nrow(mon_contributions)-14),])

# base rectangle
base <- tibble(n = rep(1:2, each = 7), 
               y_low = NA,
               y_up = NA,
               x_left = NA,
               x_right = NA)

base$y_low[1:(which(base$n==2)[1]-1)] = d$y_low[nrow(d)] - 1.5
base$y_up[1:(which(base$n==2)[1]-1)] = base$y_low[1] +1
base$x_left[1] = start_x_left - 4
base$x_right[1] <-  base$x_left[1] + 1
base$y_low[(which(base$n==2)[1]):nrow(base)] <- base$y_low[1] - 1.5
base$y_up[(which(base$n==2)[1]):nrow(base)] <- base$y_low[(which(base$n==2)[1])] +1
base$x_left[(which(base$n==2)[1])] <-  start_x_left - 4
base$x_right[(which(base$n==2)[1])] <-  base$x_left[(which(base$n==2)[1])] + 1

# fill in the rest of the base
for(i in (2:nrow(base))[-(which(base$n==2)[1]-1)]){
  base$x_left[i] <- base$x_left[i-1] + 1.5
  base$x_right[i] <- base$x_left[i]+1
}

# add in the colors

base <- bind_cols(base, mon_contributions[(nrow(mon_contributions)-13):nrow(mon_contributions),])


# PLOT THE TREE -----------------------------------------------------------


p <- ggplot(data = d) + 
  geom_rect(aes(xmin = x_left, 
                xmax = x_right, 
                ymin = y_low, 
                ymax = y_up,
                fill = color)) +
  geom_rect(data = base, aes(xmin = x_left, 
                             xmax = x_right, 
                             ymin = y_low, 
                             ymax = y_up), 
            fill = "red", alpha = 0.4) + # red color underneath
  geom_rect(data = base, aes(xmin = x_left, 
                             xmax = x_right, 
                             ymin = y_low, 
                             ymax = y_up, fill = color), 
            alpha = 0.6)

p + 
  coord_fixed() + 
  theme_void()  + 
  scale_fill_manual(values = commit_colors$color) + 
  theme(legend.position = "none")
