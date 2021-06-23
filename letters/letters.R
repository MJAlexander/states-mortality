#devtools::install_github("marcusvolz/mathart")
library(tidyverse)
library(ggforce)
library(mathart)
library(tweenr)


# Making the E ------------------------------------------------------------


# make some polygons

shape_square <- data.frame(x = c(-0.2, -0.2, 1.2, 1.2, -0.2),
                           y = c(1.2, -0.2, -0.2, 1.2, 1.2))

shape_E <- data.frame(
  x = c(0,0,
        0.6, 0.6, 0.2, 
        0.2, 0.5, 0.5, 0.2,
        0.2, 0.6, 0.6, 0.2,
        0.2, 0),
  y = c(1, 0, 
        0, 0.2, 0.2, 
        0.4, 0.4, 0.6, 0.6,
        0.8, 0.8, 1, 1,
        1, 1)
)

shape_L <- data.frame(x = c(-0.2, -0.2, 0.175, 0.175, 0.175),
                      y = c(1.2, -0.2, -0.2, 1.2, 1.2))

shape_R <- data.frame(x = c(0.775, 0.775, 1.2, 1.2, 0.775),
                      y = c(1.2, -0.2, -0.2, 1.2, 1.2))

shape_B <- data.frame(x = c(0.17, 0.17, 0.88, 0.88, 0.175),
                      y = c(0, -0.2, -0.2, 0, 0))

shape_T <- data.frame(x = c(0.17, 0.17, 0.88, 0.88, 0.175),
                      y = c(1.2, 1, 1, 1.2, 1.2))

shape_M1 <- data.frame(x = c(0.375, 0.375, 0.875, 0.875, 0.375),
                      y = c(0.8, 0.6, 0.6, 0.8, 0.8))

shape_M2 <- data.frame(x = c(0.375, 0.375, 0.875, 0.875, 0.375),
                       y = c(0.4, 0.2, 0.2, 0.4, 0.4))

shape_M3 <- data.frame(x = c(0.675, 0.675, 1, 1, 0.675),
                       y = c(0.65, 0.35, 0.35, 0.65, 0.65))


# k-d tree see: https://github.com/marcusvolz/mathart

points <- mathart::points
set.seed(02042019)
result <- kdtree(points)


# plot and save

ggplot(shape_square, aes(x = x*10000, y = y*10000)) +
  geom_polygon(fill = NA) + 
  geom_segment(aes(x, y, xend = xend, yend = yend), result, color = "#E59500", lwd = 0.2) +
  xlim(-2000, 12000) + ylim(-2000, 12000) +
  theme_blankcanvas(bg_col = "#fafafa", margin_cm = 0)+
  geom_polygon(data = shape_E, aes(x = (x+0.175)*10000, y = y*10000), fill = NA, color = "#E59500", lwd = 6) + 
  geom_polygon(data = shape_L, aes(x*10000, y = y*10000), fill = "#2A1E5C") + #"#2A1E5C"
  geom_polygon(data = shape_R, aes(x*10000, y = y*10000), fill = "#2A1E5C") + 
  geom_polygon(data = shape_B, aes(x*10000, y = y*10000), fill = "#2A1E5C") + 
  geom_polygon(data = shape_T, aes(x*10000, y = y*10000), fill = "#2A1E5C") + 
  geom_polygon(data = shape_M1, aes(x*10000, y = y*10000), fill = "#2A1E5C") + 
  geom_polygon(data = shape_M2, aes(x*10000, y = y*10000), fill = "#2A1E5C") + 
  geom_polygon(data = shape_M3, aes(x*10000, y = y*10000), fill = "#2A1E5C") + 
  coord_fixed() 

ggsave("E.pdf")



# Making the H ------------------------------------------------------------

# make some polygons

shape_H <- data.frame(
  x = c(0,0,
        0.2, 0.2, 
        0.55, 0.55,
        0.75, 0.75, 
        0.55, 0.55,
        0.2, 0.2,
        0, 0),
  y = c(1, 0, 
        0, 0.4,
        0.4, 0,
        0, 1, 
        1, 0.6, 
        0.6,1,
        1, 1)
)

shape_L <- data.frame(x = c(-0.2, -0.2, 0.115, 0.115, 0.115),
                      y = c(1.2, -0.2, -0.2, 1.2, 1.2))

shape_R <- data.frame(x = c(0.885, 0.885, 1.2, 1.2, 0.885),
                      y = c(1.2, -0.2, -0.2, 1.2, 1.2))

shape_B <- data.frame(x = c(0, 0, 1, 1, 0),
                      y = c(-0.01, -0.2, -0.2, -0.01,-0.01 ))

shape_T <- data.frame(x = c(0, 0, 0.89, 0.89, 0),
                      y = c(1.2, 1.005,1.005, 1.2, 1.2))

shape_M1 <- data.frame(x = c(0.335, 0.335, 0.665, 0.665, 0.335),
                       y = c(1.1, 0.61, 0.61, 1.1, 1.1))

shape_M2 <- data.frame(x = c(0.335, 0.335, 0.665, 0.665, 0.335),
                       y = c(0.39, -0.1, -0.1, 0.39,0.39))


# make metroplis, code from here: https://github.com/marcusvolz/metropolis/blob/master/metropolis.R

set.seed(27052021)

# Parameters
n <- 10000 # iterations
r <- 75 # neighbourhood
width <- 10000 # canvas width
height <- 10000 # canvas height
delta <- 2 * pi / 180 # angle direction noise
p_branch <- 0.1 # probability of branching
initial_pts <- 3 # number of initial points
nframes <- 500 # number of tweenr frames

# Initialise data frames
points <- data.frame(x = numeric(n), y = numeric(n), dir = numeric(n), level = integer(n))
edges <-  data.frame(x = numeric(n), y = numeric(n), xend = numeric(n), yend = numeric(n), level = integer(n))

if(initial_pts > 1) {
  i <- 2
  while(i <= initial_pts) {
    points[i, ] <- c(runif(1, 0, width), runif(1, 0, height), runif(1, -2*pi, 2*pi), 1)
    i <- i + 1
  }
}

t0 <- Sys.time()


i <- initial_pts + 1
while (i <= n) {
  valid <- FALSE
  while (!valid) {
    random_point <- sample_n(points[seq(1:(i-1)), ], 1) # Pick a point at random
    branch <- ifelse(runif(1, 0, 1) <= p_branch, TRUE, FALSE)
    alpha <- random_point$dir[1] + runif(1, -(delta), delta) + (branch * (ifelse(runif(1, 0, 1) < 0.5, -1, 1) * pi/2))
    v <- c(cos(alpha), sin(alpha)) * r * (1 + 1 / ifelse(branch, random_point$level[1]+1, random_point$level[1])) # Create directional vector
    xj <- random_point$x[1] + v[1]
    yj <- random_point$y[1] + v[2]
    lvl <- random_point$level[1]
    lvl_new <- ifelse(branch, lvl+1, lvl)
    if(xj < 0 | xj > width | yj < 0 | yj > height) {
      next
    }
    points_dist <- points %>% mutate(d = sqrt((xj - x)^2 + (yj - y)^2))
    if (min(points_dist$d) >= 1 * r) {
      points[i, ] <- c(xj, yj, alpha, lvl_new)
      edges[i, ] <- c(xj, yj, random_point$x[1], random_point$y[1], lvl_new)
      # Add a building if possible
      buiding <- 1
      valid <- TRUE
    }
  }
  i <- i + 1
  print(i)
}

edges <- edges %>% filter(level > 0)

sand <- data.frame(alpha = numeric(0), x = numeric(0), y = numeric(0))
perp <- data.frame(x = numeric(0), y = numeric(0), xend = numeric(0), yend = numeric(0))


# plot and save

ggplot(shape_square, aes(x = x*10000, y = y*10000)) +
  geom_polygon(fill = NA) + 
  geom_segment(aes(x+1530, y, xend = xend+1530, yend = yend, size = -level), edges, lineend = "round", color = "#CA1551") +
  scale_size_continuous(range = c(0.5, 0.5)) +
  theme_void() +
  theme(legend.position = "none")+
  xlim(-2000, 12000) + ylim(-2000, 12000) +
  geom_polygon(fill = NA) + 
  geom_polygon(data = shape_H, aes(x = (x+0.125)*10000, y = y*10000), fill = NA, color = "#CA1551", lwd = 6) + 
  geom_polygon(data = shape_L, aes(x*10000, y = y*10000), fill = "#2374AB") + #"#2374AB"
  geom_polygon(data = shape_R, aes(x*10000, y = y*10000), fill = "#2374AB") +
  geom_polygon(data = shape_B, aes(x*10000, y = y*10000), fill = "#2374AB") +
  geom_polygon(data = shape_T, aes(x*10000, y = y*10000), fill = "#2374AB") +
  geom_polygon(data = shape_M1, aes(x*10000, y = y*10000), fill = "#2374AB") +
  geom_polygon(data = shape_M2, aes(x*10000, y = y*10000), fill = "#2374AB") +
  coord_fixed()

ggsave("H.pdf")
