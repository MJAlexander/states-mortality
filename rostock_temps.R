# compare Roland's temperature data on a daily basis to historical data collected elsewhere in Rostock

library(lubridate)
library(ggrepel)
library(patchwork)
library(scales)

d_rostock <- read_csv("export.csv") # from https://meteostat.net/en/station/10170?t=2020-05-01/2021-12-30
d_roland <- read_csv("temperatureReddelich.csv") #from https://www.kaggle.com/rolandrau/temperature-in-reddelich

d_long <- d_roland %>% 
  mutate(date = as.Date(theDate)) %>% 
  group_by(date) %>% 
  summarise(average_temp = mean(outTempC)) %>% 
  left_join(d_rostock) %>% 
  select(date, average_temp, tavg) %>% 
  rename(`Warnemünde` = tavg, `Roland's` = average_temp) %>% 
  pivot_longer(-date, names_to = "source", values_to = "temp")


p <- d_long %>% 
  mutate(label = ifelse(date == max(date), source, NA)) %>% 
  ggplot(aes(date, temp, col = source)) + 
  geom_line() + 
  scale_color_brewer(palette = "Set1") + 
  geom_label_repel(aes(label = label),
                   nudge_x = 1,
                   nudge_y = 1,
                   na.rm = TRUE) + 
  theme_bw() + 
  theme(legend.position = 'none') + 
  xlab("") + 
  ylab("Average daily temperature (C)")

d_diff <- d_long %>% 
  pivot_wider(names_from = "source", values_from = "temp") %>% 
  mutate(diff = `Warnemünde` - `Roland's`) %>% 
  mutate(percent_diff = diff/`Warnemünde`*100) 

p2 <- d_diff %>% 
  ggplot(aes(date, diff, label = as.character(format(date, "%d-%m-%y")))) + 
  geom_line(col = "turquoise") + 
  geom_hline(yintercept = 0, lty = 2) +
  theme_bw() + 
  ylab("difference (C)") + 
  xlab("") + 
  geom_label_repel(data = subset(d_diff, abs(diff)>3), 
                   nudge_x = 1,
                   size = 3)+
  scale_x_date(labels = date_format("%m-%y"))

p+p2 + 
  plot_layout(ncol = 1)
