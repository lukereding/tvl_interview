library(tidyverse)
library(magrittr)
library(lubridate)

df <- read_csv("~/Downloads/query (1).csv")
df$time <- as_date(df$time)

df %>%
  ggplot(aes(time, mag)) +
  geom_point() + 
  scale_x_date()

df$year <- df$time %>% year

mag <- df %>% 
  group_by(year) %>%
  summarise(avg_mag = mean(mag, na.rm = TRUE)) %>%
  ggplot(aes(year, avg_mag)) +
  geom_point() +
  geom_smooth(se = F, method = lm)+
  cowplot::theme_cowplot() +
  labs(x = "", y = "magnitude", title = "average magnitude of severe\nearthquakes") +
  theme(plot.title = element_text(
    size = rel(1.2),
    hjust = 0, vjust = 1, face = "plain"
  ))


number <- df %>% 
  group_by(year) %>%
  tally %>%
  ggplot(aes(year, n)) +
  geom_point() +
  geom_smooth(se = F, method = lm) +
  cowplot::theme_cowplot()+
  labs(x = "year", y = "#", title = "average number of severe\nearthquakes per year")+
  theme(plot.title = element_text(
    size = rel(1.2),
    hjust = 0, vjust = 1, face = "plain"
  ))

# by severe I mean > 7 on the Richter scale

require(cowplot)
plot_grid(mag, number, ncol = 1)


number <- df %>% 
  group_by(year) %>%
  tally

number %<>% mutate(year2 = as.integer(year))

model <- lm(n ~ year2, data = number)

library(modelr)

predictions <- number %>% 
  data_grid(year2 = 1967:2040) %>%
  add_predictions(model, "number") 

p1 <- df %>% 
  group_by(year) %>%
  tally %>%
  ggplot(aes(year, n)) +
  geom_point(alpha = 0.8) +
  geom_line(data= predictions, aes(x = year2, y = number), size = 1.5, color = "blue") +
  cowplot::theme_cowplot() +
  labs(y = "#", title = "average number of severe\nearthquakes per year")+
  theme(plot.title = element_text(
    size = rel(1.2),
    hjust = 0, vjust = 1, face = "plain"
  ))



mag <- df %>% 
  group_by(year) %>%
  summarise(avg_mag = mean(mag, na.rm = TRUE))

model <- lm(avg_mag ~ year, data = mag)
predictions <- mag %>% 
  data_grid(year = 1967:2040) %>%
  add_predictions(model, "avg_mag") 
p2 <- df %>% 
  group_by(year) %>%
  summarise(avg_mag = mean(mag, na.rm = TRUE)) %>%
  ggplot(aes(year, avg_mag)) +
  geom_point(alpha = 0.8) +
  labs(y = "average magnitude", title = "average magnitude of severe\nearthquakes") +
  geom_line(data= predictions, aes(x = year, y = avg_mag), size = 1.5, color = "blue")+
  theme(plot.title = element_text(
    size = rel(1.2),
    hjust = 0, vjust = 1, face = "plain"
  ))


plot_grid(p1,p2,ncol = 1)














df <- read_csv("~/Downloads/tsunamievent.csv")


df %>% 
  group_by(YEAR) %>% 
  summarise(deaths = mean(DEATHS, na.rm= TRUE)) %>%
  ggplot(aes(x = YEAR, y = deaths)) +
  geom_point() +
  coord_cartesian(xlim = c(1900, 2017))


df %>% 
  group_by(YEAR) %>% 
  tally %>%
  ggplot(aes(x = YEAR, y = n)) +
  geom_point() +
  coord_cartesian(xlim = c(1900, 2017))
tvl

















