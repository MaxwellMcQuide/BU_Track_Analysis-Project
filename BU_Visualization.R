# BU Data Visualization

# Libraries 
library(rvest)
library(dplyr)
library(readxl)
library(stringr)
library(tidyverse)
library(ggformula)
library(readr)
library(mosaic)
library(ggplot2)
library(esquisse)
library(extrafont)
library(gtsummary)
library(gt)
library(ggthemes)


# Imports
link <- 'https://tf.tfrrs.org/lists/4044/2023_NCAA_Division_I_All_Schools_Rankings?gender=m'
page <- read_html(link)
D1 <- page %>% html_nodes('.tablesaw-priority-4+ td a') %>% html_text()
D1_Schools <- unique(D1)
events <- c("Men's 800","Men's Mile","Men's 3000","Men's 5000")

Indoor_Track_2023 <- read_csv(
  "C:/Users/Max/Desktop/Indoor_Track_Results/Indoor_Track_2023.csv") %>%
  select(-c(PL,WIND,SC)) %>%
  event_converter() %>%
  filter(EVENT %in% events, TEAM %in% D1_Schools, TRACK_TYPE == '200m (Banked)') %>%
  mutate(BU = as.factor(if_else(TRACK == "Boston University-Track & Tennis Center - Boston, MA",'BU','OTHER')),SEASON = 2023) %>%
  arrange(SECONDS) 

Indoor_Track_2022 <- read_csv(
  "C:/Users/Max/Desktop/Indoor_Track_Results/Indoor_Track_2022.csv") %>%
  select(-c(PL,WIND,SC)) %>%
  event_converter() %>%
  filter(EVENT %in% events, TEAM %in% D1_Schools, TRACK_TYPE == '200m (Banked)') %>%
  mutate(BU = as.factor(if_else(TRACK == "Boston University-Track & Tennis Center - Boston, MA",'BU','OTHER')),SEASON = 2022) %>%
  arrange(SECONDS) 

Indoor_Track_2021 <- read_csv(
  "C:/Users/Max/Desktop/Indoor_Track_Results/Indoor_Track_2021.csv") %>%
  select(-c(PL,WIND,SC)) %>%
  event_converter() %>%
  filter(EVENT %in% events, TEAM %in% D1_Schools, TRACK_TYPE == '200m (Banked)') %>%
  mutate(BU = as.factor(if_else(TRACK == "Boston University-Track & Tennis Center - Boston, MA",'BU','OTHER')),SEASON = 2021) %>%
  arrange(SECONDS) 

Indoor_Track_2020 <- read_csv(
  "C:/Users/Max/Desktop/Indoor_Track_Results/Indoor_Track_2020.csv") %>%
  select(-c(PL,WIND,SC)) %>%
  event_converter() %>%
  filter(EVENT %in% events, TEAM %in% D1_Schools, TRACK_TYPE == '200m (Banked)') %>%
  mutate(BU = as.factor(if_else(TRACK == "Boston University-Track & Tennis Center - Boston, MA",'BU','OTHER')),SEASON = 2020) %>%
  arrange(SECONDS) 

Indoor_Track_2019 <- read_csv(
  "C:/Users/Max/Desktop/Indoor_Track_Results/Indoor_Track_2019.csv") %>%
  select(-c(PL,WIND,SC)) %>%
  event_converter() %>%
  filter(EVENT %in% events, TEAM %in% D1_Schools, TRACK_TYPE == '200m (Banked)') %>%
  mutate(BU = as.factor(if_else(TRACK == "Boston University-Track & Tennis Center - Boston, MA",'BU','OTHER')),SEASON = 2019) %>%
  arrange(SECONDS) 

Indoor_Track_2018 <- read_csv(
  "C:/Users/Max/Desktop/Indoor_Track_Results/Indoor_Track_2018.csv") %>%
  select(-c(PL,WIND,SC)) %>%
  event_converter() %>%
  filter(EVENT %in% events, TEAM %in% D1_Schools, TRACK_TYPE == '200m (Banked)') %>%
  mutate(BU = as.factor(if_else(TRACK == "Boston University-Track & Tennis Center - Boston, MA",'BU','OTHER')),SEASON = 2018) %>%
  arrange(SECONDS) 

Indoor_2018_2023 <- bind_rows(Indoor_Track_2023,Indoor_Track_2022,Indoor_Track_2021,
                              Indoor_Track_2020,Indoor_Track_2019,Indoor_Track_2018) %>%
  arrange(BU)
Indoor_2018_2023$BU <- factor(Indoor_2018_2023$BU, levels = c("BU", "OTHER"))

View(Indoor_2018_2023)
unique(Indoor_2018_2023$TRACK)
# Visualizations

library(ggthemes)
BU_Colors = c('#ff0000','#0000ff')

# 800m

  # Summary Table
summary_800 <- Indoor_2018_2023 %>%
  filter(EVENT == "Men's 800") %>%
  select(SECONDS, BU,SEASON) %>%
  rename(Track = BU) %>%
  group_by(SEASON,Track) %>% 
  summarize(Count = n(),
            Min = time_convert(min(SECONDS)),
            Q1 = time_convert(quantile(SECONDS, 0.25)),
            Median = time_convert(median(SECONDS)),
            Q3 = time_convert(quantile(SECONDS, 0.75)),
            Max = time_convert(max(SECONDS)),
            Mean = time_convert(mean(SECONDS)),
            SD = time_convert(sd(SECONDS))) %>%
  gt(groupname_col = 'SEASON') |>
  tab_header(
    title = md("NCAA D1 Men's 800m (Indoor)")
  )

  # Boxplot

Boxplots_800 <- Indoor_2018_2023 %>%
  filter(EVENT == "Men's 800") %>%
  ggplot() +
  aes(x= SECONDS, y= BU, fill = BU) +
  geom_boxplot() +
  labs(title="NCAA D1 Men's 800m (Indoor)",
       subtitle = "BU vs Other 200m Banked Tracks",
       x= 'Time',
       y= 'Track',
       caption = '800m times under 2:10 pictured',
       fill = "") +
  facet_grid(rows = vars(SEASON)) +
  scale_color_fivethirtyeight() +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),
        plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5)) +
  scale_x_continuous(limits = c(105,130),
                     breaks= c(105,110,115,120,125,130),
                     labels = c('1:45','1:50','1:55','2:00','2:05','2:10')) + 
  scale_fill_manual(values = BU_Colors) 

Boxplots_800
?after_stat
  # Histogram
Histograms_800 <- Indoor_2018_2023 %>%
  filter(EVENT == "Men's 800") %>%
  ggplot() +
  aes(x= SECONDS, y= after_stat(density), fill = BU) +
  geom_histogram(binwidth = .75) +
  labs(title="NCAA D1 Men's 800m (Indoor)",
       subtitle = "BU vs Other 200m Banked Tracks",
       x= 'Seconds',
       y= 'Relative Frequency',
       caption = '800m times under 2:10 pictured',
       fill = "") +
  facet_wrap(vars(SEASON)) +
  scale_color_fivethirtyeight() +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),
        plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5)) +
  scale_x_continuous(limits = c(105,130),
                     breaks= c(110,120,130),
                     labels = c('1:50','2:00','2:10')) + 
  scale_fill_manual(values = BU_Colors)
Histograms_800


  # Count Dataframes
Top_100_by_season_800 <- Indoor_2018_2023 %>%
  filter(EVENT == "Men's 800") %>%
  group_by(SEASON) %>%
  top_n(-100,SECONDS) %>%
  group_by(SEASON, BU) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = BU,
              values_from = count) %>%
  select(SEASON, BU)

Top_100_Expected_800 <- Indoor_2018_2023 %>%
  filter(EVENT == "Men's 800") %>%
  group_by(SEASON, BU) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = BU,
              values_from = count) %>%
  mutate(BU_EXPECTED = round(BU/(BU+OTHER)*100,digits = 1)) %>%
  select(SEASON,BU_EXPECTED)

Top_100_Expected_Difference_800 <- left_join(Top_100_by_season_800,Top_100_Expected_800, by = 'SEASON') %>%
  mutate(DIFFERENCE_100 = BU-BU_EXPECTED) %>%
  select(SEASON, DIFFERENCE_100)

Top_500_by_season_800 <- Indoor_2018_2023 %>%
  filter(EVENT == "Men's 800") %>%
  group_by(SEASON) %>%
  top_n(-500,SECONDS) %>%
  group_by(SEASON, BU) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = BU,
              values_from = count) %>%
  select(SEASON, BU)

Top_500_Expected_800 <- Indoor_2018_2023 %>%
  filter(EVENT == "Men's 800") %>%
  group_by(SEASON, BU) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = BU,
              values_from = count) %>%
  mutate(BU_EXPECTED = round(BU/(BU+OTHER)*500,digits = 1)) %>%
  select(SEASON,BU_EXPECTED)

Top_500_Expected_Difference_800 <- left_join(Top_500_by_season_800,Top_500_Expected_800, by = 'SEASON') %>%
  mutate(DIFFERENCE_500 = BU-BU_EXPECTED) %>%
  select(SEASON, DIFFERENCE_500)

Difference_800 <- left_join(Top_100_Expected_Difference_800,Top_500_Expected_Difference_800, by = 'SEASON') %>%
  mutate(DIFFERENCE_100 = ifelse(is.na(DIFFERENCE_100),0,DIFFERENCE_100),
         DIFFERENCE_500 = ifelse(is.na(DIFFERENCE_500),0,DIFFERENCE_500),
         EVENT = '800')

# Mile

# Summary Table
summary_mile <- Indoor_2018_2023 %>%
  filter(EVENT == "Men's Mile") %>%
  select(SECONDS, BU,SEASON) %>%
  rename(Track = BU) %>%
  group_by(SEASON,Track) %>% 
  summarize(Count = n(),
            Min = time_convert(min(SECONDS)),
            Q1 = time_convert(quantile(SECONDS, 0.25)),
            Median = time_convert(median(SECONDS)),
            Q3 = time_convert(quantile(SECONDS, 0.75)),
            Max = time_convert(max(SECONDS)),
            Mean = time_convert(mean(SECONDS)),
            SD = time_convert(sd(SECONDS))) %>%
  gt(groupname_col = 'SEASON') |>
  tab_header(
    title = md("NCAA D1 Men's Mile (Indoor)")
  )


# Boxplot

Boxplots_Mile <- Indoor_2018_2023 %>%
  filter(EVENT == "Men's Mile") %>%
  ggplot() +
  aes(x= SECONDS, y= BU, fill = BU) +
  geom_boxplot() +
  labs(title="NCAA D1 Men's Mile (Indoor)",
       subtitle = "BU vs Other 200m Banked Tracks",
       x= 'Time',
       y= 'Track',
       caption = 'Mile times under 5:00 pictured',
       fill = "") +
  facet_grid(rows = vars(SEASON)) +
  scale_color_fivethirtyeight() +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),
        plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5)) +
  scale_x_continuous(limits = c(230,300),
                     breaks= c(230,240,250,260,270,280,290,300),
                     labels = c('3:50','4:00','4:10','4:20','4:30','4:40','4:50','5:00')) + 
  scale_fill_manual(values = BU_Colors) 

Boxplots_Mile

# Histogram
Histograms_Mile <- Indoor_2018_2023 %>%
  filter(EVENT == "Men's Mile") %>%
  ggplot() +
  aes(x= SECONDS, y= after_stat(density), fill = BU) +
  geom_histogram(binwidth = 1.5) +
  labs(title="NCAA D1 Men's Mile (Indoor)",
       subtitle = "BU vs Other 200m Banked Tracks",
       x= 'Time',
       y= 'Relative Frequency',
       caption = 'Mile times under 5:00 pictured',
       fill = "") +
  facet_wrap(vars(SEASON)) +
  scale_color_fivethirtyeight() +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),
        plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5)) +
  scale_x_continuous(limits = c(230,300),
                     breaks= c(240,260,280,300),
                     labels = c('4:00','4:20','4:40','5:00')) + 
  scale_fill_manual(values = BU_Colors)
Histograms_Mile

  # Count Dataframes
Top_100_by_season_mile <- Indoor_2018_2023 %>%
  filter(EVENT == "Men's Mile") %>%
  group_by(SEASON) %>%
  top_n(-100,SECONDS) %>%
  group_by(SEASON, BU) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = BU,
              values_from = count) %>%
  select(SEASON, BU)

Top_100_Expected_mile <- Indoor_2018_2023 %>%
  filter(EVENT == "Men's Mile") %>%
  group_by(SEASON, BU) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = BU,
              values_from = count) %>%
  mutate(BU_EXPECTED = round(BU/(BU+OTHER)*100,digits = 1)) %>%
  select(SEASON,BU_EXPECTED)

Top_100_Expected_Difference_mile <- left_join(Top_100_by_season_mile,Top_100_Expected_mile, by = 'SEASON') %>%
  mutate(DIFFERENCE_100 = BU-BU_EXPECTED) %>%
  select(SEASON, DIFFERENCE_100)

Top_500_by_season_mile <- Indoor_2018_2023 %>%
  filter(EVENT == "Men's Mile") %>%
  group_by(SEASON) %>%
  top_n(-500,SECONDS) %>%
  group_by(SEASON, BU) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = BU,
              values_from = count) %>%
  select(SEASON, BU)

Top_500_Expected_mile <- Indoor_2018_2023 %>%
  filter(EVENT == "Men's Mile") %>%
  group_by(SEASON, BU) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = BU,
              values_from = count) %>%
  mutate(BU_EXPECTED = round(BU/(BU+OTHER)*500,digits = 1)) %>%
  select(SEASON,BU_EXPECTED)

Top_500_Expected_Difference_mile <- left_join(Top_500_by_season_mile,Top_500_Expected_mile, by = 'SEASON') %>%
  mutate(DIFFERENCE_500 = BU-BU_EXPECTED) %>%
  select(SEASON, DIFFERENCE_500)

Difference_mile <- left_join(Top_100_Expected_Difference_mile,Top_500_Expected_Difference_mile, by = 'SEASON') %>%
  mutate(DIFFERENCE_100 = ifelse(is.na(DIFFERENCE_100),0,DIFFERENCE_100),
         DIFFERENCE_500 = ifelse(is.na(DIFFERENCE_500),0,DIFFERENCE_500),
         EVENT = 'Mile')

# 3000

# Summary Table
summary_3000 <- Indoor_2018_2023 %>%
  filter(EVENT == "Men's 3000") %>%
  select(SECONDS, BU,SEASON) %>%
  rename(Track = BU) %>%
  group_by(SEASON,Track) %>% 
  summarize(Count = n(),
            Min = time_convert(min(SECONDS)),
            Q1 = time_convert(quantile(SECONDS, 0.25)),
            Median = time_convert(median(SECONDS)),
            Q3 = time_convert(quantile(SECONDS, 0.75)),
            Max = time_convert(max(SECONDS)),
            Mean = time_convert(mean(SECONDS)),
            SD = time_convert(sd(SECONDS))) %>%
  gt(groupname_col = 'SEASON') |>
  tab_header(
    title = md("NCAA D1 Men's 3000m (Indoor)")
  )


# Boxplot

Boxplots_3000 <- Indoor_2018_2023 %>%
  filter(EVENT == "Men's 3000") %>%
  ggplot() +
  aes(x= SECONDS, y= BU, fill = BU) +
  geom_boxplot() +
  labs(title="NCAA D1 Men's 3000m (Indoor)",
       subtitle = "BU vs Other 200m Banked Tracks",
       x= 'Time',
       y= 'Track',
       caption = '3000m times under 11:00 pictured',
       fill = "") +
  facet_grid(rows = vars(SEASON)) +
  scale_color_fivethirtyeight() +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),
        plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5)) +
  scale_x_continuous(limits = c(450,660),
                     breaks= c(450,480,510,540,570,600,630,660),
                     labels = c('7:30','8:00','8:30','9:00','9:30','10:00','10:30','11:00')) + 
  scale_fill_manual(values = BU_Colors) 


# Histogram
Histograms_3000 <- Indoor_2018_2023 %>%
  filter(EVENT == "Men's 3000") %>%
  ggplot() +
  aes(x= SECONDS, y= after_stat(density), fill = BU) +
  geom_histogram(binwidth = 3) +
  labs(title="NCAA D1 Men's 3000m (Indoor)",
       subtitle = "BU vs Other 200m Banked Tracks",
       x= 'Time',
       y= 'Relative Frequency',
       caption = '3000m times under 11:00 pictured',
       fill = "") +
  facet_wrap(vars(SEASON)) +
  scale_color_fivethirtyeight() +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),
        plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5)) +
  scale_x_continuous(limits = c(450,660),
                     breaks= c(480,540,600,660),
                     labels = c('8:00','9:00','10:00','11:00')) + 
  scale_fill_manual(values = BU_Colors)


# Count Dataframes
Top_100_by_season_3000 <- Indoor_2018_2023 %>%
  filter(EVENT == "Men's 3000") %>%
  group_by(SEASON) %>%
  top_n(-100,SECONDS) %>%
  group_by(SEASON, BU) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = BU,
              values_from = count) %>%
  select(SEASON, BU)

Top_100_Expected_3000 <- Indoor_2018_2023 %>%
  filter(EVENT == "Men's 3000") %>%
  group_by(SEASON, BU) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = BU,
              values_from = count) %>%
  mutate(BU_EXPECTED = round(BU/(BU+OTHER)*100,digits = 1)) %>%
  select(SEASON,BU_EXPECTED)

Top_100_Expected_Difference_3000 <- left_join(Top_100_by_season_3000,Top_100_Expected_3000, by = 'SEASON') %>%
  mutate(DIFFERENCE_100 = BU-BU_EXPECTED) %>%
  select(SEASON, DIFFERENCE_100)

Top_500_by_season_3000 <- Indoor_2018_2023 %>%
  filter(EVENT == "Men's 3000") %>%
  group_by(SEASON) %>%
  top_n(-500,SECONDS) %>%
  group_by(SEASON, BU) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = BU,
              values_from = count) %>%
  select(SEASON, BU)

Top_500_Expected_3000 <- Indoor_2018_2023 %>%
  filter(EVENT == "Men's 3000") %>%
  group_by(SEASON, BU) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = BU,
              values_from = count) %>%
  mutate(BU_EXPECTED = round(BU/(BU+OTHER)*500,digits = 1)) %>%
  select(SEASON,BU_EXPECTED)

Top_500_Expected_Difference_3000 <- left_join(Top_500_by_season_3000,Top_500_Expected_3000, by = 'SEASON') %>%
  mutate(DIFFERENCE_500 = BU-BU_EXPECTED) %>%
  select(SEASON, DIFFERENCE_500)

Difference_3000 <- left_join(Top_100_Expected_Difference_3000,Top_500_Expected_Difference_3000, by = 'SEASON') %>%
  mutate(DIFFERENCE_100 = ifelse(is.na(DIFFERENCE_100),0,DIFFERENCE_100),
         DIFFERENCE_500 = ifelse(is.na(DIFFERENCE_500),0,DIFFERENCE_500),
         EVENT = '3000')


# 5000

# Summary Table
summary_5000 <- Indoor_2018_2023 %>%
  filter(EVENT == "Men's 5000") %>%
  select(SECONDS, BU,SEASON) %>%
  rename(Track = BU) %>%
  group_by(SEASON,Track) %>% 
  summarize(Count = n(),
            Min = time_convert(min(SECONDS)),
            Q1 = time_convert(quantile(SECONDS, 0.25)),
            Median = time_convert(median(SECONDS)),
            Q3 = time_convert(quantile(SECONDS, 0.75)),
            Max = time_convert(max(SECONDS)),
            Mean = time_convert(mean(SECONDS)),
            SD = time_convert(sd(SECONDS))) %>%
  gt(groupname_col = 'SEASON') |>
  tab_header(
    title = md("NCAA D1 Men's 5000m (Indoor)")
  )

# Boxplot

Boxplots_5000 <- Indoor_2018_2023 %>%
  filter(EVENT == "Men's 5000") %>%
  ggplot() +
  aes(x= SECONDS, y= BU, fill = BU) +
  geom_boxplot() +
  labs(title="NCAA D1 Men's 5000m (Indoor)",
       subtitle = "BU vs Other 200m Banked Tracks",
       x= 'Time',
       y= 'Track',
       caption = '5000 times under 17:00 pictured',
       fill = "") +
  facet_grid(rows = vars(SEASON)) +
  scale_color_fivethirtyeight() +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),
        plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5)) +
  scale_x_continuous(limits = c(780,1020),
                     breaks= c(780,810,840,870,900,930,960,990,1020),
                     labels = c('13:00','13:30','14:00','14:30','15:00','15:30','16:00','16:30','17:00')) + 
  scale_fill_manual(values = BU_Colors) 


# Histogram
Histograms_5000 <- Indoor_2018_2023 %>%
  filter(EVENT == "Men's 5000") %>%
  ggplot() +
  aes(x= SECONDS, y= after_stat(density), fill = BU) +
  geom_histogram(binwidth = 5) +
  labs(title="NCAA D1 Men's 5000m (Indoor)",
       subtitle = "BU vs Other 200m Banked Tracks",
       x= 'Time',
       y= 'Relative Frequency',
       caption = '5000 times under 17:00 pictured',
       fill = "") +
  facet_wrap(vars(SEASON)) +
  scale_color_fivethirtyeight() +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),
        plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5)) +
  scale_x_continuous(limits = c(780,1020),
                     breaks= c(810,870,930,990),
                     labels = c('13:30','14:30','15:30','16:30')) + 
  scale_fill_manual(values = BU_Colors)

# Count Dataframes
Top_100_by_season_5000 <- Indoor_2018_2023 %>%
  filter(EVENT == "Men's 5000") %>%
  group_by(SEASON) %>%
  top_n(-100,SECONDS) %>%
  group_by(SEASON, BU) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = BU,
              values_from = count) %>%
  select(SEASON, BU)

Top_100_Expected_5000 <- Indoor_2018_2023 %>%
  filter(EVENT == "Men's 5000") %>%
  group_by(SEASON, BU) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = BU,
              values_from = count) %>%
  mutate(BU_EXPECTED = round(BU/(BU+OTHER)*100,digits = 1)) %>%
  select(SEASON,BU_EXPECTED)

Top_100_Expected_Difference_5000 <- left_join(Top_100_by_season_5000,Top_100_Expected_5000, by = 'SEASON') %>%
  mutate(DIFFERENCE_100 = BU-BU_EXPECTED) %>%
  select(SEASON, DIFFERENCE_100)

Top_500_by_season_5000 <- Indoor_2018_2023 %>%
  filter(EVENT == "Men's 5000") %>%
  group_by(SEASON) %>%
  top_n(-500,SECONDS) %>%
  group_by(SEASON, BU) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = BU,
              values_from = count) %>%
  select(SEASON, BU)

Top_500_Expected_5000 <- Indoor_2018_2023 %>%
  filter(EVENT == "Men's 5000") %>%
  group_by(SEASON, BU) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = BU,
              values_from = count) %>%
  mutate(BU_EXPECTED = round(BU/(BU+OTHER)*500,digits = 1)) %>%
  select(SEASON,BU_EXPECTED)

Top_500_Expected_Difference_5000 <- left_join(Top_500_by_season_5000,Top_500_Expected_5000, by = 'SEASON') %>%
  mutate(DIFFERENCE_500 = BU-BU_EXPECTED) %>%
  select(SEASON, DIFFERENCE_500)

Difference_5000 <- left_join(Top_100_Expected_Difference_5000,Top_500_Expected_Difference_5000, by = 'SEASON') %>%
  mutate(DIFFERENCE_100 = ifelse(is.na(DIFFERENCE_100),0,DIFFERENCE_100),
         DIFFERENCE_500 = ifelse(is.na(DIFFERENCE_500),0,DIFFERENCE_500),
         EVENT = '5000')

#Combined Count plot
combined_colors = c('#ff0000','#0000ff','#00ff00','#FFA500')

combined_counts <- bind_rows(Difference_800,Difference_mile,
                              Difference_3000,Difference_5000)
View(combined_counts)

count_plot_100 <- combined_counts %>%
  filter(SEASON != 2021) %>%
  ggplot() +
  aes(x= SEASON, y= DIFFERENCE_100,color=as.factor(EVENT)) +
  scale_y_continuous(limits=c(-10,60)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE, size=1) +
  labs(title="Difference in Expected BU Top 100 NCAA Times",
       subtitle = "",
       x= 'Season',
       y= 'Difference',
       color = 'Event') +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),
        plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5)) +
  scale_color_manual(values = combined_colors) 

count_plot_500 <- combined_counts %>%
  filter(SEASON != 2021) %>%
  ggplot() +
  aes(x= SEASON, y= DIFFERENCE_500,color=as.factor(EVENT)) +
  scale_y_continuous(limits=c(0,100)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE, size=1) +
  labs(title="Difference in Expected Top 500 NCAA Times (BU)",
       subtitle = "",
       x= 'Season',
       y= 'Difference',
       color = 'Event') +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),
        plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5)) +
  scale_color_manual(values = combined_colors) 
count_plot_100
# Other Codes

March_2018 <- tfrrs_scraper(9,month='3',year='2018')
Feburary_2018 <- tfrrs_scraper(12, month = '2',year='2018')
Janurary_2018 <- tfrrs_scraper(10, month = '1',year='2018')
December_2017 <- tfrrs_scraper(4, month = '12',year='2017')
Indoor_Track_2018 <- bind_rows(March_2018,Feburary_2018,Janurary_2018,December_2017)

write_excel_csv(Indoor_Track_2018,'Indoor_Track_2018.csv')

Indoor_2018_2023 %>%
  filter(SEASON == 2021)


time_convert <- function(number){
  minute <- number %/% 60
  second <- number %% 60
  second_floor <- floor(second)
  second_whole <- ifelse(nchar(second_floor)==1,paste('0',round(second,1),sep=''),round(second,2))
  return_value <- paste(minute,':',second_whole,sep='')
  return(return_value)
}
time_convert(50.235)

# Views

summary_800
summary_mile
summary_3000
summary_5000

Boxplots_800
Boxplots_Mile
Boxplots_3000
Boxplots_5000

Histograms_800
Histograms_Mile
Histograms_3000
Histograms_5000

count_plot_100
count_plot_500

# Tests

  
  # Linear Model Tests
lm_800 <- combined_counts %>% 
  filter(EVENT == '800m') %>% 
  lm(formula = DIFFERENCE_100 ~ SEASON, data = .)
lm_mile <- combined_counts %>% 
  filter(EVENT == 'Mile') %>% 
  lm(formula = DIFFERENCE_100 ~ SEASON, data = .)
lm_3000 <- combined_counts %>% 
  filter(EVENT == '3000m') %>% 
  lm(formula = DIFFERENCE_100 ~ SEASON, data = .)
lm_5000 <- combined_counts %>% 
  filter(EVENT == '5000m') %>% 
  lm(formula = DIFFERENCE_100 ~ SEASON, data = .)


summary(lm_800)
summary(lm_mile)
summary(lm_3000)
summary(lm_5000)

# Difference in Means

Indoor_2018_2023 %>%
  filter(EVENT == "Men's 5000") %>%
  filter(SEASON == 2018) %>%
  t.test(SECONDS~BU, data = ., mu = 0, alternative = "less")

t_test_800

# Difference in Top 100
Top_100_Expected_800

event <- "Men's Mile"
season <- 2023
Indoor_2018_2023 %>%
  filter(EVENT == event, SEASON == season) %>%
  top_n(-100,SECONDS) %>%
  tally(~BU, data = .) 

Indoor_2018_2023 %>%
  filter(EVENT == event,SEASON == season) %>%
  tally(~BU, data = .) %>%
  prop.table()

xchisq.test(~BU, data = test_stat, p = c(.272,.728))



# Messing around
count_plot_100_linear <- combined_counts %>%
  filter(SEASON != 2021) %>%
  ggplot() +
  aes(x= SEASON, y= DIFFERENCE_100,color=as.factor(EVENT)) +
  scale_y_continuous(limits=c(-10,60)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE, size=1) +
  stat_cor(method = "pearson")+
  labs(title="Difference in Expected BU Top 100 NCAA Times",
       subtitle = "",
       x= 'Season',
       y= 'Difference',
       color = 'Event') +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),
        plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5)) +
  scale_color_manual(values = combined_colors) 

count_plot_100_linear


all_plots <- plot_grid(plot_grid(Boxplots_800, Histograms_800),
                       plot_grid(Boxplots_Mile,Histograms_Mile),
                       plot_grid(Boxplots_3000, Histograms_3000),
                       plot_grid(Boxplots_5000,Histograms_5000),
                       nrow = 4)
all_plots

