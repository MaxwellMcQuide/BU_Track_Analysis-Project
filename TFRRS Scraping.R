#Libraries

library(rvest)
library(dplyr)
library(readxl)
library(stringr)
library(tidyverse)
library(ggformula)
library(readr)

# Scraping From TFRRS

  # Page Setup (Must check TFRRS for page count)

amount_of_track_pages <- 54
link = 'https://www.tfrrs.org/results_search_page.html?page=1&search_query=&with_month=&with_sports=track&with_states=&with_year=2023'
year = '2023'

  # Nodes, Regex, and initial variables

node_meet = '.tablesaw-columntoggle'
node_link <- '#filterrific_results a'
event_node = '.pl-5'
result_node = '.col-lg-12'

regex_link ='/results/'
regex_link_2 <- '(.*/[0-9][0-9][0-9][0-9][0-9]/)(.*)'

meets <- data.frame()
links <- c()

full_track_results <- data.frame()
full_field_results <- data.frame()
full_relay_results <- data.frame()

  # Cleaning Functions

second_converter <- function(table){
  regex_time ='(.*):(.+)'
  over_minute <- mutate(filter(table,nchar(TIME) > 5), SECONDS = 60*(as.numeric(str_match(TIME,regex_time)[,2]))+ as.numeric(str_match(TIME,regex_time)[,3]))
  under_minute <- mutate(filter(table,nchar(TIME) <= 5), SECONDS = as.numeric(TIME))
  table <- bind_rows(under_minute,over_minute) 
}


track_cleaning <- function(table) {
  regex_time ='(.*):(.+)'
  table <- filter(table, !is.na(PL)) %>%
    filter(YEAR %in% c('FR-1','SO-2','JR-3','SR-4')) #%>%
    #mutate(PL = dense_rank(PL))
  #if ('Wind' %in% colnames(table)){
  #  mutate(table, WIND = as.numeric(WIND))
  #}
  second_converter(table)
}

relay_cleaning <- function(table) {
  regex_time ='(.*):(.+)'
  table <- filter(table, !is.na(PL)) %>%
    mutate(PL = dense_rank(PL))
  if ('Wind' %in% colnames(table)){
    mutate(table, WIND = as.numeric(WIND))
  }
  second_converter(table)
}

  #Link Scraping

for (i in seq(1,amount_of_track_pages)){
  print(paste(i,'of',amount_of_track_pages,'pages'))
  link = paste('https://www.tfrrs.org/results_search_page.html?page=',i,'&search_query=&with_month=&with_sports=track&with_states=&with_year=',year, sep="")
  page = read_html(link)
  new_meets <- page %>% html_nodes(node_meet) %>%
    html_table() %>% .[[1]]
  new_links = page %>% html_nodes(node_link) %>%
    html_attr("href")
  LINK <- c()
  for (link in new_links){
    if (str_detect(link, regex_link)){
      LINK <- append(LINK,link)
    }
  }
  new_meets <- cbind(new_meets, LINK)
  meets <- bind_rows(meets, new_meets)
}

  #Result Scraping (loop through meet list)

for (index in seq(1:nrow(meets))){
  grouped_link <- str_match(meets[index,'LINK'],regex_link_2)
  link <- paste('https://tf.tfrrs.org/',grouped_link[,2],'m/',grouped_link[,3],sep='')
  page = read_html(link)
  
  events <- page %>% html_nodes(event_node) %>% html_text()
  results <- page %>% html_nodes(result_node) %>%
    html_table() 
  
  print(paste(index,' of ',nrow(meets),':' ,'',link,sep=''))

  track_results <- data.frame() 
  field_results <- data.frame() 
  relay_results <- data.frame() 
  
  for (i in seq(1:length(results))){
  if(length(results)==0){
    next
  }
    results[[i]]<- mutate(results[[i]],across(everything(), as.character))
    results[[i]] <- mutate(results[[i]],EVENT = events[[i]])
    if('MARK' %in% colnames(results[[i]]) ||'POINTS' %in% colnames(results[[i]])){
      next
      #field_results <- bind_rows(field_results, results[[i]])%>% mutate(DATE = meets[index,'DATE']) %>% mutate(MEET = meets[index,'MEET'])
    }else if('ATHLETES' %in% colnames(results[[i]])){
      next
      #results[[i]] <- relay_cleaning(results[[i]])
      #relay_results <- bind_rows(relay_results, results[[i]])%>% mutate(DATE = meets[index,'DATE']) %>% mutate(MEET = meets[index,'MEET'])
    }else{
      results[[i]] <- track_cleaning(results[[i]])
      track_results <- bind_rows(track_results, results[[i]])%>% mutate(DATE = meets[index,'DATE']) %>% mutate(MEET = meets[index,'MEET'])
    }
  }
full_track_results <- bind_rows(full_track_results, track_results)
}

# Dates

date_converter <- function(df){
  date_regex = "-[0-9]+"
  return(df %>% mutate(DATE = sub("-[0-9]+",'',DATE)) %>% mutate(DATE = as.Date(DATE,format='%m/%d/%y')))
}

Track <- Track %>% date_converter() %>% mutate(EVENT = str_squish(EVENT))

# Events

event_list <- c('100','200','400','800','1500','Mile','3000','5000','10000','60','300','600','1000','500','3200','110 Hurdles', '55','2000 Steeplechase','Other','4600')
event_regex <- c('100(?!0)','(?<!3)200(?!0)','400(?!0)','800(?!0)','1,?500m?(?!0)','Mile','3,?000(?!0)','5,?000','10,?000','60(?!0)','300(?!0)','(?<!4)600','10,?00(?!0)','(?<!1,?)500(?!0)','3,?200','110', '55','2000 Steeplechase','Other','4600')
event_df = data.frame(
  name = str_squish(unique(Track$EVENT))
)

  # Make event_df dataframe
event_match <- function(name){
  for (i in seq(1:length(event_list))){
    if (str_detect(name, event_regex[i]) == TRUE){
      if (event_list[i] == '400' && str_detect(name,'Hurdles') == TRUE){
        return(paste("Men's", event_list[i],'Hurdles'))
        break
      }else if(event_list[i] == '3000' && str_detect(name,'Steeplechase') == TRUE){
        return(paste("Men's", event_list[i],'Steeplechase'))
        break
      }else{
        return(paste("Men's", event_list[i]))
        break
      }
    }
  }
}

for (ii in seq(1:length(event_df[['name']]))){
  event_df[['Event']][ii] <- event_match(event_df[['name']][ii])
}

  # Edit events in Track Dataframe
Track <- Track %>% mutate(EVENT = event_df[match(Track$EVENT,event_df$name),2])

# Reads, Views, and Saves

Track <- NCAA_Track_2023_raw
View(Track)
dim(Track)

Track <- read_csv("C:/Users/Max/Desktop/NCAA_Track_2023_raw.csv") %>% 
  select(-c("...1",'PL','SC')) %>%
  mutate(across(everything(), str_squish)) %>%
  date_converter()


# Playground

times_400 <- Track %>% 
  filter(EVENT == "Men's 400", TIME != 'DQ') %>%
  mutate(DATE = as.Date(sub('^[0-9]+-','2023-',DATE))) %>%
  date_converter()

outdoor <- filter(times_400, DATE >= as.Date('2023-03-17'))


team_average <- outdoor %>% 
  group_by(TEAM) %>%
  summarize(AVERAGE_400 = mean(SECONDS), NUM_RACES = n(NAME)) 

count <- count(times_400,TEAM)
combine <- left_join(team_average,count, by = 'TEAM')

my_model <- lm(AVERAGE_400~n, data = combine)
summary(my_model)

Florida <- filter(outdoor, TEAM == 'Florida')
Georgia <- filter(outdoor, TEAM == 'Georgia')

View(combine)
View(team_average)

View(Georgia)
dim(team_average)

View(full_track_results)
