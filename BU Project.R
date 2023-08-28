#Libraries
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

# Event_converter

event_df_maker <- function(df){
  event_list <- c('100','200','400','800','1500','Mile','3000','5000','10000','60','300','600','1000','500','3200','110 Hurdles', '55','2000 Steeplechase','Other','4600','1200','150','2 Mile','Half Marathon','1975 Steeplechase','2800')
  event_regex <- c('100(?!0)','(?<!3|1)200(?!0)','400(?!0)','(?<!2)800(?!0)','1,?500m?(?!0)','(?<!2 )Mile','3,?000(?!0)','5,?000','10,?000','60(?!0)','300(?!0)','(?<!4)600','10,?00(?!0)','(?<!1,?)500(?!0)','3,?200','110', '55','2000 Steeplechase','Other','4600','1200','150m?(?!0)','2 Mile','Half Marathon','1975 Steeplechase','2800')
  event_df <<- data.frame(
    name = str_squish(unique(df$EVENT))
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
  return(event_df)
}


event_converter <- function(df){
  event_df <- event_df_maker(df)
  return(df %>% mutate(EVENT = event_df[match(df$EVENT,event_df$name),2]))
}

# TFRRS Scraper

tfrrs_scraper <- function(pages,query = '',month = '', sport = 'track',state = '',year = ''){
  meets <- data.frame()
  links <- c()
  
  node_meet <- '.tablesaw-columntoggle'
  node_link <- '#filterrific_results a'
  event_node <- '.pl-5'
  result_node <- '.col-lg-12'
  regex_link_2 <- '(.*/[0-9][0-9][0-9][0-9][0-9]/)(.*)'
  track_type_node <- '.hidden-sm-down+ .inline-block , .hidden-md-up+ .inline-block'
  
  second_converter <- function(table){
    regex_time ='(.*):(.+)'
    over_minute <- mutate(filter(table,nchar(TIME) > 5), SECONDS = 60*(as.numeric(str_match(TIME,regex_time)[,2]))+ as.numeric(str_match(TIME,regex_time)[,3]))
    under_minute <- mutate(filter(table,nchar(TIME) <= 5), SECONDS = as.numeric(TIME))
    table <- bind_rows(under_minute,over_minute) 
  }
  
  track_cleaning <- function(table) {
    table <- filter(table, !is.na(PL)) %>%
      filter(YEAR %in% c('FR-1','SO-2','JR-3','SR-4')) %>%
      second_converter()
  }
  
  date_converter <- function(df){
    date_regex = "-[0-9]+"
    return(df %>% mutate(DATE = sub("-[0-9]+",'',DATE)) %>% mutate(DATE = as.Date(DATE,format='%m/%d/%y')))
  }
  
  print('Gathering Links...')
  for (i in seq(1,pages)){
      link <- paste('https://www.tfrrs.org/results_search_page.html?page=',i,'&search_query=',query,'&with_month=',month,'&with_sports=',sport,'&with_states=',state,'&with_year=',year,sep='')
      page = read_html(link)
      new_meets <- page %>% html_nodes(node_meet) %>%
        html_table() %>% .[[1]]
      new_links = page %>% html_nodes(node_link) %>%
        html_attr("href")
      LINK <- c()
      regex_link <<- '/results/'
      for (link in new_links){
        if (str_detect(link, regex_link)){
          LINK <- append(LINK,link)
        }
      }
      new_meets <- cbind(new_meets, LINK)
      meets <- bind_rows(meets, new_meets)
  }
  print(paste('There are',nrow(meets),'meets in this data set'))
  
  full_track_results <- data.frame()  
  
  for (i in seq(1:nrow(meets))){
    grouped_link <- str_match(meets[i,'LINK'],regex_link_2)
    link <- paste('https://tf.tfrrs.org/',grouped_link[,2],'m/',grouped_link[,3],sep='')
    page = read_html(link)
    
    events <- page %>% html_nodes(event_node) %>% html_text()
    results <- page %>% html_nodes(result_node) %>%
      html_table() 
    track_type <- page %>% html_nodes(track_type_node) %>% html_text() %>% str_squish()
    if (length(track_type) == 2){
      track_type[3] <- '400m'
    }
    
    
    print(paste(i,' of ',nrow(meets),':' ,'',link,sep=''))
    
    track_results <- data.frame() 
    field_results <- data.frame() 
    relay_results <- data.frame() 
    
    for (index in seq(1:length(results))){
      if(length(results)==0){
        next
      }
      results[[index]]<- mutate(results[[index]],across(everything(), as.character))
      results[[index]] <- mutate(results[[index]],EVENT = events[[index]])
      if('MARK' %in% colnames(results[[index]]) ||'POINTS' %in% colnames(results[[index]])){
        next
        #field_results <- bind_rows(field_results, results[[i]])%>% mutate(DATE = meets[i,'DATE']) %>% mutate(MEET = meets[i,'MEET'])
      }else if('ATHLETES' %in% colnames(results[[index]])){
        next
        #results[[i]] <- relay_cleaning(results[[i]])
        #relay_results <- bind_rows(relay_results, results[[i]])%>% mutate(DATE = meets[i,'DATE']) %>% mutate(MEET = meets[i,'MEET'])
      }else{
        results[[index]] <- track_cleaning(results[[index]])
        track_results <- bind_rows(track_results, results[[index]])%>% 
          mutate(DATE = meets[i,'DATE']) %>% 
          mutate(MEET = meets[i,'MEET']) %>%
          mutate(TRACK = track_type[2]) %>%
          mutate(TRACK_TYPE = track_type[3])
      }
    }
    full_track_results <- bind_rows(full_track_results, track_results)
  }
  print('Complete')
  full_track_results <- full_track_results %>% date_converter() %>% mutate(EVENT = str_squish(EVENT))
  return(full_track_results)
}

# Event Fuckery
unique(Indoor_Track_2022$EVENT)



# Playground
link <- 'https://tf.tfrrs.org/lists/4044/2023_NCAA_Division_I_All_Schools_Rankings?gender=m'
page <- read_html(link)
D1 <- page %>% html_nodes('.tablesaw-priority-4+ td a') %>% html_text()
D1_Schools <- unique(D1)
D1_Schools

Track_D1_Indoor <- Track %>%
  filter(TEAM %in% D1_Schools) %>%
  filter(EVENT %in% c("Men's 800","Men's Mile","Men's 3000","Men's 5000")) %>%
  filter(TRACK_TYPE != '400m')

View(Track_D1_Indoor)

BU <- Track_D1_Indoor %>%
  filter(TRACK == 'Boston University-Track & Tennis Center - Boston, MA')
BU_Mile <- BU %>%
  filter(EVENT == "Men's Mile")

Not_BU_Mile <- Track_D1_Indoor %>%
  filter(TRACK != 'Boston University-Track & Tennis Center - Boston, MA') %>%
  filter(EVENT == "Men's Mile")


library(dplyr)
library(ggplot2)


# Save and Load

write_excel_csv(Indoor_Track_2023,'Indoor_Track_2023.csv')
Track <- read_csv("C:/Users/Max/Desktop/Indoor_Track_2023.csv") %>%
  select(-c(PL,WIND,SC))


