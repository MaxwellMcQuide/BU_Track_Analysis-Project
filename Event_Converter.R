# Event_converter

event_df_maker <<- function(df){
  event_list <- c('100','200','400','800','1500','Mile','3000','3000','5000','5000','10000','60','300','600','1000','500','3200','110 Hurdles', '55','2000 Steeplechase','Other','4600','1200','150','2 Mile','Half Marathon','1975 Steeplechase','2800','5200','2840','2000','700')
  event_regex <- c('100(?!0)','(?<!3|1)200(?!0)','400(?!0)','(?<!2)800(?!0)','1,?500m?(?!0)','(?<!2 )(Mile|MILE)','3,?000(?!0)','3(k|K)','5,?000','5(k|K)','10,?000','60(?!0)','300(?!0)','(?<!4)600','10,?00(?!0)','(?<!1,?)500(?!0)','3,?200','110', '55','2000 Steeplechase','Other','4600','1200','150m?(?!0)','2 Mile','Half Marathon','1975 Steeplechase','2800','5200','2840','2000','700')
  event_df <- data.frame(
    name = str_squish(unique(df$EVENT))
  )
  
  # Make event_df dataframe
  event_match <- function(name){
    return_value <- 'error'
    for (i in seq(1:length(event_list))){
      if (str_detect(name, event_regex[i]) == TRUE){
        if (event_list[i] == '400' && str_detect(name,'Hurdles') == TRUE){
          return_value <-paste("Men's", event_list[i],'Hurdles')
          break
        }else if(event_list[i] == '3000' && str_detect(name,'Steeplechase') == TRUE){
          return_value <- paste("Men's", event_list[i],'Steeplechase')
          break
        }else{
          return_value <- paste("Men's", event_list[i])
          break
        }
      }
    }
    return(return_value)
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



# adjustments

event_df_maker(events_2021)
events_2021_df <- data.frame(name = events_2021) 
hello <- event_df_maker(events_2021_df)
View(hello)
