library(lubridate)
library(tidyverse)
library(mongolite)

heatmap_plot = function(df,
                        x_variable,
                        y_variable,
                        z_variable,
                        z_name,
                        main_title,
                        cols_color,
                        cols_fill,
                        fdow,
                        ldow){
  
  
  ## Plot Function
  p = ggplot(df, aes_string(x=x_variable, ## Time
                            y=y_variable, ## Day
                            fill =z_variable,
                            color = z_variable)) + ## Construct
    
    geom_tile(aes(width=0.8, height=0.8,alpha = late),stat = "identity") +
    theme_bw() +
    scale_alpha_continuous(guide=FALSE) +
    labs(title = paste0("Heatmap of ",main_title,
                        '\n First Day: ',fdow,' - Last Day:',ldow), x = "Time",
         y = "Day")+
    scale_fill_manual(name = paste0(z_name, " Scale"),  values = cols_fill) +
    scale_color_manual(name = paste0(z_name, " Scale"), values = cols_color) +
    theme(plot.title = element_text(hjust = 0.5,size = 15, face = "bold"),
          legend.key = element_rect(color = NA, fill = NA),
          legend.key.size = unit(1.5, "cm"),
          axis.text.y = element_text(size = 12),
          axis.text.x = element_text(size = 9),
          axis.title = element_text(size = 13,vjust =2,face = "bold"))
  
  return(p)
}


display_heat_map = function(df, construct, construct_title, firstDayOfWeek, lastDayOfWeek){
  ## Assuming reformat_construct_df reformats the dataframe for heatmap plotting
  ## Assuming it exists and works as intended for your use case
  long_df = reformat_construct_df(df, construct)
  
  ## Color settings for Goals and FoodQuality
  goal_plus = '#2dc937'  ## Green for positive outcomes
  goal_minus = '#cc3232' ## Red for negative outcomes
  high_quality = '#2dc937'  ## Green for high food quality
  medium_quality = '#e7b416' ## Yellow for medium food quality
  low_quality = '#cc3232'    ## Red for low food quality
  
  ## Define colors based on the construct
  if (construct == 'Goals'){
    cols_color_t =c('G-' = goal_minus, 'G+' = goal_plus)
    cols_fill_t = c('G-' = goal_minus, 'G+' = goal_plus)
  } else if (construct == 'FoodQuality'){
    cols_color_t =c('High' = high_quality, 'Medium' = medium_quality, 'Low' = low_quality)
    cols_fill_t = c('High' = high_quality, 'Medium' = medium_quality, 'Low' = low_quality)
  } else if (construct == 'Mood'){
    ## Assuming Mood colors are already defined in your original script
    ## Use those color definitions here
  }
  
  ## Call heatmap_plot function with the adjusted parameters
  p = heatmap_plot(df = long_df,
                   x_variable = 'Times',
                   y_variable = 'Day',
                   z_variable = 'Score',
                   z_name = str_to_title(construct),
                   main_title = construct_title,
                   cols_color = cols_color_t,
                   cols_fill = cols_fill_t,
                   fdow = firstDayOfWeek,
                   ldow = lastDayOfWeek)
  
  return(p)
}



## Connect to the database

username <- Sys.getenv("username")
password <- Sys.getenv("password")
data_base <- Sys.getenv("data_base")
cluster <- Sys.getenv("cluster")
url <- paste0("mongodb+srv://",username,":",password,"@",cluster,".rjzoaxj.mongodb.net/",data_base,"?retryWrites=true&w=majority")
mongo <- mongo(url = url,
               collection = "emans_info",
               db = "stability",
               options = ssl_options(key = openssl::read_cert(Sys.getenv("pem"))))
# 


data <- mongo$find()


df <- data %>%
  mutate(specificTime = ymd_hms(specificTime)) %>%
  # Adjust filter to select dates between January 21 and 27, 2024
  filter(as.Date(specificTime) >= as.Date("2024-01-21") & as.Date(specificTime) <= as.Date("2024-01-27")) %>%
  select(specificTime, timeBlock, goalOutcome, foodQuality)


# Example call for displaying a heatmap of Goal Outcomes
display_heat_map_for_goals <- display_heat_map(
  df = df, 
  construct = 'Goals', 
  construct_title = 'Goal Outcomes for the Week', 
  firstDayOfWeek = '2024-01-27',  # Again, assuming a specific week
  lastDayOfWeek = '2024-01-21'
)

# Example call for displaying a heatmap of Food Quality
display_heat_map_for_food_quality <- display_heat_map(
  df = df, 
  construct = 'FoodQuality', 
  construct_title = 'Food Quality Assessment for the Week', 
  firstDayOfWeek = '2024-01-27',  # Again, assuming a specific week
  lastDayOfWeek = '2024-01-21'
)






