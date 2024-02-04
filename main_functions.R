


library(tidyverse)



##-----------------------------------------------------##
## Name: draw_key_polygon3
## Purpose: IONO
## Input: data, params, size
## Output: grid
## Notes:
##-----------------------------------------------------##

draw_key_polygon3 <- function(data, params, size) {
  lwd <- min(data$size, min(size) / 4)
  
  grid::rectGrob(
    width = grid::unit(0.6, "npc"),
    height = grid::unit(0.6, "npc"),
    gp = grid::gpar(
      col = data$colour,
      fill = alpha(data$fill, data$alpha),
      lty = data$linetype,
      lwd = lwd * .pt,
      linejoin = "mitre"
    ))
}
GeomTile$draw_key = draw_key_polygon3


##-----------------------------------------------------##
## Name: heatmap_plot
## Purpose: to visualize heat map
## Input: df, x variable, y variable, z variable, construct name, title and colors
## Output: heatmap
## Notes:
##-----------------------------------------------------##
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
  # Add a custom legend for alpha
  
  
  return(p)
}



##-----------------------------------------------------##
## Name: reformat_construct_df
## Purpose: to reformat data frame of constructs into plottable format
## Input: data frame of constructs
## Output: transformed data frame
## Notes:
##-----------------------------------------------------

reformat_construct_df = function(df){
  ## Set Time and Days 
  evaluation_time = c('Wake Up', '7:45 AM', '10:45 AM', 
                      '12:00 PM', '3:00 PM', '6:00 PM', '8:30 PM')
  days_of_week = c('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday')
  
  
  ## All Possible outcomes based on data frame
  possible_vec = unique(c(df))
  
  ## Specify Day of Week as Columns
  colnames(df) = evaluation_time
  
  ## Specify Day of week as Rows
  df = as.data.frame(df)
  row.names(df) = days_of_week
  
  df = t(df)
  
  
  ## Reformat data frame for plot
  long_df = data.frame(df) %>%
    rownames_to_column('Times') %>%
    gather(Day,Score,Sunday:Saturday) %>% 
    mutate(late = case_when(
      str_detect(Score,'l') ~ 0.25,
      TRUE~ 0.9
    )
    ) 
  
  ## Turn Proper Variables into Factors
  long_df$Day = factor(long_df$Day,days_of_week)
  long_df$Times = factor(long_df$Times,evaluation_time)
  
  
  return(long_df)
}



##-----------------------------------------------------
## Name: display_heat_map
## Purpose: to display heatmap based on formatted data
## Input: data frame of constructs, construct measured
## Output: heatmap
##-----------------------------------------------------

display_heat_map = function(df,
                            construct,
                            construct_title,
                            firstDayOfWeek,
                            lastDayOfWeek){
  
  
  ## Reformat Data Frame
  long_df = reformat_construct_df(df)
  
  ## Goals
  goal_plus = '#2dc937'  ## Green
  goal = '#e7b416'       ## Yellow
  goal_minus = '#cc3232' ## Red
  
  ## Moods
  mood_plus = '#008000'  ## Green
  mood = '#ADD8E6'       ## Yellow
  mood_minus = '#808080' ## Red
  
  ## Moods
  fq_low = '#0000FF'  ## Green
  fq = '#FFA500'       ## Yellow
  fq_high = '#FF0000' ## Red
  
  
  ## Set Out color
  if(construct == 'Food Quality'){
    cols_color_t =c('H-'= fq_high,'M' = fq, 'L' = fq_low,
                    'Hl'= fq_high,'Ml' = fq, 'Ll' = fq_low)
    
    cols_fill_t =c('H'= fq_high,'M' = fq, 'L' = fq_low,
                   'Hl'= fq_high,'Ml' = fq, 'Ll' = fq_low)
  } else if (construct == 'Goals'){
    
    cols_color_t =c('G-'=goal_minus,'G' = goal, 'G+' = goal_plus,
                    'G-l'=goal_minus,'Gl' = goal, 'G+l' = goal_plus)
    
    cols_fill_t =c('G-'=goal_minus,'G' = goal, 'G+' = goal_plus,
                   'G-l'=goal_minus,'Gl' = goal, 'G+l' = goal_plus)
  } else if(construct == 'Mood'){
    cols_color_t =c('M-'=mood_minus,'M' = mood, 'M+' = mood_plus,
                    'M-l'=mood_minus,'Ml' = mood, 'M+l' = mood_plus)
    
    cols_fill_t =c('M-'=mood_minus,'M' = mood, 'M+' = mood_plus,
                   'M-l'=mood_minus,'Ml' = mood, 'M+l' = mood_plus) 
  }
  ## Display heat map
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



##------------------------------------------------------------
## Name: numericConstructs
## Purpose: TO convert a construct in to numbers
## Input: a construct
## Output: a number
##------------------------------------------------------------
numericConstructs = function(x){
  
  
  ## Current Levels translation
  if(str_detect(x,'G+') | str_detect(x,'M+') | str_detect(x,'L') ){
    value = 5
  } else if( str_detect(x,'G') | str_detect(x,'M') | str_detect(x,'M') ){
    value = 3
  } else if( str_detect(x,'G-') | str_detect(x,'M-') | str_detect(x,'H') ){
    value = 1
  }
  
  
  ## Late penalty
  late_constant = 0.9
  if(str_detect(x,'l')){
    value = value *late_constant
  }
  
  
  return(value)
  
  
}


##------------------------------------------------------------
## Name: valuesFromConstructs
## Purpose: TO convert  a vector constructs in to a vector numbers
## Input: a vector constructs
## Output: a vector of numbers
##------------------------------------------------------------
valuesFromConstructs = function(x){
  values = map(x,numericConstructs) %>% 
    unlist()
  return(values)
}


##------------------------------------------------------------
## Name: color_df
## Purpose: used as data to show colors for month days
##------------------------------------------------------------
color_df = data.frame(
  colors_hex = c('#2dc937', '#e7b416','#cc3232'),
  feelings = c('Great','OK','Need Help')
  
)
