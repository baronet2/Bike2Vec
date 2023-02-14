load('proYear_df.Rdata')

get_riders <- function(html){
  read_html(html) %>% 
    html_nodes('td') %>%      
    html_text()                             
}

colnames(proYear_df) <- c('Rider Name', 'Year', 'Age', 'Division')

proYear_df <- proYear_df %>% mutate_if(is.factor, as.character)
proYear_df$Age <- as.numeric(proYear_df$Age)

for (year in 2020:2021){

  url <- paste0('https://www.procyclingstats.com/teams.php?season=',year,'&filter=Filter&s=new-on-pro-level')
 
  text <- get_riders(url)
  df <- data.frame(matrix(text, nrow = length(text)/5, byrow = TRUE))
  colnames(df) <- c('ID', 'Rider Name', 'Team', 'Division', 'Age')
  df$Year <- year
  
  df$Division <- case_when(
    df$Division=='PRT' ~ 2,
    df$Division=='WT' ~ 1)
  
  df$Team <- df$ID <- NULL
  
  df <- df %>% mutate_if(is.factor, as.character)
  
  df$Age <- as.numeric(df$Age)
  
  df <- df %>% filter(Age <= 25, !is.na(Division))
  
  proYear_df <- rbind(proYear_df, df)
}

save(proYear_df, file = "updated_neopros_bram.Rdata")

plot(proYear_df %>% group_by(Year) %>% summarise(average_age = mean(Age)), type = 'b')
?plot
