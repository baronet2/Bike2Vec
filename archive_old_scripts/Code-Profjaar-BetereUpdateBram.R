setwd('D:\\Thesissen 2020\\Mathijs\\Thesis_finaal')
rm(list=ls())
# General-purpose data wrangling
library(tidyverse)  

# Parsing of HTML/XML files  
library(rvest)    

# String manipulation
library(stringr) 
library(stringi)

# Verbose regular expressions
library(rebus)     

# Eases DateTime manipulation
library(lubridate)

# Column splits enabled
library(reshape2)

library(varhandle)

library(xlsx)

library(tidyr)

library(dplyr)


#years <- c(2019,2018,2017,2016,2015,2014,2013,2012,2011,2010,2009,2008)
years <- c(2019:2021)
find.index <- function(x, value, degree) {
  r <- rle(x)
  match.pos <- which(r$value == value)
  if (length(match.pos) < degree) {
    return(NA)  
  }
  return(sum(r$length[1:(match.pos[degree]-1)])+1)
}
get_all <- function(html){
  read_html(html) %>% 
    html_nodes('td , th' ) %>%      
    html_text()                             
}

get_teams <- function(html){
  read_html(html) %>% 
    html_nodes('.mob_columns1 a' ) %>%      
    html_text()                             
}
get_division <- function(html){
  read_html(html) %>% 
    html_nodes('.teamsOverview a, .statDivLeft h3' ) %>%      
    html_text()                             
}


get_riders <- function(html){
  read_html(html) %>% 
    html_nodes('.pad2 a') %>%      
    html_text()                             
}
get_trainee <- function(html){
  read_html(html) %>% 
    html_nodes('.item') %>%      
    html_text()                             
}


rm_accent <- function(str,pattern="all") {
  if(!is.character(str))
    str <- as.character(str)
  
  pattern <- unique(pattern)
  
  if(any(pattern=="Ç"))
    pattern[pattern=="Ç"] <- "ç"
  
  symbols <- c(
    acute = "áéíóúÁÉÍÓÚýÝ",
    grave = "àèìòùÀÈÌÒÙ",
    circunflex = "âêîôûÂÊÎÔÛ",
    tilde = "ãõÃÕñÑ",
    umlaut = "äëïöüÄËÏÖÜÿ",
    cedil = "çÇ"
  )
  
  nudeSymbols <- c(
    acute = "aeiouAEIOUyY",
    grave = "aeiouAEIOU",
    circunflex = "aeiouAEIOU",
    tilde = "aoAOnN",
    umlaut = "aeiouAEIOUy",
    cedil = "cC"
  )
  
  accentTypes <- c("´","`","^","~","¨","ç")
  
  if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
    return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))
  
  for(i in which(accentTypes%in%pattern))
    str <- chartr(symbols[i],nudeSymbols[i], str) 
  
  return(str)
}

teams_list<-data.frame()
for (year in years){ 
  tryCatch({
    URL_teams<-paste0("https://www.procyclingstats.com/teams.php?s=worldtour&year=",year)
   
    teams_all<-get_teams(URL_teams)
    
    teams_all<-rm_accent(teams_all)
    teams_all<-tolower(teams_all)
    
    
    teams_all<-gsub("team-fortuneo-samsic", "fortuneo-samsic", teams_all)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
    teams_all<-stri_trim(teams_all)
    teams_all<-gsub(" ", "-", teams_all)
    
    teams_all<-gsub("'", "-", teams_all)
    teams_all<-gsub("'", "-", teams_all)
    teams_all<-gsub("/", "-", teams_all)
    teams_all<-gsub("&", "", teams_all)
    teams_all<-gsub(",", "", teams_all)
    teams_all<-gsub("---", "-", teams_all) 
    teams_all<-gsub("---", "-", teams_all)
    teams_all<-gsub("--", "-", teams_all)
    
    
    teams<-as.data.frame(matrix(teams_all, ncol = 1, byrow = TRUE))
    teams["year"]<-year
    teams_list<-rbind(teams_list,teams)
    
  },error = function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
colnames(teams_list)[1]<-as.character("Team")

currentyear<-min(years)
riders_year<-data.frame()
merged_team<-data.frame()
rider_list <- c()

for(i in 1:nrow(teams_list)){
  tryCatch({
  team_row<-teams_list[i,]
 
   if(team_row$Team=="astana" & (!team_row$year %in% c(2007,2008)))
   {
    team_row$Team<-"astana-pro-team" 
   }
   
  if(team_row$Team=="androni-giocattoli-sidermec"& (team_row$year==2018|team_row$year==2019|team_row$year==2020)){
    team_row$Team<-"androni-sidermec-bottecchia"
  }
  if(team_row$Team=="accent.jobs-willems-veranda's"){
    team_row$Team<-"accent.jobs-willems-veranda-s"
  }
  if(team_row$Team=="team-fortuneo-samsic"){
    team_row$Team<-"fortuneo-samsic"
  }
  if(team_row$Team=="holowesko-citadel-p-b-arapahoe-resources"){
    team_row$Team<-"holowesko-citadel-pb-arapahoe-resources"
  }
  if(team_row$Team=="team-bikeexchange"){
    team_row$Team<-"teambikeexchange"
  }
  if(team_row$Team=="riwal-ceramicspeed-cycling-team"){
    team_row$Team<-"riwal-readynez-cycling-team"
  }
  if(team_row$Team=="bb-hotels-p-b-ktm"){
    team_row$Team<-"b-b-hotels-pb-ktm"
  }
  
  
  if(team_row$Team=="team-dimension-data"& (team_row$year==2016|team_row$year==2017)){
    team_row$Team<-"dimension-data"
  }
  if(team_row$Team=="team-csc"& (team_row$year==2003)){
    team_row$Team<-"csc-proteam"
  }
  if(team_row$Team=="bb-hotels-vital-concept-p-b-ktm"){
    team_row$Team<-"vital-concept-bb-hotels-pb-ktm"
  }
  if(team_row$Team=="bardiani-csf-faizane"){
    team_row$Team<-"bardiani-csf"
  }
 
  
   
    URL_team<-paste0("https://www.procyclingstats.com/team/",team_row$Team,"-",team_row$year)
    if(team_row$Team=="milram"& (team_row$year %in% c(2008,2009))){
      URL_team<-paste0(URL_team,2)
    }
    
    riders_all_trainee<-get_trainee(URL_team)
    riders_all<-get_riders(URL_team)
    
    
    if(is_empty(riders_all)){
      print(team_row$Team)
      print(team_row$year)
    }
    riders<-unique(riders_all)
    rider_list <- c(rider_list, riders)
    },error = function(e){cat("Fout:",conditionMessage(e), "\n")})
}

rider_list <- unique(rider_list)

save(rider_list, file = 'ridersPro20192021.Rdata')
