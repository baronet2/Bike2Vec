setwd('D:\\Thesissen 2020\\Mathijs\\Thesis_finaal')
rm(list=ls())
# General-purpose data wrangling
library(tidyverse)  

# Parsing of HTML/XML files  
library(rvest)    

# String manipulation
library(stringr)   

# Verbose regular expressions
library(rebus)     

# Eases DateTime manipulation
library(lubridate)

# Column splits enabled
library(reshape2)

library(varhandle)

library(tidyr)


years <- c(2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)

get_all <- function(html){
  read_html(html) %>% 
    html_nodes('td , th' ) %>%      
    html_text()                             
}

get_stages <- function(html){
  read_html(html) %>% 
    html_nodes('.stages select' ) %>%      
    html_text()                             
}

get_teams <- function(html){
  read_html(html) %>% 
    html_nodes('.teamsOverview a' ) %>%      
    html_text()                             
}
get_flag <- function(html){
  read_html(html) %>% 
    html_nodes('.flag' )%>%
    html_attr("class")
}


find.index <- function(x, value, degree) {
  r <- rle(x)
  match.pos <- which(r$value == value)
  if (length(match.pos) < degree) {
    return(NA)  
  }
  return(sum(r$length[1:(match.pos[degree]-1)])+1)
}


PCS_Points<-data.frame()
PCS_Points_sub_bind<-data.frame()

for (i in seq(0, 3000, 100)){
  for (year in 2007:2020){
    URL_points <- paste0("https://www.procyclingstats.com/rankings.php?date=", year, "-12-31&nation=&age=&zage=&page=smallerorequal&team=&offset=",i,"&filter=Filter&id=97&prev_id=prev&younger=&older=&limit=200&morefilters=0"
    )
  tryCatch({
    PCS_Points_all<-get_all(URL_points)
    flags<-get_flag(URL_points)
    
    PCS_Points_sub <- as.data.frame(matrix(PCS_Points_all, ncol = 6, byrow = TRUE))
    
    colnames(PCS_Points_sub) <- as.character(unlist(PCS_Points_sub[1,]))
    colnames(PCS_Points_sub)[6]<-'points'
    PCS_Points_sub=PCS_Points_sub[-1,]
    PCS_Points_sub<-add_column(PCS_Points_sub, Country=flags, .after = 6)
    
    PCS_Points_sub[,4]<-tolower(PCS_Points_sub[,4])
    PCS_Points_sub[,4]<-str_trim(PCS_Points_sub[,4])
    PCS_Points_sub[,6]<-as.numeric(as.character(PCS_Points_sub[,6]))
    PCS_Points_sub[,7]<-str_trim(gsub("flag", "", PCS_Points_sub[,7]))
    
    PCS_Points_sub$year <- year
    
    PCS_Points_sub_bind<-rbind(PCS_Points_sub_bind,PCS_Points_sub)
    
  },error = function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
  }}

PCS_Points <- PCS_Points_sub_bind

calculate_points_as_neopro <- function(x) {
  py1<-paste0("Pnts-",x[2])
  py2<-paste0("Pnts-",(as.numeric(x[2])+1))
  x["Points as neopro"]<-as.numeric(x[py1])+as.numeric(x[py2])
}

load("updated_neopros_bram.Rdata")

colnames(proYear_df)<-c("Rider","Pro_year","Age_neopro","Division")
proYear_df$Rider<-tolower(proYear_df$Rider)
proYear_df$Rider<-str_trim(proYear_df$Rider)
proYear_df<-proYear_df %>% filter(Age_neopro < 25)

PCS_Points$Rider<-tolower(PCS_Points$Rider)
PCS_Points$Rider<-str_trim(PCS_Points$Rider)

PCS_Points_pro<-merge(x=proYear_df,y=PCS_Points,x.by="Rider",y.by="Rider",all.x=TRUE,all.y=FALSE)

PCS_Points_first2 <- PCS_Points_pro %>% filter(Pro_year == year | Pro_year+1 == year) %>% group_by(Rider) %>% 
  summarise(Points_as_neopro = sum(points))

PCS_Points_save<-merge(x=proYear_df,y=PCS_Points_first2,x.by="Rider",y.by="Rider",all.x=TRUE,all.y=FALSE)

PCS_Points_save$Points_as_neopro <- ifelse(PCS_Points_save$Pro_year<=2019&is.na(PCS_Points_save$Points_as_neopro),0,PCS_Points_save$Points_as_neopro)

save(PCS_Points_save, file = 'PCS_Points_neopro_updatedBram.Rdata')


load('PCS_Points_neopro_updatedBram.Rdata')

plot(PCS_Points_save %>% group_by(Pro_year) %>% summarise(Average_points = mean(Points_as_neopro)), type = 'b')
