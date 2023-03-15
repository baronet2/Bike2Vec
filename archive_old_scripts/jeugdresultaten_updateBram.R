rm(list=ls())
if (!require('pacman')) install.packages('pacman'); require('pacman',character.only=TRUE,quietly=TRUE)

# General-purpose data wrangling
p_load(tidyverse)  

# Parsing of HTML/XML files  
p_load(rvest)    

# String manipulation
p_load(stringr)   

# Verbose regular expressions
p_load(rebus)     

# Eases DateTime manipulation
p_load(lubridate)

# Column splits enabled
p_load(reshape2)

p_load(varhandle)

p_load(xlsx)

p_load(tidyr)

p_load(chron) 

p_load(rapportools)

p_load(tokenizers)

p_load(stringr)


# 
races <- c(
            #U23 races
            'kattekoers'
          ,'giro-ciclistico-d-italia'
          ,'circuito-belvedere'
          ,'gp-di-poggiana'
          ,'gp-industria-e-commercio2'
          ,'tour-des-flandres-espoirs'
          ,'liege-bastogne-liege-u23'
           ,'tour-de-l-avenir'
          ,'la-cote-picarde-nations-cup'
          ,'coppa-della-pace-trofeo-flli-anelli'
          ,'gp-palio-del-recioto'
          ,'giro-ciclistico-della-valle-d-aosta-mont-blanc'

          ,'gp-capodarco'
          ,'gran-premio-della-liberazione'
          ,'gp-industrie-del-marmo'
          ,'gp-di-poggiana'
          ,'piccolo-giro-di-lombardia'
          ,'le-triptyque-des-monts-et-chateaux'
          ,'paris-roubaix-espoirs'
          ,'paris-tours-u23'
          ,'ronde-de-l-isard'
          ,'ruota-d-oro-gp-festa-del-perdono'
          ,'gp-industria-e-commercio2'
          ,'trofeo-piva'
          ,'european-championships-itt-u23'
          ,'european-championships'
          ,'world-championships-itt-u23'
           ,'world-championships-u23'

          #Junior Races
          ,'omloop-der-vlaamse-gewesten2'
          ,'tour-du-valromey'
          ,'liege-la-gleize'
          ,'bernaudeau-junior'
          ,'course-de-la-paix-junior'
          ,'gp-general-patton'
          ,'grand-prix-ruebliland'
          ,'gp-dell-arno'
          ,'keizer-der-juniores'
          ,'la-coupe-du-president-de-la-ville-de-grudziadz'
          ,'trofeo-karlsberg'
          ,'omloop-der-vlaamse-gewesten2'
          ,'paris-roubaix-juniors'
          ,'ronde-van-vlaanderen-juniores'
          ,'sint-martinusprijs-kontich'
          ,'driedaagse-van-axel'
          ,'tour-de-l-abitibi'
          ,'tour-du-pays-de-vaud'
          ,'trofeo-buffoni'
          ,'trofeo-comune-di-vertova'
          ,'trofeo-emilio-paganessi'
          ,'le-trophee-centre-morbihan'
          ,'chrono-des-nations-les-herbiers-vendee-mj'
          ,'european-championship-mj'
          ,'giro-internazionale-della-lunigiana'
          ,'uci-world-championships-itt-mj'
          ,'uci-world-championships-mj'
          ,'int-junioren-rundfahrt-niedersachsen'


          #2.2 Races
          ,'olympias-tour'
          ,'tour-alsace'
          ,'tour-de-normadie'
          ,'eschborn-frankfurt-u23'
          ,'zlm-tour'
          ,'tour-de-berlin'
          ,'paris-arras-tour'
          ,'tour-des-pays-de-savoie'



 )


 #years<-2007
 get_all <- function(html){
   read_html(html) %>% 
     html_nodes('td , th' ) %>%      
     html_text()                             
 }
 
 get_stages <- function(html){
   read_html(html) %>% 
     html_nodes('.mb_w90' ) %>%      
     html_text()                             
 }
find.index <- function(x, value, degree) {
  r <- rle(x)
  match.pos <- which(r$value == value)
  if (length(match.pos) < degree) {
    return(NA)  
  }
  return(sum(r$length[1:(match.pos[degree]-1)])+1)
}


clean <- function(string){
  if(sum(str_count(string,":"))<2){
    string<-paste0("00:",string)
  }
  return(string)
}

check_rep <- function(string){
grepl("(.+)\\1+$",string)
}

final_all$bool <- lapply(final_all$Time, check_rep)

results <- list()
teller <- 1


years <- 2005:2020

for (year in years){ 
  counter<-0
  
  for (race in races) {
    tryCatch({  
      part1 <- paste0('https://www.procyclingstats.com/race/',race)  
      part2 <- paste0('/',year)
      
      url <- paste0(part1, part2)
      
      all <-get_all(url)
      #relevant <- dump[((find.index(dump, 'Clear filter', 1)) +1):((find.index(dump, 'View top-25', 1)) -1)]
      
      if(length(all)==0)
      {message<-paste0("empty:",url )
        print(message)}      else{
        
##For single races        

      if(is.na(find.index(all, 'Rnk', 2))){
        index<-find.index(all, '1', 1)
        final_all <- as.data.frame(matrix(all, ncol = index-1, byrow = TRUE))
        colnames(final_all) <- as.character(unlist(final_all[1,]))
        namecolresult<- paste0(race," ",year)
        
        colnames(final_all)[1]<-namecolresult
        final_all[,1]<-as.numeric(as.character(final_all[,1]))
        last_place<-max(final_all[,1], na.rm = TRUE)
        
        final_all$abandon <- ifelse(is.na(final_all[,1]),1,0)
        namecol_aban <- paste0(race,"-abandon",year)
        colnames(final_all)[ncol(final_all)]<-namecol_aban
        
        final_all[is.na(final_all[,1]),1]<-(last_place+1)
        final_all=final_all[-1,]
        final_all$Rider<-substr(as.character(final_all$Rider),1,(nchar(as.character(final_all$Rider))-nchar(as.character(final_all$Team))))
      
        if((is.empty(final_all$Time))|all(unlist(gregexpr("[[:digit:]]+", final_all$Time))<0)){
          if(counter>0){
            merged<- merge(x=final_all[,c("Rider",namecolresult
            ),drop=TRUE],y=merged, x.by="Rider",all.x =TRUE,all.y = TRUE)
            
          }
          else
          {
            merged<-final_all[,c("Rider",namecolresult),drop=TRUE]
          }
          counter=counter+1
        }
        else{
          final_all$Time<-ifelse(lapply(final_all$Time, check_rep), 
                                 substr(as.character(final_all$Time),1,nchar(as.character(final_all$Time))/2), 
                                 as.character(final_all$Time))
          final_all$Time <- gsub(',,', '', final_all$Time)
          previous_time<-"00:00:00"
          for(i in 2:length(final_all$Time)){
            if((str_count(final_all[i,"Time"],",")>0)|(str_count(final_all[i,"Time"],"-")>0)){
              final_all[i,"Time"]<-previous_time
            }
            else{
              previous_time<-final_all[i,"Time"]
            }
          }
          final_all$Time <- lapply(final_all$Time,function(x) clean(x))
          final_all[1,"Time"]<-"00:00:00"
          final_all$TimeDiff <- as.numeric(times(final_all$Time))*24*60*60
          namecol_timediff<- paste0(race,"-TimeDiff ",year)
          colnames(final_all)[ncol(final_all)]<-namecol_timediff
          
          
          if(counter>0){
            merged<- merge(x=final_all[,c("Rider",namecolresult,namecol_timediff,namecol_aban
            ),drop=TRUE],y=merged, x.by="Rider",all.x =TRUE,all.y = TRUE)
            
          }
          else
          {
            merged<-final_all[,c("Rider",namecolresult,namecol_timediff,namecol_aban),drop=TRUE]
          }
          counter=counter+1
          
        }
        
       
        
##For stage races
        }    else{
        Rnk<-find.index(all, 'Rnk', 2)
        Rnkend<-find.index(all, 'Rnk',3)
        if(is.na(Rnkend)){
          gc<-all[Rnk:length(all)]
          } else{
          gc <- all[Rnk:(Rnkend-1)]
        }
       
        indexgc<-find.index(gc, '1', 1)
        final_all <- as.data.frame(matrix(gc, ncol = indexgc-1, byrow = TRUE))
        colnames(final_all) <- as.character(unlist(final_all[1,]))
        namecolresult<- paste0(race,"-GC ",year)
        
        
        colnames(final_all)[1]<-namecolresult
        final_all=final_all[-1,]
        final_all[,1]<-as.numeric(as.character(final_all[,1]))
        last_place<-max(final_all[,1], na.rm = TRUE)
        
        final_all$abandon <- ifelse(is.na(final_all[,1]),1,0)
        namecol_aban <- paste0(race,"-abandon",year)
        colnames(final_all)[ncol(final_all)]<-namecol_aban
        
        final_all[is.na(final_all[,1]),1]<-(last_place+1)
        final_all$Rider<-substr(as.character(final_all$Rider),1,(nchar(as.character(final_all$Rider))-nchar(as.character(final_all$Team))))
        if((all(is.empty(final_all$Time)))|all(unlist(gregexpr("[[:digit:]]+", final_all$Time))<0)){
          if(counter>0){
            merged<- merge(x=final_all[,c("Rider",namecolresult
            ),drop=TRUE],y=merged, x.by="Rider",all.x =TRUE,all.y = TRUE)
            
          }  else {
            merged<-final_all[,c("Rider",namecolresult),drop=TRUE]
          }
          counter=counter+1
        }   else{
          final_all$Time<-ifelse(lapply(final_all$Time, check_rep), 
                                 substr(as.character(final_all$Time),1,nchar(as.character(final_all$Time))/2), 
                                 as.character(final_all$Time))
          final_all$Time <- gsub(',,', '', final_all$Time)
          previous_time<-"00:00:00"
          for(i in 2:length(final_all$Time)){
            if((str_count(final_all[i,"Time"],",")>0)|(str_count(final_all[i,"Time"],"-")>0)){
              final_all[i,"Time"]<-previous_time
            }
            else{
              previous_time<-final_all[i,"Time"]
            }
          }
          final_all$Time <- lapply(final_all$Time,function(x) clean(x))
          final_all[1,"Time"]<-"00:00:00"
          final_all$TimeDiff <- as.numeric(times(final_all$Time))*24*60*60
          namecol_timediff<- paste0(race,"-TimeDiff ",year)
          colnames(final_all)[ncol(final_all)]<-namecol_timediff
          
          
          if(counter>0){
            merged<- merge(x=final_all[,c("Rider",namecolresult,namecol_timediff,namecol_aban
            ),drop=TRUE],y=merged, x.by="Rider",all.x =TRUE,all.y = TRUE)
            
          }  else  {merged<-final_all[,c("Rider",namecolresult,namecol_timediff,namecol_aban),drop=TRUE]}
          counter=counter+1
          
        }
        
        stages<-get_stages(paste0(url,'/gc/stages'))
        stageNumberList <- regmatches(stages, gregexpr("[[:digit:]]+", stages))
        stageNumbers<-as.character(unlist(stageNumberList))
        
        if(any(grepl("Prologue", stages))){
          url_stage <- paste0(url,"/prologue")
          stage_all<-get_all(url_stage)
          
          if(length(stage_all)==0)
          {message<-paste0("empty:",url_stage )
          print(message)}
          
          else{

          stage_Rnk<-find.index(stage_all, 'Rnk', 1)
          stage_Rnkend<-find.index(stage_all, 'Rnk',2)

          if(is.na(stage_Rnkend)){
            stage_results <- stage_all[(stage_Rnk-1):length(stage_all)]
          }          else{
            stage_results <- stage_all[(stage_Rnk-1):(stage_Rnkend-1)]          }
          
          indexstage<-find.index(stage_results, '1', 1)
          final_stage <- as.data.frame(matrix(stage_results, ncol = indexstage-1, byrow = TRUE))


          colnames(final_stage) <- as.character(unlist(final_stage[1,]))
          namecolresult<- paste0(race," ",year,"prologue ")

          colnames(final_stage)[1]<-namecolresult
          final_stage=final_stage[-1,]
          final_stage[,1]<-as.numeric(as.character(final_stage[,1]))
          last_place<-max(final_stage[,1], na.rm = TRUE)
          final_stage[is.na(final_stage[,1]),1]<-(last_place+1)
          final_stage$Rider<-substr(as.character(final_stage$Rider),1,(nchar(as.character(final_stage$Rider))-nchar(as.character(final_stage$Team))))

          if(counter>0){
            merged<- merge(x=final_stage[,c("Rider",namecolresult),drop=TRUE],y=merged, x.by="Rider",all.x =TRUE,all.y = TRUE)

          } else  {merged<-final_stage[,c("Rider",namecolresult),drop=TRUE]}
          counter=counter+1
        }
        
        }
        
        if(sum(duplicated(stageNumbers))>0){
          duplicated(stageNumbers)
          stageNumbers[duplicated(stageNumbers)]
          
          index_a<-match(stageNumbers[duplicated(stageNumbers)],stageNumbers)
          index_b<-index_a+1
          stageNumbers[index_a]<-paste0(stageNumbers[index_a],"a")
          stageNumbers[index_b]<-paste0(stageNumbers[index_b],"b")
        }
        
        for(stage in stageNumbers){
          
          
          
          url_stage <- paste0(url,"/stage-",stage)
          stage_all<-get_all(url_stage)
          
          if(length(stage_all)==0)
          {message<-paste0("empty:",url_stage )
          print(message)
          } else{ 
            stage_Rnk<-find.index(stage_all, 'Rnk', 1)
            stage_Rnkend<-find.index(stage_all, 'Rnk',2)
          
          
            if(is.na(stage_Rnkend)){
              stage_results <- stage_all[(stage_Rnk-1):length(stage_all)]
            } else{
                stage_results <- stage_all[(stage_Rnk-1):(stage_Rnkend-1)]}
            
            indexstage<-find.index(stage_results, '1', 1)
            final_stage <- as.data.frame(matrix(stage_results, ncol = indexstage-1, byrow = TRUE))
            colnames(final_stage) <- as.character(unlist(final_stage[1,]))
            namecolresult<- paste0(race," ",year," stage ",stage)
            colnames(final_stage)[1]<-namecolresult
            final_stage=final_stage[-1,]
            final_stage[,1]<-as.numeric(as.character(final_stage[,1]))
            last_place<-max(final_stage[,1], na.rm = TRUE)
            final_stage[is.na(final_stage[,1]),1]<-(last_place+1)
            final_stage$Rider<-substr(as.character(final_stage$Rider),1,(nchar(as.character(final_stage$Rider))-nchar(as.character(final_stage$Team))))
            
          if(counter>0){
            merged<- merge(x=final_stage[,c("Rider",namecolresult),drop=TRUE],y=merged, x.by="Rider",all.x =TRUE,all.y = TRUE)
            
          } else{merged<-final_stage[,c("Rider",namecolresult),drop=TRUE]}
          counter=counter+1
        }
        }
          
      }

       teller <- teller + 1
      }
       }, error = function(e){cat("ERROR :",conditionMessage(e), "\n" , url,"\n" , race,"\n")}
      )
  }
  
  indicators_riders <-  sapply(merged$Rider, function(x) count_words(x))>=2
  merged <- merged[indicators_riders,]
  merged <- merged[!grepl(':',merged$Rider),]
  merged <- merged[merged$Rider!='Czech Re'& merged$Rider!='Great B',]
  
  numbers <- grepl("[[:digit:]]",merged$Rider) 
  merged<-filter(merged, !numbers)
  
  merged[,1]<-tolower(merged[,1])
  merged[,1]<-str_trim(merged[,1])
  
  if(year==2005){
    merged2005<-merged
  }
  if(year==2006){
    merged2006<-merged
  }
  if(year==2007){
    merged2007<-merged
  }
  if(year==2008){
    merged2008<-merged
  }
  if(year==2009){
    merged2009<-merged
  }
  if(year==2010){
    merged2010<-merged
  }
  if(year==2011){
    merged2011<-merged
  }
  if(year==2012){
    merged2012<-merged
  }
  if(year==2013){
    merged2013<-merged
  }
  if(year==2014){
    merged2014<-merged
  }
  if(year==2015){
    merged2015<-merged
  }
  if(year==2016){
    merged2016<-merged
  }
  if(year==2017){
    merged2017<-merged
  }
  if(year==2018){
    merged2018<-merged
  }
  if(year==2019){
    merged2019<-merged
  }
  if(year==2020){
    merged2020<-merged
  }
  

}




merged<-merged2005
merged<- merge(x=merged,y=merged2006, x.by="Rider",all.x =TRUE,all.y = TRUE)
merged<- merge(x=merged,y=merged2007, x.by="Rider",all.x =TRUE,all.y = TRUE)
merged<- merge(x=merged,y=merged2008, x.by="Rider",all.x =TRUE,all.y = TRUE)
merged<- merge(x=merged,y=merged2009, x.by="Rider",all.x =TRUE,all.y = TRUE)
merged<- merge(x=merged,y=merged2010, x.by="Rider",all.x =TRUE,all.y = TRUE)
merged<- merge(x=merged,y=merged2011, x.by="Rider",all.x =TRUE,all.y = TRUE)
merged<- merge(x=merged,y=merged2012, x.by="Rider",all.x =TRUE,all.y = TRUE)
merged<- merge(x=merged,y=merged2013, x.by="Rider",all.x =TRUE,all.y = TRUE)
merged<- merge(x=merged,y=merged2014, x.by="Rider",all.x =TRUE,all.y = TRUE)
merged<- merge(x=merged,y=merged2015, x.by="Rider",all.x =TRUE,all.y = TRUE)
merged<- merge(x=merged,y=merged2016, x.by="Rider",all.x =TRUE,all.y = TRUE)
merged<- merge(x=merged,y=merged2017, x.by="Rider",all.x =TRUE,all.y = TRUE)
merged<- merge(x=merged,y=merged2018, x.by="Rider",all.x =TRUE,all.y = TRUE)
merged<- merge(x=merged,y=merged2019, x.by="Rider",all.x =TRUE,all.y = TRUE)
merged<- merge(x=merged,y=merged2020, x.by="Rider",all.x =TRUE,all.y = TRUE)


#Hierna checken en desnoods verder filteren

save(merged, file = 'C:\\Users\\banjanss\\Documents\\Cycling Scouting\\allYouthResults.Rdata')
