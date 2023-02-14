setwd('C:\\Users\\banjanss\\Documents\\CyclingScouting\\Thesis_finaal')

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

library(dplyr)

library(bnstruct)

if (!require('bnstruct')) { 
  install.packages('bnstruct')
  require("bnstruct") 
}



U23_races<-c(
  
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
  ,'olympias-tour'
  ,'tour-alsace'
  ,'tour-de-normadie'
  ,'eschborn-frankfurt-u23'
  ,'zlm-tour'
  ,'tour-de-berlin'
  ,'paris-arras-tour'
  ,'tour-des-pays-de-savoie'
)

MJ_races<-c(
  'omloop-der-vlaamse-gewesten2'
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
  
)
races_list<-list(U23_races,MJ_races)

stageraces <-c(
               'giro-ciclistico-d-italia'
               ,'tour-de-l-avenir'
               ,'giro-ciclistico-della-valle-d-aosta-mont-blanc'
               ,'le-triptyque-des-monts-et-chateaux'
               ,'ronde-de-l-isard'
              ,'olympias-tour'
              ,'tour-alsace'
              ,'tour-de-normadie'
              ,'tour-de-berlin'
              ,'paris-arras-tour'
              ,'tour-des-pays-de-savoie'
               
               ,'tour-du-valromey'
               ,'liege-la-gleize'
               ,'course-de-la-paix-junior'
               ,'grand-prix-ruebliland'
               ,'keizer-der-juniores'
               ,'la-coupe-du-president-de-la-ville-de-grudziadz'
               ,'trofeo-karlsberg'
               ,'sint-martinusprijs-kontich'
               ,'driedaagse-van-axel'
               ,'tour-de-l-abitibi'
               ,'tour-du-pays-de-vaud'
               ,'giro-internazionale-della-lunigiana'
               ,'tour-de-l-abitibi'
          ,'int-junioren-rundfahrt-niedersachsen'
)


oneday_races <- c(
  #U23 races
  'kattekoers'
  ,'tour-des-flandres-espoirs'
  ,'liege-bastogne-liege-u23'
  ,'coppa-della-pace-trofeo-flli-anelli'
  ,'gp-palio-del-recioto'
  ,'gp-capodarco'
  ,'gran-premio-della-liberazione'
  ,'gp-industrie-del-marmo'
  ,'gp-di-poggiana'
  ,'piccolo-giro-di-lombardia'
  ,'paris-roubaix-espoirs'
  ,'paris-tours-u23'
  ,'ruota-d-oro-gp-festa-del-perdono'
  ,'gp-industria-e-commercio2'
  ,'trofeo-piva'
  ,'circuito-belvedere'
  ,'gp-di-poggiana'
  ,'gp-industria-e-commercio2'
  ,'european-championships-itt-u23'
  ,'european-championships'
  ,'world-championships-itt-u23'
  ,'world-championships-u23'
  ,'eschborn-frankfurt-u23'
  ,'zlm-tour'
  
  #Junior Races
  ,'omloop-der-vlaamse-gewesten2'
  ,'bernaudeau-junior'
  ,'gp-general-patton'
  ,'gp-dell-arno'
  ,'paris-roubaix-juniors'
  ,'ronde-van-vlaanderen-juniores'
  ,'trofeo-buffoni'
  ,'trofeo-comune-di-vertova'
  ,'trofeo-emilio-paganessi'
  ,'le-trophee-centre-morbihan'
  ,'chrono-des-nations-les-herbiers-vendee-mj'
  ,'european-championship-mj'
  ,'uci-world-championships-itt-mj'
  ,'uci-world-championships-mj'
  
)

oneday_races_cobbles <- c(
  
  'omloop-der-vlaamse-gewesten2'
  ,'paris-roubaix-juniors'
  ,'ronde-van-vlaanderen-juniores'
)

oneday_races_MJ <- c(
  'bernaudeau-junior'
  ,'gp-general-patton'
  ,'gp-dell-arno'
  ,'trofeo-buffoni'
  ,'trofeo-comune-di-vertova'
  ,'trofeo-emilio-paganessi'
  ,'le-trophee-centre-morbihan'
  ,'european-championship-mj'
  ,'uci-world-championships-mj'
)


oneday_races_Italy_U23 <- c(
  'liege-bastogne-liege-u23'
  ,'coppa-della-pace-trofeo-flli-anelli'
  ,'gp-palio-del-recioto'
  ,'gp-capodarco'
  ,'gran-premio-della-liberazione'
  ,'gp-industrie-del-marmo'
  ,'gp-di-poggiana'
  ,'piccolo-giro-di-lombardia'
  ,'ruota-d-oro-gp-festa-del-perdono'
  ,'gp-industria-e-commercio2'
  ,'trofeo-piva'
  ,'circuito-belvedere'
  ,'gp-di-poggiana'
  ,'gp-industria-e-commercio2'
  ,'world-championships-u23'
  ,'european-championships'
)

stageraces_strong <-c(
  'le-triptyque-des-monts-et-chateaux'
  ,'olympias-tour'
  ,'tour-de-normadie'
  ,'tour-de-berlin'
  ,'paris-arras-tour'
  ,'kattekoers'
  ,'tour-des-flandres-espoirs'
  ,'paris-roubaix-espoirs'
  ,'paris-tours-u23'
  ,'eschborn-frankfurt-u23'
  ,'zlm-tour'
)
stageraces_climb <-c(
  'ronde-de-l-isard'
  ,'tour-alsace'
  
)

stageraces_MJ <-c(
'tour-du-valromey'
,'liege-la-gleize'
,'course-de-la-paix-junior'
,'grand-prix-ruebliland'
,'keizer-der-juniores'
,'la-coupe-du-president-de-la-ville-de-grudziadz'
,'trofeo-karlsberg'
,'sint-martinusprijs-kontich'
,'driedaagse-van-axel'
,'tour-de-l-abitibi'
,'tour-du-pays-de-vaud'
,'giro-internazionale-della-lunigiana'
,'tour-de-l-abitibi'
,'int-junioren-rundfahrt-niedersachsen')

ITT_races<-c(
  'uci-world-championships-itt-mj'
  ,'chrono-des-nations-les-herbiers-vendee-mj'
  ,'world-championships-itt-u23'
  ,'european-championships-itt-u23'
)
big_tours<-c('tour-de-l-avenir'
             ,'giro-ciclistico-d-italia'
             ,'giro-ciclistico-della-valle-d-aosta-mont-blanc'
             ,'tour-des-pays-de-savoie')

Impute_list<-list(big_tours,oneday_races_cobbles,oneday_races_MJ,oneday_races_Italy_U23,stageraces_strong,stageraces_climb,stageraces_MJ,ITT_races)




load("allYouthResults.rdata")
load("PCS_Points_neopro_updatedBram.rdata")

updated_BRAM <- df %>% select(-contains("This"))


#load("PCS_Climbers_pro.Rdata")
#load("H:/Masterproef/PCS_Wins_pro.Rdata")

merged[,1]<-tolower(merged[,1])
merged[,1]<-str_trim(merged[,1])
merge_pros <- merge(x=PCS_Points_save[,c("Rider","Pro_year","Points_as_neopro","Age_neopro","Division"),drop=TRUE],y=merged,by="Rider",all.x=TRUE,all.y=FALSE)

checker <- merge_pros[,-c(1:5)]

results <- merge_pros[rowSums(is.na(checker))!=ncol(checker),]

#merged<-merge(x=PCS_Wins_pro[,c("Rider","Pro_year","Wins_as_neopro","Age neopro","Division"),drop=TRUE],y=merged,x.by="Rider",y.by="Rider",all.x=TRUE,all.y=FALSE)
#merged<-merge(x=PCS_Climbers_pro[,c("Rider","Pro_year","Climbers_as_neopro","Age neopro","Division"),drop=TRUE],y=merged,x.by="Rider",y.by="Rider",all.x=TRUE,all.y=FALSE)


# cols_impute<-intersect(grep("giro-ciclistico-d-italia",colnames(merged)),grep("GC",colnames(merged)))
# cols_use<-intersect(grep("tour-de-l-avenir",colnames(merged)),grep("GC",colnames(merged)))
# cols_use<-as.integer(unlist(cols_use, use.names = FALSE))
# cols_impute<-as.integer(unlist(cols_impute, use.names = FALSE))
# 

merge_pros$numberResults <- rowSums(!is.na(checker))

evolution_number_results <- merge_pros %>% group_by(Pro_year) %>% summarise(avg_results = mean(numberResults))
plot(evolution_number_results, type = 'b')

merge_pros$nrbAbandons <- rowSums(merge_pros[,grep('abandon', colnames(merge_pros), value = TRUE)], na.rm = TRUE)
merge_pros$numberResults <- merge_pros$numberResults - merge_pros$nrbAbandons
merge_pros$abandonRatio <- merge_pros$nrbAbandons/merge_pros$numberResults 

merge_pros <- filter(merge_pros, merge_pros$Pro_year>=2010) #Based on enough youth results to be representative

#Some small data quality issues
duplicates <- merge_pros[duplicated(merge_pros$Rider),]
entire_set <- data.frame()
for (rider in unique(duplicates$Rider)){
  df <- merge_pros[merge_pros$Rider==rider,]
  df <- df[,colSums(is.na(df))<nrow(df)]
  assign(rider, df)
}

#Impossible to say certain which result is most correct when doubt --> just keep first observation
merge_pros<-merge_pros[!duplicated(merge_pros$Rider), ]

Basetable <- merge_pros[,c(1:3,(ncol(merge_pros)-2):ncol(merge_pros))]


#Oude code Matthijs, nog niet direct weggooien
#Basetable<-data.frame(merged[,c(1,2,3,4,5,6)],stringsAsFactors=FALSE)
# H<-1.5*IQR(Basetable$Points_as_neopro)
# qnt <- quantile(Basetable$Points_as_neopro, probs=c(.25, .75), na.rm = T)
# caps <- quantile(Basetable$Points_as_neopro, probs=c(.05, .95), na.rm = T)
# Basetable$Points_as_neopro[Basetable$Points_as_neopro < (qnt[1] - H)]  <- caps[1]
# Basetable$Points_as_neopro[Basetable$Points_as_neopro >(qnt[2] + H)] <- caps[2]


#Basetable$Division<-as.factor(Basetable$Division)
#Basetable$Country<-as.factor(Basetable$Country)

#Problem: currently uses 9999 as indicator of no participation + abandon columns added to list of columns

#Drop abandon columns
merge_pros[,grep('abandon', colnames(merge_pros), value = TRUE)] <- NULL


Basetable_stageraces<-function(race,df,basetable){
  cols<-intersect(grep("GC",colnames(df)),grep(race,colnames(df)))
  name1<-paste(race, " GC best result")
  basetable[name1]<-apply(df[,cols],1,function(x) min(x, na.rm = TRUE))
  
  cols<-intersect(grep("TimeDiff",colnames(df)),grep(race,colnames(df)))
  name_TimeDiff<-paste(race, " minimum TimeDiff")
  basetable[name_TimeDiff]<-apply(df[,cols],1,function(x) min(x, na.rm = TRUE))
  
  cols<-intersect(grep("stage",colnames(df)),grep(race,colnames(df)))
  name<-paste(race, " stage victory")
  basetable[name]<-apply(df[,cols]==1,1,function(x) sum(x, na.rm = TRUE))
  name<-paste(race, " stage best result")
  name2<-paste(race, " participation")
  basetable[name]<-apply(df[,cols],1,function(x) min(x, na.rm = TRUE))
  basetable[name2]<-ifelse(is.infinite(basetable[,name1]),0,1)
  basetable[is.infinite(basetable[,name1]), c(name, name1, name_TimeDiff)]<-NA
  
  return(basetable)
}

Basetable_oneday<-function(race,df,basetable){
  cols<-setdiff(grep(race,colnames(df)),grep("TimeDiff",colnames(df)))
  name1<-paste(race, " best result")
  name2<-paste(race, " participation")
  basetable[name1]<-apply(df[,cols],1,function(x) min(x, na.rm = TRUE))
  
  cols<-intersect(grep("TimeDiff",colnames(df)),grep(race,colnames(df)))
  name_TimeDiff<-paste(race, " minimum TimeDiff")
  basetable[name_TimeDiff]<-apply(df[,cols],1,function(x) min(x, na.rm = TRUE))
  
  basetable[name2]<-ifelse(is.infinite(basetable[,name1]),0,1)
  basetable[is.infinite(basetable[,name1]), c(name1, name_TimeDiff)]<-NA
  return(basetable)
}
for( race in stageraces){
  Basetable<-Basetable_stageraces(race,merge_pros,Basetable)
}

for(race in oneday_races){
  Basetable<-Basetable_oneday(race,merge_pros,Basetable)
}



cols<-grep(" participation",colnames(Basetable))
Basetable[,cols]<-lapply(Basetable[,cols],as.factor)

tussenresultaat <- Basetable
              

for(races in races_list){
  cols_list<-NULL
  for(race in races){
    # cols1<-intersect(grep(race,colnames(Basetable)),grep("best",colnames(Basetable)))
    # cols2<-intersect(grep(race,colnames(Basetable)),grep("TimeDiff",colnames(Basetable)))
    cols<-grep(race,colnames(merged))
    cols_list<-c(cols_list,cols)
    (cols_list<-unique(cols_list))
  }
  
  
  merged_divided<-merged[,cols_list]
  merged_divided_results<-Filter(is.numeric, merged_divided)
  merged_divided_results<-add_column(merged_divided_results, Rider=merged$Rider, .before = 1)
  
 
  difs <- setdiff(races,U23_races)
  con<-is_empty(difs)
  
  if(con){
    Number_of_top5<-apply((merged_divided_results<=5&merged_divided_results>3),1,sum)
    Number_of_podiums<-apply((merged_divided_results<=3&merged_divided_results>1),1,sum)
    Number_of_victories<-apply(merged_divided_results==1,1,sum)
    started_races<-apply(merged_divided_results<9999,1,sum)

    Basetable<-add_column(Basetable, victory_ratio_U23=Number_of_victories/started_races, .after = 6)
    Basetable<-add_column(Basetable, podium_ratio_U23=Number_of_podiums/started_races, .after = 7)
    Basetable<-add_column(Basetable, top5_ratio_U23=Number_of_top5/started_races, .after = 8)
    Basetable<-add_column(Basetable, victories_U23=Number_of_victories, .after = 9)
  }
  else{
    Number_of_top5<-apply((merged_divided_results<=5&merged_divided_results>3),1,sum)
    Number_of_podiums<-apply((merged_divided_results<=3&merged_divided_results>1),1,sum)
    Number_of_victories<-apply(merged_divided_results==1,1,sum)
    started_races<-apply(merged_divided_results<9999,1,sum)

    Basetable<-add_column(Basetable, victory_ratio_MJ=Number_of_victories/started_races, .after = 6)
    Basetable<-add_column(Basetable, podium_ratio_MJ=Number_of_podiums/started_races, .after = 7)
    Basetable<-add_column(Basetable, top5_ratio_MJ=Number_of_top5/started_races, .after = 8)
    Basetable<-add_column(Basetable, victories_MJ=Number_of_victories, .after = 9)

  }
}

Basetable<-filter(Basetable,!is.na(Basetable[,10]))
Basetable<-filter(Basetable,!is.na(Basetable[,7]))



merged_results<-Filter(is.numeric, merged[,7:ncol(merged)])

merged_results<-add_column(merged_results, Rider=merged$Rider, .before = 1)


Number_of_top5<-apply((merged_results<=5&merged_results>3),1,sum)
Number_of_podiums<-apply((merged_results<=3&merged_results>1),1,sum)
Number_of_victories<-apply(merged_results==1,1,sum)
started_races<-apply(merged_results<9999,1,sum)
avg_result<-apply(merged_results<9999,1,mean)
df<-data.frame(Basetable$Rider,Number_of_top5,Number_of_podiums,Number_of_victories,started_races,avg_result)
df["victory ratio"]<-Number_of_victories/started_races
df["podium ratio"]<-Number_of_podiums/started_races
df["top 5 ratio"]<-Number_of_podiums/started_races


Basetable<-add_column(Basetable, victory_ratio=Number_of_victories/started_races, .after = 6)
Basetable<-add_column(Basetable, podium_ratio=Number_of_podiums/started_races, .after = 7)
Basetable<-add_column(Basetable, top5_ratio=Number_of_top5/started_races, .after = 8)
# Basetable<-add_column(Basetable, avg_result, .after = 8)
# Basetable<-add_column(Basetable, started_races, .after = 8)
Basetable<-filter(Basetable,!is.na(Basetable$`victory_ratio`))







for(impute_races in Impute_list){
  cols_list<-NULL
  for(race in impute_races){
     cols1<-intersect(grep(race,colnames(Basetable)),grep("best",colnames(Basetable)))
     cols2<-intersect(grep(race,colnames(Basetable)),grep("TimeDiff",colnames(Basetable)))
    
    cols_list<-c(cols_list,cols1,cols2)
    (cols_list<-unique(cols_list))
  }
 # num_cols<-lapply(Basetable,is.numeric)
 # cols<-unlist(num_cols)
 # num_base<-Basetable[,cols]
  matrix<-as.matrix(Basetable[,cols_list])
  # colnames(matrix)<-gsub(" ","_",colnames(matrix))
  # colnames(matrix)<-gsub("-","_",colnames(matrix))
  cols_factor<-grep("participation",colnames(matrix))
  test_knn<-knn.impute(matrix, k = 1,cat.var =cols_factor ,to.impute = 1:nrow(Basetable), using = 1:nrow(Basetable))
  Basetable[,cols_list]<-test_knn
  # test_mice<-mice(matrix, m = 5, method = "pmm", maxit = 50, seed = 500)
  # matrix<-complete(test_mice)
  # matrix<-add_column(matrix,Basetable[,1],.before=1)
  check<-Basetable[Basetable[,1] %in% c("bernal egan","carapaz richard","ackermann pascal","jakobsen fabio","sagan peter","degenkolb john", "pinot thibaut", "alaphilippe julian","frame alex"),]
}

check<-Basetable[Basetable[,1] %in% c("halvorsen kristoffer"),]
cols<-grep(" participation",colnames(Basetable))
Basetable[,cols]<-lapply(Basetable[,cols],as.factor)


sum(is.na(Basetable))

sum(!is.na(Basetable))

colnames(Basetable)[colSums(is.na(Basetable)) > 0]
Basetable[is.na(Basetable)]<-0
save(Basetable, file = '~/Unief/Masterproef/R/Basetable.Rdata')





