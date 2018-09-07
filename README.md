# NBA-prediction
Naive Bayes, Logistic Regression, Random Forest

library(MASS)
library(klaR)
library(dplyr)
library(randomForest)

# funtion 
# 1) Host team or Guest team function: help distinguish the host or guest team
hostorguest<-function(sampledata){
  sampledata$matchup=as.character(sampledata$matchup)
  for (i in 1:nrow(sampledata)){
    if (grepl("vs.",sampledata$matchup[i])){
      sampledata$hog[i]=0 #Host
      sampledata$matchup[i]=substr(sampledata$matchup[i],9,11)
    } 
    else{
      sampledata$hog[i]=1 #Guest
      sampledata$matchup[i]=substr(sampledata$matchup[i],7,9)
    }
  }
  sampledata$matchup=as.factor(sampledata$matchup)
  return(sampledata)
}

# 2) Naive Bayes Function
nb<-function(train,test){
  nbmodel <- NaiveBayes(is_host_win~n_all_star_guest+n_all_star_host+cum_pts_guest+cum_fgm_guest+cum_fga_guest+cum_x3pm_guest+cum_x3pa_guest+cum_ftm_guest+cum_fta_guest+cum_oreb_guest+cum_dreb_guest+cum_ast_guest+cum_stl_guest+cum_blk_guest+cum_tov_guest+cum_pts_host+cum_fgm_host+cum_fga_host+cum_x3pm_host+cum_x3pa_host+cum_ftm_host+cum_fta_host+cum_oreb_host+cum_dreb_host+cum_ast_host+cum_stl_host+cum_blk_host+cum_tov_host,data=train)
  fittedresults=predict(nbmodel,newdata=test,type='response')
  Error=mean(fittedresults$class!=test$is_host_win)
  return(Error)
}

# 3) Logistic Regression Function
logistic<-function(train,test){
  logisticmodel <- glm(is_host_win~n_all_star_guest+n_all_star_host+cum_pts_guest+cum_fgm_guest+cum_fga_guest+cum_x3pm_guest+cum_x3pa_guest+cum_ftm_guest+cum_fta_guest+cum_oreb_guest+cum_dreb_guest+cum_ast_guest+cum_stl_guest+cum_blk_guest+cum_tov_guest+cum_pts_host+cum_fgm_host+cum_fga_host+cum_x3pm_host+cum_x3pa_host+cum_ftm_host+cum_fta_host+cum_oreb_host+cum_dreb_host+cum_ast_host+cum_stl_host+cum_blk_host+cum_tov_host,family=binomial(link='logit'),data=train)
  Fittedresults=predict(logisticmodel,newdata=test,type='response')
  fittedresults=ifelse(Fittedresults>0.5,1,0)
  fittedresults[fittedresults==1]="W"
  fittedresults[fittedresults==0]="L"
  Error=mean(fittedresults!=test$is_host_win)
  return(Error)
}

# 4) Random Forest Function
# select mtry & ntree
# set.seed(10)
# for (i in 1:(length(names(train))-1)){
#   mtry_fit=randomForest(is_host_win~n_all_star_guest+n_all_star_host+cum_pts_guest+cum_fgm_guest+cum_fga_guest+cum_x3pm_guest+cum_x3pa_guest+cum_ftm_guest+cum_fta_guest+cum_oreb_guest+cum_dreb_guest+cum_ast_guest+cum_stl_guest+cum_blk_guest+cum_tov_guest+cum_pts_host+cum_fgm_host+cum_fga_host+cum_x3pm_host+cum_x3pa_host+cum_ftm_host+cum_fta_host+cum_oreb_host+cum_dreb_host+cum_ast_host+cum_stl_host+cum_blk_host+cum_tov_host,data=train,mtry=i)
#   err=mean(mtry_fit$err.rate)
#   print(err)
# }
# set.seed(10)
# ntree_fit=randomForest(is_host_win~n_all_star_guest+n_all_star_host+cum_pts_guest+cum_fgm_guest+cum_fga_guest+cum_x3pm_guest+cum_x3pa_guest+cum_ftm_guest+cum_fta_guest+cum_oreb_guest+cum_dreb_guest+cum_ast_guest+cum_stl_guest+cum_blk_guest+cum_tov_guest+cum_pts_host+cum_fgm_host+cum_fga_host+cum_x3pm_host+cum_x3pa_host+cum_ftm_host+cum_fta_host+cum_oreb_host+cum_dreb_host+cum_ast_host+cum_stl_host+cum_blk_host+cum_tov_host,data=train,mtry=6,ntree=1000)
# plot(ntree_fit)
# model fit (mtry=6, ntree=400)
rf<-function(train,test){
  rfmodel <- randomForest(is_host_win~n_all_star_guest+n_all_star_host+cum_pts_guest+cum_fgm_guest+cum_fga_guest+cum_x3pm_guest+cum_x3pa_guest+cum_ftm_guest+cum_fta_guest+cum_oreb_guest+cum_dreb_guest+cum_ast_guest+cum_stl_guest+cum_blk_guest+cum_tov_guest+cum_pts_host+cum_fgm_host+cum_fga_host+cum_x3pm_host+cum_x3pa_host+cum_ftm_host+cum_fta_host+cum_oreb_host+cum_dreb_host+cum_ast_host+cum_stl_host+cum_blk_host+cum_tov_host,data=train,mtry=6,ntree=400,importance=TRUE)
  fittedresults=predict(rfmodel,newdata=test,type='response')
  Error=mean(fittedresults!=test$is_host_win)
  return(Error)
}

# input
# all star dataframe
team=c("WAS","UTA","TOR","SAS","SAC","POR","PHX","PHI","ORL","OKC","NYK","NOP","MIN","MIL","MIA","MEM","LAL","LAC","IND","HOU","GSW","DET","DEN","DAL","CLE","CHI","CHA","BOS","BKN","ATL")
season1617=c(1,0,2,3,0,0,0,0,0,1,1,2,0,0,0,0,0,1,1,1,4,1,0,0,1,1,0,2,0,1)
all_star_1617=data.frame(team,season1617)
colnames(all_star_1617)=c("team","num_all_star")
# game data in season 16-17
game_data_1617=read.csv("1617.csv",sep=",")

# cumlative information for each team 
processed_data_1617=data.frame(game_data_1617$TEAM,game_data_1617$DATE,game_data_1617$MATCHUP,game_data_1617$W.L,game_data_1617$PTS,game_data_1617$FGM,game_data_1617$FGA,game_data_1617$X3PM,game_data_1617$X3PA,game_data_1617$FTM,game_data_1617$FTA,game_data_1617$OREB,game_data_1617$DREB,game_data_1617$AST,game_data_1617$STL,game_data_1617$BLK,game_data_1617$TOV)
colnames(processed_data_1617)=c("team","date","matchup","wol","pts","fgm","fga","x3pm","x3pa","ftm","fta","oreb","dreb","ast","stl","blk","tov")
processed_data_1617=hostorguest(processed_data_1617)
processed_data_1617_cum <- processed_data_1617 %>% arrange(team) %>%
  group_by(team) %>%
  mutate(cum_pts = cumsum(pts)-pts) %>%
  mutate(cum_fgm = cumsum(fgm)-fgm) %>%
  mutate(cum_fga = cumsum(fga)-fga) %>%
  mutate(cum_x3pm = cumsum(x3pm)-x3pm) %>%
  mutate(cum_x3pa = cumsum(x3pa)-x3pa) %>%
  mutate(cum_ftm = cumsum(ftm)-ftm) %>%
  mutate(cum_fta = cumsum(fta)-fta) %>%
  mutate(cum_oreb = cumsum(oreb)-oreb) %>%
  mutate(cum_dreb = cumsum(dreb)-dreb) %>%
  mutate(cum_ast = cumsum(ast)-ast) %>%
  mutate(cum_stl = cumsum(stl)-stl) %>%
  mutate(cum_blk = cumsum(blk)-blk) %>%
  mutate(cum_tov = cumsum(tov)-tov) ->processed_data_1617_cum
processed_data_1617_cum <- select(processed_data_1617_cum, -(pts:tov)) 
processed_data_1617_cum <- select(processed_data_1617_cum, -(matchup:hog))

# game information (1230 games in one regular season)
game_info <- filter(processed_data_1617,hog==0)
game_info <- select(game_info, -(pts:hog))
colnames(game_info)=c("host_team","date","guest_team","is_host_win")
game_info <- merge(game_info,all_star_1617,by.x="guest_team",by.y="team",all.x=TRUE)
game_info <- merge(game_info,all_star_1617,by.x="host_team",by.y="team",all.x=TRUE)
colnames(game_info)=c("host_team","guest_team","date","is_host_win","n_all_star_guest","n_all_star_host")
game_info <- merge(game_info,processed_data_1617_cum,by.x=c("guest_team","date"),by.y=c("team","date"),all.x=TRUE)
game_info <- merge(game_info,processed_data_1617_cum,by.x=c("host_team","date"),by.y=c("team","date"),all.x=TRUE)
colnames(game_info)=c("host_team","date","guest_team","is_host_win","n_all_star_guest","n_all_star_host","cum_pts_guest","cum_fgm_guest","cum_fga_guest","cum_x3pm_guest","cum_x3pa_guest","cum_ftm_guest","cum_fta_guest","cum_oreb_guest","cum_dreb_guest","cum_ast_guest","cum_stl_guest","cum_blk_guest","cum_tov_guest","cum_pts_host","cum_fgm_host","cum_fga_host","cum_x3pm_host","cum_x3pa_host","cum_ftm_host","cum_fta_host","cum_oreb_host","cum_dreb_host","cum_ast_host","cum_stl_host","cum_blk_host","cum_tov_host")
game_info <- game_info[,c(2,1,3,4,6,20:32,5,7:19)] # adjust the order of column: game information, host information, guest information

# processed data output
# write.csv(game_info,"Processed data:game information.csv")

# An example 
set.seed(20)
train=game_info[sort(sample(nrow(game_info),0.8*nrow(game_info),replace=F)),]
set.seed(20)
test=game_info[-sample(nrow(game_info),0.8*nrow(game_info),replace=F),]
# Naive Bayes
Error=nb(train,test)
print(paste('Naive Bayes Accuracy:',1-Error))
# Logistic Regression
Error=logistic(train,test)
print(paste('Logistic Regression Accuracy:',1-Error))
# Random Forest
Error=rf(train,test)
print(paste('Random Forest Accuracy:',1-Error))

# Accuracy (iteration=1000)
errornb=0
errorlr=0
errorrf=0
for (i in 1:10){
  set.seed(i)
  train=game_info[sort(sample(nrow(game_info),0.8*nrow(game_info),replace=F)),]
  set.seed(i)
  test=game_info[-sample(nrow(game_info),0.8*nrow(game_info),replace=F),]
  errornb=errornb+nb(train,test)
  errorlr=errorlr+logistic(train,test)
  errorrf=errorrf+rf(train,test)
}
print(paste('Naive Bayes Accuracy:',1-errornb/10))
print(paste('Logistic Regression Accuracy:',1-errorlr/10))
print(paste('Random Forest Accuracy:',1-errorrf/10))
