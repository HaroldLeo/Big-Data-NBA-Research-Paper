
install.packages("dplyr")
library("dplyr")

install.packages("readxl")
library("readxl")

install.packages("magrittr")
library("magrittr")

#First we load in the NBA Team and Player Data:

# Read in Team-Stats Data:
teamstats <- read_excel("2017-2018_NBA_Box_Score_Team-Stats.xlsx", sheet = "2017-18 Season")

# Read in Team  Name
teamcities <- read_excel("2017-2018_NBA_Box_Score_Team-Stats.xlsx", sheet = "Teams-Cities")


# data exploration
library(ggplot2)

win <- teamstats %>%
  filter(DATASET=="2017-2018 Regular Season")%>%
  group_by(TEAMS) %>%
  summarise_at(vars(`1Q`,`2Q`,`3Q`,`4Q`,`F`, MIN, FG, FGA, `3P`, `3PA`, 
                    FT, FTA, OR, DR, TOT, A, PF,ST, TO, `TO TO`, BL, PTS,
                    POSS,PACE,OEFF,DEFF,win),          
               list(avg = mean)) 
ggplot(data=win, aes(x=reorder(TEAMS,win_avg), y=win_avg)) +
  geom_bar(stat="identity", fill="steelblue") + 
  geom_text(aes(label = win_avg), nudge_y = 0.2) + 
  coord_flip() +
  scale_fill_viridis_d() +
  ggtitle("Win rates by teams in 2017-18 NBA regular season") +
  xlab("Teams") + ylab("Win rate")

ggplot(data=sub1, aes(x=reorder(TEAMS,win_avg), y=win_avg)) +
  geom_col( aes(color = VENUE, fill = VENUE), position = position_dodge(0.8), width = 0.8) + 
  # geom_text(aes(label = win_avg), nudge_y = 3,) + 
  coord_flip() +
  scale_fill_viridis_d() +
  ggtitle("Win rates by teams and venue in 2017-18 NBA regular season") +
  xlab("Teams") + ylab("Win rate") +
  scale_color_manual(values = c("#0073C2FF", "#EFC000FF"))+
  scale_fill_manual(values = c("#0073C2FF", "#EFC000FF"))

playoff_win <- teamstats %>%
  filter((DATASET=="2017-2018 Regular Season") & (playoff==1))%>%
  group_by(TEAMS) %>%
  summarise_at(vars(`1Q`,`2Q`,`3Q`,`4Q`,`F`, MIN, FG, FGA, `3P`, `3PA`, 
                    FT, FTA, OR, DR, TOT, A, PF,ST, TO, `TO TO`, BL, PTS,
                    POSS,PACE,OEFF,DEFF,win),          
               list(avg = mean)) 

ggplot(data=playoff_win, aes(x=reorder(TEAMS,win_avg), y=win_avg)) +
  geom_bar(stat="identity", fill="steelblue") + 
  geom_text(aes(label = win_avg), nudge_y = 0.2) + 
  coord_flip() +
  scale_fill_viridis_d() +
  ggtitle("Win rates by playoff teams in 2017-18 NBA regular season") +
  xlab("Teams") + ylab("Win rate")

rest <- teamstats %>%
  filter(DATASET=="2017-2018 Regular Season")%>%
  group_by(`REST DAYS`) %>%
  summarise(count = n()) 

ggplot(data=rest, aes(x=`REST DAYS`, y=count)) +
  geom_bar(stat="identity", fill="steelblue") +
  geom_text(aes(label=count), vjust=-0.3, size=3.5) +
  theme_minimal() +
  ggtitle("Number of team appearances with respect to rest days in 2017-18 NBA regular season") +
  xlab("Rest days") + ylab("Number of appearances")

boxplot(teamstats$`1Q`,teamstats$`2Q`,teamstats$`3Q`,teamstats$`4Q`,teamstats$F,
        xlab = "Period",
        ylab = "Points", 
        main = "Average scores in 2017-18 NBA season",
        notch = TRUE, 
        varwidth = TRUE, 
        names = c("1st quarter","2nd quarter","3rd quarter","4th quarter", "Full game"))

boxplot(teamstats$FG, teamstats$FGA, teamstats$`3P`, teamstats$`3PA`, teamstats$FT, teamstats$FTA,
        xlab = "Categories",
        ylab = "Shots made", 
        main = "Average shooting data in 2017-18 NBA season",
        notch = TRUE, 
        varwidth = TRUE, 
        names = c("Field goal","FGA","3 point","3P att.", "Free throw", "FT att."))

boxplot(teamstats$OR, teamstats$DR, teamstats$A, teamstats$PF, teamstats$TO, teamstats$BL,
        xlab = "Categories",
        ylab = "Number", 
        main = "Average data in 2017-18 NBA season",
        notch = TRUE, 
        varwidth = TRUE, 
        names = c("O. rebound","D. reb","Assists","Per. Foul", "Turnover", "Block"))

boxplot(playerstats$FG, playerstats$FGA, playerstats$`3P`, playerstats$`3PA`, playerstats$FT, playerstats$FTA,
        xlab = "Categories",
        ylab = "Shots made", 
        main = "Average shooting data per player in 2017-18 NBA season",
        notch = FALSE, 
        varwidth = TRUE, 
        names = c("Field goal","FGA","3 point","3P att.", "Free throw", "FT att."))

boxplot(playerstats$OR, playerstats$DR, playerstats$A, playerstats$PF, playerstats$TO, playerstats$BL,
        xlab = "Categories",
        ylab = "Number", 
        main = "Average other data per player in 2017-18 NBA season",
        notch = FALSE, 
        varwidth = TRUE, 
        names = c("O. rebound","D. reb","Assists","Per. Foul", "Turnover", "Block"))

positions <- playerstats %>%
  group_by(POSITION) %>%
  summarise_at(vars(MIN, FG, FGA, `3P`, `3PA`, 
                    FT, FTA, OR, DR, TOT, A, PF, ST, TO, BL, PTS),          
               list(avg = mean)) 
ggplot(data=positions, aes(x=POSITION, y=PTS_avg)) +
  geom_bar(stat="identity", fill="steelblue") +
  geom_text(aes(label=PTS_avg), vjust=-0.3, size=3.5) +
  theme_minimal() +
  ggtitle("Average points per game with respect to positions in 2017-18 NBA season") +
  xlab("Positions") + ylab("Points")
venue <- playerstats %>%
  group_by(`VENUE (R/H)`) %>%
  summarise_at(vars(MIN, FG, FGA, `3P`, `3PA`, 
                    FT, FTA, OR, DR, TOT, A, PF, ST, TO, BL, PTS),          
               list(avg = mean)) 
ggplot(data=venue, aes(x=`VENUE (R/H)`, y=PTS_avg)) +
  geom_bar(stat="identity", fill="steelblue") +
  geom_text(aes(label=PTS_avg), vjust=-0.3, size=3.5) +
  theme_minimal() +
  ggtitle("Average points per game with respect to positions in 2017-18 NBA season") +
  xlab("Venue") + ylab("Points")



# part 1

#Rename starting lineup players
names(teamstats)[36] <- "Player 1"
names(teamstats)[37] <- "Player 2"
names(teamstats)[38] <- "Player 3"
names(teamstats)[39] <- "Player 4"
names(teamstats)[40] <- "Player 5"

#Then add the variable indicating win or loss
win <- c()
for (n in 1:1312){
  if (teamstats$`F`[2*n-1] > teamstats$`F`[2*n]){
    win[2*n-1] <- 1
    win[2*n] <- 0
  } else {
    win[2*n] <- 1
    win[2*n-1] <- 0
  }
}
teamstats$win <- win

# then we generate the playoff team list
playoff_list <- unique(teamstats[teamstats$`DATASET`=="2018 Playoffs",]$`TEAMS`)


#Next, we want to do row clusterings (see lecture 7 - K-means and hierarchical clustering) for the teams: 
# Need to transform the data first: 
# One row for each team: average over the regular season for each team over the following variables: 1Q,2Q,3Q,4Q,F, MIN, FG, FGA, 3P, 3PA, FT, FTA, OR,...Rest Days. Do this by home vs away (road) games. If T is the number of teams, should have 2T rows.

str(teamstats)
sub1 <- teamstats %>%
  filter(DATASET=="2017-2018 Regular Season")%>%
  group_by(TEAMS,VENUE) %>%
  summarise_at(vars(`1Q`,`2Q`,`3Q`,`4Q`,`F`, MIN, FG, FGA, `3P`, `3PA`, 
                    FT, FTA, OR, DR, TOT, A, PF,ST, TO, `TO TO`, BL, PTS,
                    POSS,PACE,OEFF,DEFF,win),          
               list(avg = mean)) 

sub1 <- as.data.frame(sub1) #transform into dataframe

sub1$playoff <- ifelse(sub1$TEAMS %in% playoff_list, 1, 0) 

sub1$TP_Hit_rate <- sub1$`3P_avg`/sub1$`3PA_avg` # add the variable of 3 pointer hit rate
sub1$FG_Hit_rate <- sub1$FG_avg/sub1$FGA_avg # add the variable of field goal hit rate


#We first cluster teams with their 3 pointer shooting capabilities

sub_3pt <- sub1[,c("TEAMS","VENUE","TP_Hit_rate", "3PA_avg")]

sub_3pt_home <- filter(sub_3pt, VENUE == "Home")[,-c(2)]
sub_3pt_road <- filter(sub_3pt, VENUE == "Road")[,-c(2)]

sub_3pt_home_r <- sub_3pt_home[,-1] #name the rows
rownames(sub_3pt_home_r) <- sub_3pt_home[,1]
sub_3pt_home <- sub_3pt_home_r

plot(sub_3pt_home,pch=19,col=4,cex=0.2, main="NBA Teams by 3-Pointer Capaiblity (Home)")

text(sub_3pt_home,labels=rownames(sub_3pt_home))



sub_3pt_road_r <- sub_3pt_road[,-1]  #name the rows
rownames(sub_3pt_road_r) <- sub_3pt_road[,1]
sub_3pt_road <- sub_3pt_road_r

plot(sub_3pt_road,pch=19,col=4,cex=0.2, main="NBA Teams by 3-Pointer Capaiblity (Road)")

text(sub_3pt_road,labels=rownames(sub_3pt_road))


# K-means - 3 pointer
## home
sub_3pt_home$TP_Hit_rate <- sub_3pt_home$TP_Hit_rate*100 #rescale into 100% to gain more accuracy in K-means approach

grp_3p_home = kmeans(sub_3pt_home, centers=7, nstart=100)

grp_3p_home
grp_3p_home$cluster

plot(sub_3pt_home,pch=19,col=4,cex=0.2,main="NBA Teams by 3-Pointer Capaiblity (7 Clusters)")

text(sub_3pt_home,labels=rownames(sub_3pt_home),
     col=grp_3p_home$cluster+1)

means_3p_home <- grp_3p_home$centers
points(means_3p_home,col=c(2,3,4,5,6),cex=2,pch=4)

## road
sub_3pt_road$Hit_rate <- sub_3pt_road$Hit_rate*100 #rescale into 100% to gain more accuracy in K-means approach

grp_3p_road = kmeans(sub_3pt_road, centers=5, nstart=10000)

grp_3p_road
grp_3p_road$cluster

plot(sub_3pt_road,pch=19,col=4,cex=0.2,main="NBA Teams by 3-Pointer Capaiblity (Road Clusters)")

text(sub_3pt_road,labels=rownames(sub_3pt_road),
     col=grp_3p_road$cluster+1)

means_3p_road <- grp_3p_road$centers
points(means_3p_road,col=c(2,3,4,5,6),cex=2,pch=4)


# Hierarchical clustering - 3 pointer
## Home
distance_3p_home <- dist(sub_3pt_home,method="euclidean")
hc_3p_home <- hclust(distance_3p_home, method = "average")
plot(hc_3p_home, main="NBA Teams by 3-Pointer Capaiblity (Home Clusters)")
memb_3p_home <- cutree(hc_3p_home, k = 5)
table(memb_3p_home, grp_3p_home$cluster)

## Road
distance_3p_road <- dist(sub_3pt_road,method="euclidean")
hc_3p_road <- hclust(distance_3p_road, method = "average")
plot(hc_3p_road,main="NBA Teams by 3-Pointer Capaiblity (Road Clusters)")
memb_3p_road <- cutree(hc_3p_road, k = 5)
table(memb_3p_road, grp_3p_road$cluster)



# Then perform K-means with all variables (try several Ks = 2-10) and hiearchical clusterings on these variables for two sets: the first set of all teams playing at home (H), the second set of all teams playing at road (R)
## home
sub1_home <- filter(sub1, VENUE=="Home")[,-c(2,32)]
## road
sub1_road <- filter(sub1, VENUE=="Road")[,-c(2,32)]


sub1_home_r <- sub1_home[,-1]  #name the rows
rownames(sub1_home_r) <- sub1_home[,1]
sub1_home <- sub1_home_r

sub1_road_r <- sub1_road[,-1]  #name the rows
rownames(sub1_road_r) <- sub1_road[,1]
sub1_road <- sub1_road_r

# K-means
## home
grp_home = kmeans(x=sub1_home, centers=7, nstart=1000)
#plot(sub1_home,pch=19,col=4,cex=0.2)
grp_home
grp_home$cluster


## road
grp_road = kmeans(x=sub1_road, centers=5, nstart=1000)
#plot(sub1_road,pch=19,col=4,cex=0.2)
grp_road
grp_road$cluster

#AICc and BIC analysis
## 3-pointers
kfit_3pt_home <- lapply(2:10, function(k) kmeans(sub_3pt_home,k))
source("kIC.R")

kaicc_3pt_home <- sapply(kfit_3pt_home,kIC)
kbic_3pt_home <- sapply(kfit_3pt_home,kIC,"B")

## All variables
kfit_home <- lapply(2:10, function(k) kmeans(sub1_home,k))
source("kIC.R")

kaicc_home <- sapply(kfit_home,kIC)
kbic_home <- sapply(kfit_home,kIC,"B")


kfit_road <- lapply(2:10, function(k) kmeans(sub1_road,k))
source("kIC.R")

kaicc_road <- sapply(kfit_road,kIC)
kbic_road <- sapply(kfit_road,kIC,"B")


## plot 'em:
### 3 pointers home

par(mfrow=c(1,2))
plot(2:10, kaicc_3pt_home, xlab="K", ylab="IC",
     bty="n", type="l", lwd=2, main = "AICc")
abline(v=which.min(kaicc_3pt_home)*5)
plot(2:10, kbic_3pt_home, xlab="K", ylab="IC",main = "BIC",
     bty="n", type="l", lwd=2, col=4)
abline(v=which.min(kbic_3pt_home),col=4)

### home
par(mfrow=c(1,2))
plot(2:10, kaicc_home, xlab="K", ylab="IC",main = "AICc",
     bty="n", type="l", lwd=2)
abline(v=which.min(kaicc_home)*5)
plot(2:10, kbic_home, xlab="K", ylab="IC",main = "BIC",
     bty="n", type="l", lwd=2, col=4)
abline(v=which.min(kbic_home),col=4)

### road
par(mfrow=c(1,2))
plot(2:10, kaicc_road, xlab="K", ylab="IC",
     bty="n", type="l", lwd=2)
abline(v=which.min(kaicc_road)*5)
plot(2:10, kbic_road, xlab="K", ylab="IC",
     bty="n", type="l", lwd=2, col=4)
abline(v=which.min(kbic_road),col=4)



# Hierarchical clustering

## Home
par(mfrow=c(1,1))
distance_home <- dist(sub1_home,method="euclidean")
hc_home <- hclust(distance_home, method = "average")
plot(hc_home)
memb_home <- cutree(hc_home, k = 5)
table(memb_home, grp_home$cluster)

## Road
distance_road <- dist(sub1_road,method="euclidean")
hc_road <- hclust(distance_road, method = "average")
plot(hc_road)
memb_road <- cutree(hc_road, k = 5)
table(memb_road, grp_road$cluster)
#dev.off()


#Then run cluster regression: use the clusterings to predict whether or not the team made it to the playoff? Something like, are offensive teams more likely to make it to the playoff vs defensive? "1st quarter" team vs "2nd quarter" vs "4th "quarter team? Depends on what we find from the clustering exercise.

# Add back playoff
playoff_home <- filter(sub1, VENUE == "Home")[,-c(1:31)]
sub1_home$playoff <- playoff_home
sub_3pt_home$playoff <- playoff_home

playoff_road <- filter(sub1, VENUE == "Road")[,-c(1:31)]
sub1_road$playoff <- playoff_road
sub_3pt_road$playoff <- playoff_road



# Cluster Regression
# use cluster regression to predict whether a team goes to playoff

library(gamlr)
#Just 3-pointers - home
home_3pt_clust <- sparse.model.matrix(~factor(kfit_3pt_home[[7]]$cluster)) # cluster membership matrix
home_3pt_playoffclus <- cv.gamlr(home_3pt_clust,sub_3pt_home$playoff,lambda.min.ratio=1e-5) # 

plot(home_3pt_playoffclus)

max(1-home_3pt_playoffclus$cvm/home_3pt_playoffclus$cvm[1]) 

tapply(sub_3pt_home$playoff,kfit_3pt_home[[7]]$cluster,mean)

#All variables - home

home_clust <- sparse.model.matrix(~factor(kfit_home[[7]]$cluster)) # cluster membership matrix
home_playoffclus <- cv.gamlr(home_clust,sub1_home$playoff,lambda.min.ratio=1e-5) # 

plot(home_playoffclus)

max(1-home_playoffclus$cvm/home_playoffclus$cvm[1]) 

tapply(sub1_home$playoff,kfit_home[[7]]$cluster,mean)


# part 2
###TEAMSTATS
#LASSO on wining
col_needed=cbind(teamstats[,5:8],teamstats[,15:29],teamstats[,31:33])
a = data.frame(col_needed)
x = sparse.model.matrix(~., data=a)[,-1]
lasso1<-gamlr(x,y=teamstats$win,standardize=FALSE,family="binomial")
summary(lasso1)$r2[which.min(AICc(lasso1))]
summary(lasso1)$lambda[which.min(AICc(lasso1))]

plot(lasso1)
c = abs(coef(lasso1))
c[order(c, decreasing=TRUE),]

cv.fit <- cv.gamlr(x,
                   +                    y=Y,
                   +                    lambda.min.ratio=1e-3,
                   +                    family="binomial",
                   +                    verb=TRUE)

coef(cv.fit, select="min")
cv.fit$lambda.min
coef(cv.fit, select="1se")
cv.fit$lambda.1se


##prediction on winning(model accuracy test)
w = teamstats[,ncol(teamstats)]
a1 <- cbind(a,w)
train_index <- a1$win%>% createDataPartition(p = 0.8, list = FALSE)
train_data <- a1[train_index[,1],]
test_data <-a1[-train_index[,1],]
asso.model <- cv.gamlr(train_data[,1:22], y=train_data[,23] , standardize = FALSE, family = "binomial")
prob <- predict(lasso.model, train_data[,1:22],type="response")
predicted.result <- ifelse(prob > 0.5, "1", "0")
pred.result <- as.numeric(predicted.result)
mean(pred.result == observed.result)


###LASSO ON FINAL SCORE
###POISSON
lasso2p<-gamlr(x[,6:21],y=teamstats$F,standardize=FALSE,family="poisson")
summary(lasso2p)$r2[which.min(AICc(lasso2p))]
c2p = abs(coef(lasso2p))
c2p[order(c2p, decreasing=TRUE),]
plot(lasso2p)

####GAUSSIAN
lasso2<-gamlr(x[,6:21],y=teamstats$F,standardize=FALSE,family="gaussian")
summary(lasso2)$r2[which.min(AICc(lasso2))]


plot(lasso2)
c2 = abs(coef(lasso2))
c2[order(c2, decreasing=TRUE),]

plot(lasso2)

####GETTING INTO PLAYOFF
teamsub <- teamstats %>%
  filter(DATASET=="2017-2018 Regular Season")%>%
  group_by(TEAMS,VENUE) %>%
  summarise_at(vars(`1Q`,`2Q`,`3Q`,`4Q`,`F`, MIN, FG, FGA, `3P`, `3PA`, 
                    FT, FTA, OR, DR, TOT, A, PF,ST, TO, `TO TO`, BL, PTS,
                    POSS,PACE,OEFF,DEFF,win),          
               list(avg = mean))
teamsub$playoff <- ifelse(teamsub$`TEAMS` %in% playoff_teams, 1, 0)
xt = data.frame(teamsub[3:28])
xt_spm = sparse.model.matrix(~.,data=xt)[,-1]
lassot = gamlr(xt_spm,y=teamsub$playoff,family="binomial")

#### PLAYER GETTING INTO PLAYOFF

##SETUP
teamcities <- read_excel("2017-2018_NBA_Box_Score_Team-Stats.xlsx", sheet = "Teams-Cities")
playerstats <- read_excel("NBA-2017-2018-Player-BoxScore-Dataset.xlsx", sheet = "2017-18 Season")
Regularsub <- playerstats[playerstats$`DATA SET`=="2017-2018 Regular Season",]%>%
  group_by(`PLAYER FULL NAME`,`VENUE (R/H)`) %>%
  summarise_at(vars(colnames(playerstats)[8:23]),list(avg = mean)) 

Rhome = Regularsub[Regularsub$`VENUE (R/H)`=="H",]
Rroad = Regularsub[Regularsub$`VENUE (R/H)`=="R",]
a_player = unique(playerstats[,c(1,3,4,7)])
b_player = a_player[a_player$`VENUE (R/H)`=="H",]
Rh_player = b_player$POSITION[1:530]
playoff_list <- unique(playerstats[playerstats$`DATA SET`=="2018 Playoffs",]$`PLAYER FULL NAME`)
Regularsub$playoff <- ifelse(Regularsub$`PLAYER FULL NAME` %in% playoff_list, 1, 0)

##LASSO
xp = data.frame(Regularsub[,3:18])
xp_spm = sparse.model.matrix(~.,data = xp)[,-1]
lassop = gamlr(xp_spm,y=Regularsub$playoff,family="binomial")
summary(lassop)$r2[which.min(AICc(lassop))]
plot(lassop)
cp = abs(coef(lasso1))
cp[order(cp, decreasing=TRUE),]
summary(lassop)$lambda[which.min(AICc(lassop))]
summary(lassop)$r2[which.min(BIC(lassop))]
summary(lassop)$lambda[which.min(BIC(lassop))]

cvp.fit <- cv.gamlr(xp_spm, y=Regularsub$playoff,lambda.min.ratio=1e-3,family="binomial",verb=TRUE)
coef(cvp.fit, select="min")
cvp.fit$lambda.min


# part 3
# next we do the analysis part of players
# load in player stats
playerstats <- read_excel("NBA-2017-2018-Player-BoxScore-Dataset.xlsx", sheet = "2017-18 Season")

# average the player stats of the season (separate for road and home, for regular season)
Regularsub <- playerstats[playerstats$`DATA SET`=="2017-2018 Regular Season",]%>%
  group_by(`PLAYER FULL NAME`,`VENUE (R/H)`) %>%
  summarise_at(vars(colnames(playerstats)[8:23]),list(avg = mean)) 

Rhome = Regularsub[Regularsub$`VENUE (R/H)`=="H",]
Rroad = Regularsub[Regularsub$`VENUE (R/H)`=="R",]
a_player = unique(playerstats[,c(1,3,4,7)])
b_player = a_player[a_player$`VENUE (R/H)`=="H",]
Rh_player = b_player$POSITION[1:530]
playerlist = intersect(Rhome$`PLAYER FULL NAME`,Rroad$`PLAYER FULL NAME`)
playoff_list <- unique(playerstats[playerstats$`DATA SET`=="2018 Playoffs",]$`PLAYER FULL NAME`)
Regularsub$playoff <- ifelse(Regularsub$`PLAYER FULL NAME` %in% playoff_list, 1, 0) 


# Do clustering to get similar players
# for clustering, we have k-means and hierarchical clustering
# first we use k-means, there are 7 positions in all
ave_perf=data.frame(sapply(Rhome[-c(1:3,14,18)],function(x)x/Rhome[3]))
clu_home = kmeans(scale(ave_perf), 7, nstart=10)
table(Rh_player,clu_home$cluster)


source("kIC.R")
kfit <- lapply(1:30, function(k) kmeans(scale(Rhome[3:18]),k))
Rhome$clu = kfit[[10]]$cluster
kaicc <- sapply(kfit,kIC)
kbic <- sapply(kfit,kIC,"B")
par(mfrow=c(1,2))
plot(1:30, kaicc, xlab="K", ylab="IC",
     bty="n", type="l", lwd=2)
abline(v=which.min(kaicc))
plot(1:30, kbic, xlab="K", ylab="IC",
     bty="n", type="l", lwd=2, col=4)
abline(v=which.min(kbic),col=4)

kfit2 <- lapply(1:30, function(k) kmeans(scale(Rroad[3:18]),k))
kaicc2 <- sapply(kfit2,kIC)
kbic2 <- sapply(kfit2,kIC,"B")
par(mfrow=c(1,2))
plot(1:30, kaicc2, xlab="K", ylab="IC",
     bty="n", type="l", lwd=2)
abline(v=which.min(kaicc2))
plot(1:30, kbic2, xlab="K", ylab="IC",
     bty="n", type="l", lwd=2, col=4)
abline(v=which.min(kbic2),col=4)

par(mfrow=c(1,2))

plot(x=Rhome$MIN_avg, y=Rhome$PTS_avg, col=rainbow(10)[kfit[[10]]$cluster],ylab="points avg",xlab="minutes avg")
plot(x=Rhome$OR_avg, y=Rhome$DR_avg,col=rainbow(10)[kfit[[10]]$cluster],ylab="DR avg",xlab="OR avg")
plot(x=Rhome$ST_avg, y=Rhome$BL_avg, col=rainbow(10)[kfit[[10]]$cluster],ylab="block avg",xlab="steals avg")
plot(x=Rhome$FG_avg, y=Rhome$`3P_avg`, col=rainbow(10)[kfit[[10]]$cluster],ylab="3 points avg",xlab="FG avg")

# do comparison to the players that appear in both home games and road games
playerlist = intersect(Rhome$`PLAYER FULL NAME`,Rroad$`PLAYER FULL NAME`)
Rhome1 = Rhome[Rhome$`PLAYER FULL NAME` %in% playerlist,]
Rroad1 = Rroad[Rroad$`PLAYER FULL NAME` %in% playerlist,]
h1 = kmeans(scale(Rhome1[3:18]),10)
r1 = kmeans(scale(Rroad1[3:18]),10)
table(h1$cluster,kfit[[10]]$cluster)


# then we do hierarchical clustering
## Home
par(mfrow=c(1,1))
distance_home <- dist(scale(Rhome[3:18]),method="euclidean")
hc_home <- hclust(distance_home, method = "average")
plot(hc_home)
memb_home <- cutree(hc_home, k = 10)
table(memb_home, clu_home$cluster)
## Road
distance_road <- dist(scale(Rroad[3:18]),method="euclidean")
hc_road <- hclust(distance_road, method = "average")
plot(hc_road)
memb_road <- cutree(hc_road, k = 10)
table(memb_road, clu_road$cluster)
table(clu_road$cluster, clu_home$cluster)


# use cluster regression to predict whether a player goes to playoff
library(gamlr)
playerclust <- sparse.model.matrix(~factor(kfit[[10]]$cluster))
player_playoffclus <- cv.gamlr(playerclust, Regularsub[Regularsub$`VENUE (R/H)`=="H",]$playoff, family="binomial")
plot(player_playoffclus)
max(1-player_playoffclus$cvm/player_playoffclus$cvm[1])
predraw = predict(player_playoffclus, playerclust,type="response")
pred = ifelse(predraw >= 0.5, 1, 0)
(length(pred)-sum((pred-Regularsub[Regularsub$`VENUE (R/H)`=="H",]$playoff)^2))/length(pred)


# causal lasso
y = Regularsub[Regularsub$`VENUE (R/H)`=="H",]$playoff
d = Rhome$DR_avg
x = Rhome[,-c(1:3,11:12)]
treat = gamlr(x, d)
summary(treat)
dhat = predict(treat,x,type="response")
d2 = as.matrix(dhat)
causal <- gamlr(cBind(d,d2,x),y,free=2)
summary(causal)
coef(causal)
plot(treat)
plot(causal)



# part 4
# PCA part
pc = prcomp(Rhome[3:18],scale=TRUE)
plot(pc)
summary(pc)
zc = predict(pc)
grp <- kmeans(scale(Rhome[3:18]), centers=10, nstart=20)
plot(zc[,1]~as.factor(Regularsub[Regularsub$`VENUE (R/H)`=="H",]$playoff))
plot(zc[,2]~as.factor(Regularsub[Regularsub$`VENUE (R/H)`=="H",]$playoff))
loadings <- pc$rotation[,1:2]
hist(loadings[,1], main="",xlab="1st principal component loadings",col=8, border=grey(.9))
hist(loadings[,2], main="",xlab="2nd principal component loadings",col=8, border=grey(.9))


# do pca regression
pc_reg = glm(Regularsub[Regularsub$`VENUE (R/H)`=="H",]$playoff~., data=data.frame(zc[,1:3]),family="binomial")
summary(pc_reg)


## how do the PCs look?

par(mfrow=c(1,2))

plot(zc[,1:2], type="n", xlim=c(-4,5))

text(x=zc[,1], y=zc[,2], labels=rownames(Rhome[3:18]), col=rainbow(10)[grp$cluster])

plot(zc[,3:4], type="n", xlim=c(-3,3))

text(x=zc[,3], y=zc[,4], labels=rownames(Rhome[3:18]), col=rainbow(10)[grp$cluster])


y = Regularsub[Regularsub$`VENUE (R/H)`=="H",]$playoff
d = Rhome$PTS_avg
x = Rhome[,-c(1:9,18)]
treat = gamlr(x, d)
summary(treat)
dhat = predict(treat,x,type="response")
d2 = as.matrix(dhat)
causal <- gamlr(cBind(d,d2,x),y,free=2)
summary(causal)
coef(causal)
plot(treat)
plot(causal)


