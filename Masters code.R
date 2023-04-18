df=read.csv("C:/Users/dsmie/PL2021.csv")
df2020=read.csv("C:/Users/dsmie/PL2020.csv")
df2019=read.csv("C:/Users/dsmie/PL2019.csv")
df2018=read.csv("C:/Users/dsmie/PL2018.csv")
df2017=read.csv("C:/Users/dsmie/PL2017.csv")
dfall=read.csv("C:/Users/dsmie/PLall.csv")
library(Countr)
library(plyr)
library(dplyr)
library(ggplot2)
library(lmtest)
library(skellam)
library(energy)


df$HomeTeam <- as.factor(df$HomeTeam)
df$AwayTeam <- as.factor(df$AwayTeam)
df2 <- rbind(data.frame(goals=df$HomeGoals,team=df$HomeTeam,opponent=df$AwayTeam,home=1),
                    data.frame(goals=df$AwayGoals,team=df$AwayTeam,opponent=df$HomeTeam,home=0)) 

away_poiss <- glm(formula = goals ~ home + team +opponent,data=df2, family = poisson)
away_wei <- renewalCount(formula = goals ~ home + team + opponent,data=df2,dist = "weibull", weiMethod = "conv_dePril", computeHessian = FALSE,control = renewal.control(trace = 0, method = "nlminb"))

breaks_ <- 0:5
pears <- compareToGLM(poisson_model = away_poiss, breaks = breaks_, weibull = away_wei)

frequency_plot(pears$Counts, pears$Actual,
               dplyr::select(pears, contains("_predicted")),
               colours = c("grey", "blue", "green", "black"))

lr <- lrtest(away_poiss, away_wei)
lr

gof_pois <- chiSq_gof(away_poiss, breaks = breaks_)
gof_wei <- chiSq_gof(away_wei, breaks = breaks_)

print(gof_wei)
print(gof_pois)

ks.test(pears$Actual,pears$weibull_predicted)
ks.test(pears$Actual,pears$poisson_predicted)

dcor.test(pears$Actual,pears$weibull_predicted,index = 1.0,R=199)
dcor.test(pears$Actual,pears$poisson_predicted,index = 1.0,R=199)



df2020$HomeTeam <- as.factor(df2020$HomeTeam)
df2020$AwayTeam <- as.factor(df2020$AwayTeam)
df2020new <- rbind(data.frame(goals=df2020$HomeGoals,team=df2020$HomeTeam,opponent=df2020$AwayTeam,home=1),
             data.frame(goals=df2020$AwayGoals,team=df2020$AwayTeam,opponent=df2020$HomeTeam,home=0)) 

poiss_2020 <- glm(formula = goals ~ home + team +opponent,data=df2020new, family = poisson)
wei_2020 <- renewalCount(formula = goals ~ home + team + opponent,data=df2020new,dist = "weibull", weiMethod = "conv_dePril", computeHessian = FALSE,control = renewal.control(trace = 0, method = "nlminb"))

pears2020 <- compareToGLM(poisson_model = poiss_2020, breaks = breaks_, weibull = wei_2020)

frequency_plot(pears2020$Counts, pears2020$Actual,
               dplyr::select(pears2020, contains("_predicted")),
               colours = c("grey", "blue", "green", "black"))

lr2020 <- lrtest(poiss_2020, wei_2020)
lr2020
gof_pois2020 <- chiSq_gof(poiss_2020, breaks = breaks_)
gof_wei2020 <- chiSq_gof(wei_2020, breaks = breaks_)
gof_pois2020
gof_wei2020
ks.test(pears2020$Actual,pears2020$weibull_predicted)
ks.test(pears2020$Actual,pears2020$poisson_predicted)
dcor.test(pears2020$Actual,pears2020$weibull_predicted,index = 1.0,R=199)
dcor.test(pears2020$Actual,pears2020$poisson_predicted,index = 1.0,R=199)

df2019$HomeTeam <- as.factor(df2019$HomeTeam)
df2019$AwayTeam <- as.factor(df2019$AwayTeam)
df2019new <- rbind(data.frame(goals=df2019$HomeGoals,team=df2019$HomeTeam,opponent=df2019$AwayTeam,home=1),
                   data.frame(goals=df2019$AwayGoals,team=df2019$AwayTeam,opponent=df2019$HomeTeam,home=0)) 

poiss_2019 <- glm(formula = goals ~ home + team +opponent,data=df2019new, family = poisson)
wei_2019 <- renewalCount(formula = goals ~ home + team + opponent,data=df2019new,dist = "weibull", weiMethod = "conv_dePril", computeHessian = FALSE,control = renewal.control(trace = 0, method = "nlminb"))

pears2019 <- compareToGLM(poisson_model = poiss_2019, breaks = breaks_, weibull = wei_2019)

frequency_plot(pears2019$Counts, pears2019$Actual,
               dplyr::select(pears2019, contains("_predicted")),
               colours = c("grey", "blue", "green", "black"))

lr2019 <- lrtest(poiss_2019, wei_2019)
lr2019
gof_pois2019 <- chiSq_gof(poiss_2019, breaks = breaks_)
gof_wei2019 <- chiSq_gof(wei_2019, breaks = breaks_)
gof_pois2019
gof_wei2019
ks.test(pears2019$Actual,pears2019$weibull_predicted)
ks.test(pears2019$Actual,pears2019$poisson_predicted)
dcor.test(pears2019$Actual,pears2019$weibull_predicted,index = 1.0,R=199)
dcor.test(pears2019$Actual,pears2019$poisson_predicted,index = 1.0,R=199)

df2018$HomeTeam <- as.factor(df2018$HomeTeam)
df2018$AwayTeam <- as.factor(df2018$AwayTeam)
df2018new <- rbind(data.frame(goals=df2018$HomeGoals,team=df2018$HomeTeam,opponent=df2018$AwayTeam,home=1),
                   data.frame(goals=df2018$AwayGoals,team=df2018$AwayTeam,opponent=df2018$HomeTeam,home=0)) 

poiss_2018 <- glm(formula = goals ~ home + team +opponent,data=df2018new, family = poisson)
wei_2018 <- renewalCount(formula = goals ~ home + team + opponent,data=df2018new,dist = "weibull", weiMethod = "conv_dePril", computeHessian = FALSE,control = renewal.control(trace = 0, method = "nlminb"))

pears2018 <- compareToGLM(poisson_model = poiss_2018, breaks = breaks_, weibull = wei_2018)

frequency_plot(pears2018$Counts, pears2018$Actual,
               dplyr::select(pears2018, contains("_predicted")),
               colours = c("grey", "blue", "green", "black"))

lr2018 <- lrtest(poiss_2018, wei_2018)
lr2018
gof_pois2018 <- chiSq_gof(poiss_2018, breaks = breaks_)
gof_wei2018 <- chiSq_gof(wei_2018, breaks = breaks_)
gof_pois2018
gof_wei2018
ks.test(pears2018$Actual,pears2018$weibull_predicted)
ks.test(pears2018$Actual,pears2018$poisson_predicted)
dcor.test(pears2018$Actual,pears2018$weibull_predicted,index = 1.0,R=199)
dcor.test(pears2018$Actual,pears2018$poisson_predicted,index = 1.0,R=199)


df2017$HomeTeam <- as.factor(df2017$HomeTeam)
df2017$AwayTeam <- as.factor(df2017$AwayTeam)
df2017new <- rbind(data.frame(goals=df2017$HomeGoals,team=df2017$HomeTeam,opponent=df2017$AwayTeam,home=1),
                   data.frame(goals=df2017$AwayGoals,team=df2017$AwayTeam,opponent=df2017$HomeTeam,home=0)) 

poiss_2017 <- glm(formula = goals ~ home + team +opponent,data=df2017new, family = poisson)
wei_2017 <- renewalCount(formula = goals ~ home + team + opponent,data=df2017new,dist = "weibull", weiMethod = "conv_dePril", computeHessian = FALSE,control = renewal.control(trace = 0, method = "nlminb"))

pears2017 <- compareToGLM(poisson_model = poiss_2017, breaks = breaks_, weibull = wei_2017)

frequency_plot(pears2017$Counts, pears2017$Actual,
               dplyr::select(pears2017, contains("_predicted")),
               colours = c("grey", "blue", "green", "black"))

lr2017 <- lrtest(poiss_2017, wei_2017)
lr2017
gof_pois2017 <- chiSq_gof(poiss_2017, breaks = breaks_)
gof_wei2017 <- chiSq_gof(wei_2017, breaks = breaks_)
gof_pois2017
gof_wei2017
ks.test(pears2017$Actual,pears2017$weibull_predicted)
ks.test(pears2017$Actual,pears2017$poisson_predicted)
dcor.test(pears2017$Actual,pears2017$weibull_predicted,index = 1.0,R=199)
dcor.test(pears2017$Actual,pears2017$poisson_predicted,index = 1.0,R=199)


dfall$HomeTeam <- as.factor(dfall$HomeTeam)
dfall$AwayTeam <- as.factor(dfall$AwayTeam)
dfallnew <- rbind(data.frame(goals=dfall$HomeGoals,team=dfall$HomeTeam,opponent=dfall$AwayTeam,home=1),
                   data.frame(goals=dfall$AwayGoals,team=dfall$AwayTeam,opponent=dfall$HomeTeam,home=0)) 

poiss_all <- glm(formula = goals ~ home + team +opponent,data=dfallnew, family = poisson)
wei_all <- renewalCount(formula = goals ~ home + team + opponent,data=dfallnew,dist = "weibull", weiMethod = "conv_dePril", computeHessian = FALSE,control = renewal.control(trace = 0, method = "nlminb"))

pearsall <- compareToGLM(poisson_model = poiss_all, breaks = breaks_, weibull = wei_all)

frequency_plot(pearsall$Counts, pearsall$Actual,
               dplyr::select(pearsall, contains("_predicted")),
               colours = c("grey", "blue", "green", "black"))

lrall <- lrtest(poiss_all, wei_all)
lrall
gof_poisall <- chiSq_gof(poiss_all, breaks = breaks_)
gof_weiall <- chiSq_gof(wei_all, breaks = breaks_)
gof_poisall
gof_weiall
ks.test(pearsall$Actual,pearsall$weibull_predicted)
ks.test(pearsall$Actual,pearsall$poisson_predicted)
dcor.test(pearsall$Actual,pearsall$weibull_predicted,index = 1.0,R=199)
dcor.test(pearsall$Actual,pearsall$poisson_predicted,index = 1.0,R=199)

####################################################################
my_post_theme=
  theme_minimal() +
  theme(axis.text.x = element_text(face="bold", color="#666666", 
                                   size=10, margin = margin(t = -5)),
        axis.title = element_text(color="black", face="bold",size=12),
        plot.title = element_text(color="black", face="bold", size=14),
        axis.text.y = element_text(face="bold", color="#666666", 
                                   size=10),
        legend.text=element_text(size=12),
        legend.title=element_text(size=13),
        legend.key=element_blank(),
        axis.ticks.length=unit(0, "cm"))

skellam2021 <- mutate(df, goal_diff=HomeGoals-AwayGoals) %>% group_by(goal_diff) %>%
  dplyr::summarize(actual=n()/nrow(.)) %>% 
  inner_join(data.frame(goal_diff=-5:5,
                        pred=skellam::dskellam(-5:5,mean(df$HomeGoals),
                                               mean(df$AwayGoals))),by=c("goal_diff"))
skellam2020 <- mutate(df2020, goal_diff=HomeGoals-AwayGoals) %>% group_by(goal_diff) %>%
  dplyr::summarize(actual=n()/nrow(.)) %>% 
  inner_join(data.frame(goal_diff=-5:5,
                        pred=skellam::dskellam(-5:5,mean(df2020$HomeGoals),
                                               mean(df2020$AwayGoals))),by=c("goal_diff"))

skellam2019 <- mutate(df2019, goal_diff=HomeGoals-AwayGoals) %>% group_by(goal_diff) %>%
  dplyr::summarize(actual=n()/nrow(.)) %>% 
  inner_join(data.frame(goal_diff=-5:5,
                        pred=skellam::dskellam(-5:5,mean(df2019$HomeGoals),
                                               mean(df2019$AwayGoals))),by=c("goal_diff"))

skellam2018 <- mutate(df2018, goal_diff=HomeGoals-AwayGoals) %>% group_by(goal_diff) %>%
  dplyr::summarize(actual=n()/nrow(.)) %>% 
  inner_join(data.frame(goal_diff=-5:5,
                        pred=skellam::dskellam(-5:5,mean(df2018$HomeGoals),
                                               mean(df2018$AwayGoals))),by=c("goal_diff"))

skellam2017 <- mutate(df2017, goal_diff=HomeGoals-AwayGoals) %>% group_by(goal_diff) %>%
  dplyr::summarize(actual=n()/nrow(.)) %>% 
  inner_join(data.frame(goal_diff=-5:5,
                        pred=skellam::dskellam(-5:5,mean(df2017$HomeGoals),
                                               mean(df2017$AwayGoals))),by=c("goal_diff"))

skellamall <- mutate(dfall, goal_diff=HomeGoals-AwayGoals) %>% group_by(goal_diff) %>%
  dplyr::summarize(actual=n()/nrow(.)) %>% 
  inner_join(data.frame(goal_diff=-5:5,
                        pred=skellam::dskellam(-5:5,mean(dfall$HomeGoals),
                                               mean(dfall$AwayGoals))),by=c("goal_diff"))

ks.test(skellam2021$actual,skellam2021$pred)
dcor.test(skellam2021$actual,skellam2021$pred,index = 1.0,R=199)

ks.test(skellam2020$actual,skellam2020$pred)
dcor.test(skellam2020$actual,skellam2020$pred,index = 1.0,R=199)

ks.test(skellam2019$actual,skellam2019$pred)
dcor.test(skellam2019$actual,skellam2019$pred,index = 1.0,R=199)

ks.test(skellam2018$actual,skellam2018$pred)
dcor.test(skellam2018$actual,skellam2018$pred,index = 1.0,R=199)

ks.test(skellam2017$actual,skellam2017$pred)
dcor.test(skellam2017$actual,skellam2017$pred,index = 1.0,R=199)

ks.test(skellamall$actual,skellamall$pred)
dcor.test(skellamall$actual,skellamall$pred,index = 1.0,R=199)


mutate(df, goal_diff=HomeGoals-AwayGoals) %>% group_by(goal_diff) %>%
  summarize(actual=n()/nrow(.)) %>% 
  inner_join(data.frame(goal_diff=-5:5,
                        pred=skellam::dskellam(-5:5,mean(df$HomeGoals),
                                               mean(df$AwayGoals))),
             by=c("goal_diff")) %>%
  ggplot(aes(x=as.factor(goal_diff))) + 
  geom_bar(aes(y = actual,fill="d2"), stat="identity") +
  geom_line(aes(y = pred, group = 1, color = "d1"), size=1.25) +
  scale_colour_manual(name="Skellam",labels="", values=c("d1" = "blue", "d2" = "red"))+
  scale_fill_manual(name="Actual",labels="",values="red")  +
  ggtitle("Difference in Goals Scored (Home Team vs Away Team)")  + xlab("Home Goals - Away Goals") + ylab("Proportion of Matches") +
  my_post_theme


mutate(df2020, goal_diff=HomeGoals-AwayGoals) %>% group_by(goal_diff) %>%
  summarize(actual=n()/nrow(.)) %>% 
  inner_join(data.frame(goal_diff=-5:5,
                        pred=skellam::dskellam(-5:5,mean(df2020$HomeGoals),
                                               mean(df2020$AwayGoals))),
             by=c("goal_diff")) %>%
  ggplot(aes(x=as.factor(goal_diff))) + 
  geom_bar(aes(y = actual,fill="d2"), stat="identity") +
  geom_line(aes(y = pred, group = 1, color = "d1"), size=1.25) +
  scale_colour_manual(name="Skellam",labels="", values=c("d1" = "blue", "d2" = "red"))+
  scale_fill_manual(name="Actual",labels="",values="red")  +
  ggtitle("Difference in Goals Scored (Home Team vs Away Team)")  + xlab("Home Goals - Away Goals") + ylab("Proportion of Matches") +
  my_post_theme


mutate(df2019, goal_diff=HomeGoals-AwayGoals) %>% group_by(goal_diff) %>%
  summarize(actual=n()/nrow(.)) %>% 
  inner_join(data.frame(goal_diff=-5:5,
                        pred=skellam::dskellam(-5:5,mean(df2019$HomeGoals),
                                               mean(df2019$AwayGoals))),
             by=c("goal_diff")) %>%
  ggplot(aes(x=as.factor(goal_diff))) + 
  geom_bar(aes(y = actual,fill="d2"), stat="identity") +
  geom_line(aes(y = pred, group = 1, color = "d1"), size=1.25) +
  scale_colour_manual(name="Skellam",labels="", values=c("d1" = "blue", "d2" = "red"))+
  scale_fill_manual(name="Actual",labels="",values="red")  +
  ggtitle("Difference in Goals Scored (Home Team vs Away Team)")  + xlab("Home Goals - Away Goals") + ylab("Proportion of Matches") +
  my_post_theme


mutate(df2018, goal_diff=HomeGoals-AwayGoals) %>% group_by(goal_diff) %>%
  summarize(actual=n()/nrow(.)) %>% 
  inner_join(data.frame(goal_diff=-5:5,
                        pred=skellam::dskellam(-5:5,mean(df2018$HomeGoals),
                                               mean(df2018$AwayGoals))),
             by=c("goal_diff")) %>%
  ggplot(aes(x=as.factor(goal_diff))) + 
  geom_bar(aes(y = actual,fill="d2"), stat="identity") +
  geom_line(aes(y = pred, group = 1, color = "d1"), size=1.25) +
  scale_colour_manual(name="Skellam",labels="", values=c("d1" = "blue", "d2" = "red"))+
  scale_fill_manual(name="Actual",labels="",values="red")  +
  ggtitle("Difference in Goals Scored (Home Team vs Away Team)")  + xlab("Home Goals - Away Goals") + ylab("Proportion of Matches") +
  my_post_theme


mutate(df2017, goal_diff=HomeGoals-AwayGoals) %>% group_by(goal_diff) %>%
  summarize(actual=n()/nrow(.)) %>% 
  inner_join(data.frame(goal_diff=-5:5,
                        pred=skellam::dskellam(-5:5,mean(df2017$HomeGoals),
                                               mean(df2017$AwayGoals))),
             by=c("goal_diff")) %>%
  ggplot(aes(x=as.factor(goal_diff))) + 
  geom_bar(aes(y = actual,fill="d2"), stat="identity") +
  geom_line(aes(y = pred, group = 1, color = "d1"), size=1.25) +
  scale_colour_manual(name="Skellam",labels="", values=c("d1" = "blue", "d2" = "red"))+
  scale_fill_manual(name="Actual",labels="",values="red")  +
  ggtitle("Difference in Goals Scored (Home Team vs Away Team)")  + xlab("Home Goals - Away Goals") + ylab("Proportion of Matches") +
  my_post_theme


mutate(dfall, goal_diff=HomeGoals-AwayGoals) %>% group_by(goal_diff) %>%
  summarize(actual=n()/nrow(.)) %>% 
  inner_join(data.frame(goal_diff=-5:5,
                        pred=skellam::dskellam(-5:5,mean(dfall$HomeGoals),
                                               mean(dfall$AwayGoals))),
             by=c("goal_diff")) %>%
  ggplot(aes(x=as.factor(goal_diff))) + 
  geom_bar(aes(y = actual,fill="d2"), stat="identity") +
  geom_line(aes(y = pred, group = 1, color = "d1"), size=1.25) +
  scale_colour_manual(name="Skella m",labels="", values=c("d1" = "blue", "d2" = "red"))+
  scale_fill_manual(name="Actual",labels="",values="red")  +
  ggtitle("Difference in Goals Scored (Home Team vs Away Team)")  + xlab("Home Goals - Away Goals") + ylab("Proportion of Matches") +
  my_post_theme
