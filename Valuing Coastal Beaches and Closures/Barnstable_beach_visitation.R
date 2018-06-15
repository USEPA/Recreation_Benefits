setwd("L:/Public/Lyon/Visitation R/Clean Code")
beaches <- read.csv("Barnstable_beaches.csv")

#R packages needed
library("dplyr")
library("plm") 
library("lme4")
library("merTools")
library("tidyr")
library("stargazer")

#Remove NAs
beaches_2 <- na.omit(beaches)

#Format for "plm" package
rownames(beaches_2) <- c()
beaches_2 <- plm.data(beaches_2,index=c("variable"))
#Pooled OLS 
pooled <- plm(value~parking+temp+precip+rainday+wind+holiday+wkend+advisory+jun+jul+aug+sep+yr2012+yr2013+yr2014+yr2015+yr2016, data=beaches_2, model="pooling")
summary(pooled)
#Breusch-Pagan test for using pooled model
plmtest(pooled, effect="individual", type="bp") #reject null
#Fixed effects
fixed <- plm(value~temp+precip+rainday+wind+holiday+wkend+advisory+jun+jul+aug+sep+yr2012+yr2013+yr2014+yr2015+yr2016, data=beaches_2, model="within")
summary(fixed)
#Random effects
random_plm <- plm(value~parking+temp+precip+rainday+wind+holiday+wkend+advisory+jun+jul+aug+sep+yr2012+yr2013+yr2014+yr2015+yr2016, data=beaches_2, model="random")
summary(random_plm)
#Hausman test to compare fixed v random effects
phtest(random_plm, fixed) #no sig diff
#display table comparing three pandel models for Supplementary Materials
#stargazer(pooled, fixed, random_plm, type="text", out="Compare panel models.html")

#RMSE
fitted_p <- as.numeric(pooled$model[[1]] - pooled$residuals) 
fitted_fe<- as.numeric(fixed$model[[1]] - fixed$residuals) 
fitted_re <- as.numeric(random_plm$model[[1]] - random_plm$residuals) 

rmse <- function(error)
{sqrt(mean(error^2))}

error_p  =beaches_2$value-fitted_p
error_fe =beaches_2$value-fitted_fe
error_re =beaches_2$value-fitted_re

rmse_p  = rmse(error_p)
rmse_fe = rmse(error_fe)
rmse_re = rmse(error_re)


#Random effects using lmer model to make predictions
random_lm4 <- lmer(value ~ parking+temp+precip+rainday+wind+advisory+holiday+wkend+jun+jul+aug+sep+yr2012+yr2013+yr2014+yr2015+yr2016 + (1 | variable), data=beaches_2, REML=FALSE)
summary(random_lm4)

#Predict for by type of day where weather and year held constant
predict_type <- beaches_2[,c("variable", "parking", "length", "may", "jun", "jul", "aug", "sep", "holiday", "wkend","date")] 
predict_type$temp <- 79
predict_type$wind <- 3.608379
predict_type$rainday <- 0
predict_type$precip <- 0
predict_type$advisory <- 0
predict_type$yr2011 <- 0 
predict_type$yr2012 <- 0 
predict_type$yr2013 <- 0 
predict_type$yr2014 <- 0 
predict_type$yr2015 <- 1 
predict_type$yr2016 <- 0
predict_1 <- predict(random_lm4, newdata=predict_type, allow.new.levels=T) 
predict_1 <- as.numeric(predict_1)
predict_table <- beaches_2[,c("variable", "may", "jun", "jul", "aug", "sep", "holiday", "wkend")] 
predict_table$predict_log <- predict_1
#Add in re for the in-sample prediction
re=ranef(random_lm4)$variable
predict_table$predict_log[predict_table$variable=='lncraig_t']=predict_table$
  predict_log[predict_table$variable=='lncraig_t']+re$`(Intercept)`[1]
predict_table$predict_log[predict_table$variable=='lnkal_t']=predict_table$
  predict_log[predict_table$variable=='lnkal_t']+re$`(Intercept)`[2]
predict_table$predict_log[predict_table$variable=='lnsea_t']=predict_table$
  predict_log[predict_table$variable=='lnsea_t']+re$`(Intercept)`[3]
predict_table$predict_log[predict_table$variable=='lnvet_t']=predict_table$
  predict_log[predict_table$variable=='lnvet_t']+re$`(Intercept)`[4]
predict_table$car <- exp(predict_table$predict_log)
predict_table$people <- predict_table$car*3
#Export
#write.csv(predict_table, file="Predicted by type of day.csv")

#Predict for type of day with range of values to present
set.seed(50)
predict_range <- predictInterval(random_lm4, newdata=predict_type, n.sims=2000, level=0.68) 
#to make sure predictInterval and prediction functions are equivalent
compare <- predict_range$fit - predict_table$predict_log
plot(compare)
mean(compare) #yes
predict_range <- exp(predict_range)
predict_range <- predict_range *3
x <- predict_type[,c("variable", "may", "jun", "jul", "aug", "sep", "holiday", "wkend","date", "yr2015")] 
predict_range <- cbind(predict_range, x)
#Export
#write.csv(predict_range, file="Predict by type of day ranges.csv")

#To get number of days within each season 
year_days <- aggregate(beaches$variable, by=list(Category=beaches$year), FUN=length)
year_days$season_days <- year_days$x/4

#Predict for whole season 
average_table2<-data.frame()
list1<-list()
for(i in 1:100){
  
  
  predict_season <- predictInterval(random_lm4, newdata=beaches, 
                                    n.sims=2000, level=0.68) %>% exp()
  predict_season <- predict_season *3
  z <- beaches[,c("variable", "may", "jun", "jul", "aug", "sep", "holiday", "wkend", "year","date")] 
  predict_table <- cbind(predict_season, z)
  #Subset to get only full years (2012-2015)
  table(beaches$year)
  predict_table <- predict_table[order(predict_table$year, predict_table$variable), ]
  predict_table4yr <- predict_table[-c(1:272, 1980:2120),]
  #Reorder back to group by only variable (beachname)
  predict_table4yr <- predict_table4yr[order(predict_table4yr$variable), ]
  #Get average total by beach (lower, fit, and upper bounds)
  average_lwr <- aggregate(predict_table4yr$lwr, by=list(Category=predict_table4yr$variable), FUN=sum)
  average_fit <- aggregate(predict_table4yr$fit, by=list(Category=predict_table4yr$variable), FUN=sum)
  average_upr <- aggregate(predict_table4yr$upr, by=list(Category=predict_table4yr$variable), FUN=sum)
  average_lwr$lwr <- average_lwr$x/4
  average_fit$fit <- average_fit$x/4
  average_upr$upr <- average_upr$x/4
  average_table <- average_lwr[,c("Category", "lwr")] 
  average_table$fit <- average_fit$fit
  average_table$upr <- average_upr$upr
  
  average_table2<-rbind(average_table2,average_table)
  
  list1[[i]]<-average_table
  
  print(i)
  
  
}

#Summarize loop to get mean season visitation based on iterations
summary_season <- aggregate(average_table2, by=list(Category=average_table2$Category), FUN=mean)



