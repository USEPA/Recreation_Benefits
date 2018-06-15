setwd("L:/Public/Lyon/Visitation R/Clean Code")
meta <- read.csv("Meta_analysis.csv")

library("plm")
library("stargazer")
library("lmtest")

#Create interaction variables for quality attributes
meta$hasquality <- 0
meta$hasquality[meta$season_closed>=0]=1
meta$season_has <- meta$season_closed * meta$hasquality
meta$season_has[is.na(meta$season_has)] <- 0
meta$length_has <- meta$length_mi * meta$hasquality
meta$length_has[is.na(meta$length_has)] <- 0
meta$closedever <- ifelse(meta$season_closed > 0, 1,0)
meta$closedever[is.na(meta$closedever)] <- 0
meta$closedever_hasquality <- meta$closedever * meta$hasquality

#Mean
mean(meta$cs_day)

#Sample size weighted mean
meta_weight=meta[complete.cases(meta$samp_size),]
weighted.mean(meta_weight$cs_day, meta_weight$samp_size, na.rm=FALSE)

#without income
#for paper table, have to correct standard error in reg16c in table!
reg1 <- lm(logcs_day~salt+northeast, meta)
summary(reg1)
reg2 <- lm(logcs_day~salt+northeast+daytrip+resident, meta)
summary(reg2)
reg3 <- lm(logcs_day~salt+northeast+daytrip+resident+income, meta)
summary(reg3)
#Excluded income
reg4 <- lm(logcs_day~salt+northeast+daytrip+resident+hasquality+length_has+
            closedever_hasquality, meta)
summary(reg4)
AIC(reg1,reg2,reg3,reg4)
#Clustered by study with standard errors
reg4c <- plm(logcs_day~salt+northeast+daytrip+resident+hasquality+length_has+
              closedever_hasquality, data=meta,model="pooling")

G <- length(unique(meta$study_id)) 
N <- length(meta$study_id)
dfa <- (G/(G - 1)) * (N - 1)/reg4c$df.residual
study_c_vcov <- dfa * vcovHC(reg4c, type = "HC0", cluster = "group", adjust = T)
coeftest(reg4c,vcov = study_c_vcov)
waldtest(reg4c, vcov = study_c_vcov, test = "F")

#Export
#stargazer(reg1, reg2, reg4, reg4c,type="text", out="Meta_regressions.html")

#simulate for range of WTP

predict_wtp<- meta[1,c("salt","northeast","daytrip","resident","hasquality","length_has","closedever_hasquality")]

#policy variable levels
predict_wtp$salt <- 1
predict_wtp$northeast <- 1
predict_wtp$daytrip <- 1
predict_wtp$resident <- .49
predict_wtp$hasquality <- 1
predict_wtp$length_has <- .5 
predict_wtp$closedever_hasquality <- 1 

#get 1 standard erorr prediction interval
wtphat <- predict(reg4,newdata=predict_wtp,interval="prediction",level=.6827)
wtphat <- exp(wtphat)

#Create table for other region regressions

reg4_us<- lm(logcs_day~salt+daytrip+resident+hasquality+length_has+
               closedever_hasquality, meta)

reg4_mw <- lm(logcs_day~salt+midwest+daytrip+resident+hasquality+length_has+
             closedever_hasquality, meta)

reg4_s <- lm(logcs_day~salt+south+daytrip+resident+hasquality+length_has+
                closedever_hasquality, meta)

reg4_w <- lm(logcs_day~salt+west+daytrip+resident+hasquality+length_has+
                closedever_hasquality, meta)

#stargazer(reg4_us, reg4, reg4_mw, reg4_s, reg4_w,type="text", out="regionalregs.html")


