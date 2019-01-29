###calibration of cell data for NE Beaches###

## Authors: Nate Merrill, Sarina Atkinson (aka. Sarina Lyon) ##
## Contact: merrill.nathaniel@epa.gov ##

rm(list=ls()) #clear all

#load needed packages
library(lubridate)
library(ggplot2)
library(dplyr)
library(stargazer)
library(randomForest)
library(Cairo)

#####Load Data#####

#set directory with input data#

setwd("C:/Users/nmerri02/Desktop/GitHub/Rec_Demand/Cell data project") #change this as needed
load("cellcalibration.RData") #adjust this if needed

#fit stats function for comparing models
fitstats = function(r,y){
  rmse=sqrt(mean((r)^2))
  me= mean(r)
  mae= mean(abs(r))
  mape = mean(abs(r/y))*100
  rsqrd= 1 - ( sum(r^2) / sum( (y-mean(y))^2 ) )
  result=list(me=me,rmse=rmse,mae=mae,mape=mape,rsqrd=rsqrd)
  return(result)
}

####Scatter Plots###########
#create size and data source variables
all_ppltest=all[!is.na(all$ppl),]
all_ppltest$size <- ifelse(all_ppltest$Beach_Name=="narragansett town beach", "Large", ifelse(all_ppltest$Beach_Name=="covells beach"|all_ppltest$Beach_Name=="dowses beach", "Medium", "Small"))
all_ppltest$size <- factor(all_ppltest$size, c("Small", "Medium", "Large"))
all_ppltest$source <- ifelse(all_ppltest$Beach_Name=="narragansett town beach", "Narr", ifelse(all_ppltest$Beach_Name=="covells beach"|all_ppltest$Beach_Name=="dowses beach"|all_ppltest$Beach_Name=="john's pond (public)"|all_ppltest$Beach_Name=="mill creek"|all_ppltest$Beach_Name=="hamblin pond"|all_ppltest$Beach_Name=="wequaquet lake town", "Town", "3Bays"))
all_ppltest %>% group_by(source) %>% summarise(n=n())

#fix for Narragansett Beach counts (see appendix for public to private count correctiond discussion)
gansettfactor=.85 
all_ppltest$ppl[all_ppltest$source=="Narr"] <- all_ppltest$ppl[all_ppltest$source=="Narr"]*(1+gansettfactor)

#plot season time series of observations of visitation for Narragansett Beach
png(filename = "L:/Public/SHC 4.61/Economics/Cell data project/Manuscript/Figs and Tables/Narr Season Raw.png", #adjust this as needed
    type="cairo", units = "in", width = 10, height = 6, pointsize = 16, res = 300)
narr_plot <- ggplot(all_ppltest[all_ppltest$source=="Narr", ], aes(date2, ppl)) + geom_line(color="#3B9AB2", size=1) +
  labs(x="Date", y="Visits", title="Narragansett Beach Visitation for Summer 2017") + 
  theme_classic() + theme(plot.title = element_text(hjust=0.5), text = element_text(size=14))
narr_plot
dev.off()

#cell to people scatter plots
png(filename = "L:/Public/SHC 4.61/Economics/Cell data project/Manuscript/Figs and Tables/Raw scatter 2.png", 
    type="cairo", units = "in", width = 8, height = 6, pointsize = 16, res = 300)
a <- ggplot(all_ppltest, aes(cell_9to3, ppl)) + 
  xlim(0, 20000) + ylim(0, 6000) + 
  geom_point(aes(colour = factor(size, labels=c("Small - Three Bays access points","Medium - Barnstable Town beaches","Large - Narragansett Town Beach")))) +
  labs(x="Cell Devices",y="Onsite Counts of People", title="Comparison of Observations and CDR data", color="") +
  theme_light() + theme(legend.position="bottom", legend.direction = "horizontal", legend.title=element_blank(), plot.title = element_text(hjust = 0.5), text = element_text(size=12)) + 
  scale_color_manual(values=c("#F22F0F", "#E1BD6C", "#342749")) 
a
dev.off()

####Fit Random Forest ################
ntrees=2000

library(ranger)
ppl_comp <- all_ppltest[, c("cell_9to3", "ppl","Area_sq_m", "temp", "wind", "precip", "jun","jul", "aug", "tue", "wed", "thu", "fri", "sat", "sun", "wkend","source","POI","date2")]
ppl_comp$source=as.factor(ppl_comp$source)

ppl_comp$ln_ppl=log(ppl_comp$ppl)

set.seed(1)
floor(sqrt(ncol(ppl_comp)-1))

rf1 <- ranger(ppl ~ ., data = ppl_comp[1:17],num.trees=ntrees,mtry=10,keep.inbag=TRUE,importance = "impurity",respect.unordered.factors=TRUE)
  
print(rf1)

#importance plot
imp=data.frame(importance(rf1))
imp <- add_rownames(imp, "variable")
ggplot(imp, aes(x=reorder(variable,importance.rf1.), y=importance.rf1.,fill=importance.rf1.))+ 
  geom_bar(stat="identity", position="dodge")+ coord_flip()+
  ylab("Variable Importance")+
  xlab("")+
  ggtitle("Information Value Summary")+
  guides(fill=F)+
  scale_fill_gradient(low="red", high="blue")

#partial plots
library(pdp) 
partial(rf1, pred.var = "source", plot = TRUE, rug = TRUE)
partial(rf1, pred.var = "cell_9to3", plot = TRUE, rug = TRUE)
partial(rf1, pred.var = "Area_sq_m", plot = TRUE, rug = TRUE)

#test mtry (varaibles tried at each tree split) #

set.seed(1)
bound <- floor(nrow(ppl_comp)*.632)
samp_df <- sample(seq_len(nrow(ppl_comp)), size=bound, replace = FALSE)

train1 <- ppl_comp[samp_df, ]
test1 <- ppl_comp[-samp_df, ]

oob.err <- double(16)
test.err <- double(16)
mape.err <- double(16)
in.err <- double(16)
 
for (mtry in 1:16)
{
    rf <- ranger(ppl ~ ., data = train1[1:17],num.trees=ntrees,mtry=mtry,respect.unordered.factors=TRUE)
                 
    oob.err[mtry] <- rf$prediction.error
        
    pred <- predict(rf, test1)
    pred=pred$predictions
    
    pred2<- predict(rf,train1)
    pred2=pred2$predictions
    
    test.err[mtry] <- with(test1, mean((ppl-pred)^2))
    
    mape.err[mtry] <- with(test1, mean(abs((ppl-pred)/ppl)))
    
    in.err[mtry]<-with(train1,mean((ppl-pred2)^2))
    cat(mtry, " ")
}

matplot(1:mtry, cbind(oob.err,test.err,in.err), pch=19, col=c("#342749", "#13775E",2), 
        type="b", ylab="Mean Squared Error", 
        xlab="Number of Predictors Considered at Each Split",size=20)
legend("topright", legend=c("Out of Bag Error","Test Error","In-sample Error"), pch=19, col=c("#342749", "#13775E",2))

#### Test all models for best out of sample fit with k-fold cross validation ####

set.seed(50)
kfolds=10

ppl_comp<-ppl_comp[sample(nrow(ppl_comp)),] #scramble data order so that folds are a random pull
folds <- cut(seq(1,nrow(ppl_comp)),breaks=kfolds,labels=FALSE) #create folds

me1<- double(kfolds)
me1ln<- double(kfolds)
me2<- double(kfolds)
me2ln<- double(kfolds)
me3<- double(kfolds)
me3ln<- double(kfolds)
me4<- double(kfolds)
me4ln<- double(kfolds)
merf3<- double(kfolds)

rmse1<- double(kfolds)
rmse1ln<- double(kfolds)
rmse2<- double(kfolds)
rmse2ln<- double(kfolds)
rmse3<- double(kfolds)
rmse3ln<- double(kfolds)
rmse4<- double(kfolds)
rmse4ln<- double(kfolds)
rmserf3<- double(kfolds)

mae1<- double(kfolds)
mae1ln<- double(kfolds)
mae2<- double(kfolds)
mae2ln<- double(kfolds)
mae3<- double(kfolds)
mae3ln<- double(kfolds)
mae4<- double(kfolds)
mae4ln<- double(kfolds)
maerf3<- double(kfolds)

mape1<- double(kfolds)
mape1ln<- double(kfolds)
mape2<- double(kfolds)
mape2ln<- double(kfolds)
mape3<- double(kfolds)
mape3ln<- double(kfolds)
mape4<- double(kfolds)
mape4ln<- double(kfolds)
maperf3<- double(kfolds)

rsqrd1<- double(kfolds)
rsqrd1ln<- double(kfolds)
rsqrd2<- double(kfolds)
rsqrd2ln<- double(kfolds)
rsqrd3<- double(kfolds)
rsqrd3ln<- double(kfolds)
rsqrd4<- double(kfolds)
rsqrd4ln<- double(kfolds)
rsqrdrf3<- double(kfolds)

#position of metric out of fitstat function
me=1
rmse=2
mae=3
mape=4
rsqrd=5

for (k in 1:kfolds)
  {
    testIndexes <- which(folds==k,arr.ind=TRUE)
    test1 <- ppl_comp[testIndexes, ]
    train1 <- ppl_comp[-testIndexes, ]
    
    #create train and test sets
    ppl_comp_in=train1
    ppl_comp_out=test1
    
    #create regression models
    reg1=lm(ppl~cell_9to3,data=ppl_comp_in)
    reg1ln=lm(ln_ppl~cell_9to3,data=ppl_comp_in)
    reg2=lm(ppl~cell_9to3+Area_sq_m+source,data=ppl_comp_in)
    reg2ln=lm(ln_ppl~cell_9to3+Area_sq_m+source,data=ppl_comp_in)
    reg3 =lm(ppl~cell_9to3+Area_sq_m+source+jun+jul+aug+tue+wed+thu+fri+sat+sun, data=ppl_comp_in) 
    reg3ln =lm(ln_ppl~cell_9to3+Area_sq_m+source+jun+jul+aug+tue+wed+thu+fri+sat+sun, data=ppl_comp_in) 
    reg4=lm(ppl~cell_9to3+Area_sq_m+source+jun+jul+aug+tue+wed+thu+fri+sat+sun+temp+precip,data=ppl_comp_in)
    reg4ln=lm(ln_ppl~cell_9to3+Area_sq_m+source+jun+jul+aug+tue+wed+thu+fri+sat+sun+temp+precip,data=ppl_comp_in)
    
    #create random forest model
    rf3 <- ranger(ppl ~ ., data = ppl_comp_in[1:17], num.trees =ntrees, mtry=10,respect.unordered.factors=TRUE)
    
    #create errors on test set from each model
    r1=ppl_comp_out$ppl - predict(reg1,ppl_comp_out)
    r1ln=ppl_comp_out$ppl - exp(predict(reg1ln,ppl_comp_out))
    r2= ppl_comp_out$ppl - predict(reg2,ppl_comp_out)
    r2ln=ppl_comp_out$ppl - exp(predict(reg2ln,ppl_comp_out))
    r3= ppl_comp_out$ppl - predict(reg3,ppl_comp_out)
    r3ln=ppl_comp_out$ppl - exp(predict(reg3ln,ppl_comp_out))
    r4= ppl_comp_out$ppl - predict(reg4,ppl_comp_out)
    r4ln=ppl_comp_out$ppl - exp(predict(reg4ln,ppl_comp_out))
   
    predictranger=predict(rf3, data=ppl_comp_out)
    rf3_r= ppl_comp_out$ppl - predictranger$predictions
    
    
    #Mean Error
    me1[k]= as.numeric(fitstats(r1, ppl_comp_out$ppl)[me])
    me1ln[k]= as.numeric(fitstats(r1ln, ppl_comp_out$ppl)[me])
    me2[k]=  as.numeric(fitstats(r2,  ppl_comp_out$ppl)[me])
    me2ln[k]=  as.numeric(fitstats(r2ln,  ppl_comp_out$ppl)[me])
    me3[k]=  as.numeric(fitstats(r3,  ppl_comp_out$ppl)[me])
    me3ln[k]=  as.numeric(fitstats(r3ln,  ppl_comp_out$ppl)[me])
    me4[k]= as.numeric(fitstats(r4, ppl_comp_out$ppl)[me])
    me4ln[k]= as.numeric(fitstats(r4ln,  ppl_comp_out$ppl)[me])
    merf3[k]= as.numeric(fitstats(rf3_r,  ppl_comp_out$ppl)[me])
    
    #Root Mean Squared Error
    rmse1[k]= as.numeric(fitstats(r1, ppl_comp_out$ppl)[rmse])
    rmse1ln[k]= as.numeric(fitstats(r1ln, ppl_comp_out$ppl)[rmse])
    rmse2[k]=  as.numeric(fitstats(r2,  ppl_comp_out$ppl)[rmse])
    rmse2ln[k]=  as.numeric(fitstats(r2ln,  ppl_comp_out$ppl)[rmse])
    rmse3[k]=  as.numeric(fitstats(r3,  ppl_comp_out$ppl)[rmse])
    rmse3ln[k]=  as.numeric(fitstats(r3ln,  ppl_comp_out$ppl)[rmse])
    rmse4[k]= as.numeric(fitstats(r4, ppl_comp_out$ppl)[rmse])
    rmse4ln[k]= as.numeric(fitstats(r4ln,  ppl_comp_out$ppl)[rmse])
    rmserf3[k]= as.numeric(fitstats(rf3_r,  ppl_comp_out$ppl)[rmse])

    #Mean Absolute Error
    mae1[k]= as.numeric(fitstats(r1, ppl_comp_out$ppl)[mae])
    mae1ln[k]= as.numeric(fitstats(r1ln, ppl_comp_out$ppl)[mae])
    mae2[k]=  as.numeric(fitstats(r2,  ppl_comp_out$ppl)[mae])
    mae2ln[k]=  as.numeric(fitstats(r2ln,  ppl_comp_out$ppl)[mae])
    mae3[k]=  as.numeric(fitstats(r3,  ppl_comp_out$ppl)[mae])
    mae3ln[k]=  as.numeric(fitstats(r3ln,  ppl_comp_out$ppl)[mae])
    mae4[k]= as.numeric(fitstats(r4, ppl_comp_out$ppl)[mae])
    mae4ln[k]= as.numeric(fitstats(r4ln,  ppl_comp_out$ppl)[mae])
    maerf3[k]= as.numeric(fitstats(rf3_r,  ppl_comp_out$ppl)[mae])
    
    #Mean Absolute Percent Error
    mape1[k]= as.numeric(fitstats(r1, ppl_comp_out$ppl)[mape])
    mape1ln[k]= as.numeric(fitstats(r1ln, ppl_comp_out$ppl)[mape])
    mape2[k]=  as.numeric(fitstats(r2,  ppl_comp_out$ppl)[mape])
    mape2ln[k]=  as.numeric(fitstats(r2ln,  ppl_comp_out$ppl)[mape])
    mape3[k]=  as.numeric(fitstats(r3,  ppl_comp_out$ppl)[mape])
    mape3ln[k]=  as.numeric(fitstats(r3ln,  ppl_comp_out$ppl)[mape])
    mape4[k]= as.numeric(fitstats(r4, ppl_comp_out$ppl)[mape])
    mape4ln[k]= as.numeric(fitstats(r4ln,  ppl_comp_out$ppl)[mape])
    maperf3[k]= as.numeric(fitstats(rf3_r,  ppl_comp_out$ppl)[mape])
    
    #R-Squared #note R-sqared doesnt make much sense for non-linear models
    rsqrd1[k]= as.numeric(fitstats(r1, ppl_comp_out$ppl)[rsqrd])
    rsqrd1ln[k]= as.numeric(fitstats(r1ln, ppl_comp_out$ppl)[rsqrd])
    rsqrd2[k]=  as.numeric(fitstats(r2,  ppl_comp_out$ppl)[rsqrd])
    rsqrd2ln[k]=  as.numeric(fitstats(r2ln,  ppl_comp_out$ppl)[rsqrd])
    rsqrd3[k]=  as.numeric(fitstats(r3,  ppl_comp_out$ppl)[rsqrd])
    rsqrd3ln[k]=  as.numeric(fitstats(r3ln,  ppl_comp_out$ppl)[rsqrd])
    rsqrd4[k]= as.numeric(fitstats(r4, ppl_comp_out$ppl)[rsqrd])
    rsqrd4ln[k]= as.numeric(fitstats(r4ln,  ppl_comp_out$ppl)[rsqrd])
    rsqrdrf3[k]= as.numeric(fitstats(rf3_r,  ppl_comp_out$ppl)[rsqrd])
    cat(k, " ")
    }

#Fitstats for each model #note R-sqared doesnt make much sense for non-linear models
#Reg1
mean(me1)
mean(rmse1)
mean(mae1)
mean(mape1)
mean(rsqrd1)

#Reg1ln
mean(me1ln)
mean(rmse1ln)
mean(mae1ln)
mean(mape1ln)
mean(rsqrd1ln)

#Reg2
mean(me2)
mean(rmse2)
mean(mae3)
mean(mape2)
mean(rsqrd2)

#Reg2ln
mean(me2ln)
mean(rmse2ln)
mean(mae2ln)
mean(mape2ln)
mean(rsqrd2ln)

#Reg3
mean(me3)
mean(rmse3)
mean(mae3)
mean(mape3)
mean(rsqrd3)

#Reg3ln
mean(me3ln)
mean(rmse3ln)
mean(mae3ln)
mean(mape3ln)
mean(rsqrd3ln)

#Reg4
mean(me4)
mean(rmse4)
mean(mae4)
mean(mape4)
mean(rsqrd4)

#Reg4ln
mean(me4ln)
mean(rmse4ln)
mean(mae4ln)
mean(mape4ln)
mean(rsqrd4ln)

#Rf3
mean(merf3)
mean(rmserf3)
mean(maerf3)
mean(maperf3)
mean(rsqrdrf3)

#### plot best fit model (RF) # ()
rf4=ranger(ppl ~ ., data = ppl_comp[1:17],num.trees=ntrees,mtry=10,keep.inbag=TRUE,respect.unordered.factors=TRUE)

rf4p=predict(rf4,data = ppl_comp[1:17])
rf4p=rf4p$predictions
ppl_comp$rf4p=rf4p

#scatter plot
png(filename = "L:/Public/SHC 4.61/Economics/Cell data project/Manuscript/Figs and Tables/Predicted scatter 2.png", 
    type="cairo", units = "in", width = 8, height = 6, pointsize = 16, res = 300)
rf4_plot <- ggplot(ppl_comp, aes(rf4p, ppl)) + 
  xlim(0, 6000) + ylim(0,6000) + 
  geom_point(aes(colour = factor(source,levels=c("3Bays", "Town", "Narr"), labels=c("Small - Three Bays access points","Medium - Barnstable Town beaches","Large - Narragansett Town Beach")))) +
  labs(x="Estimated People",y="Onsite Counts of People", title="Comparison of Observations and Model", color="") +
  geom_abline(intercept =1, colour = "#C0C0C0") + theme_light() + 
  theme(legend.position="bottom", legend.direction = "horizontal", legend.title=element_blank(), plot.title = element_text(hjust=0.5), text = element_text(size=12)) +
  scale_color_manual(values =c("#F22F0F", "#E1BD6C", "#342749"))
rf4_plot
dev.off()

####predict whole set with best model#####

#fix for Narragansett Beach counts (see appendix for public to private count correctiond discussion)

forpredict=all
forpredict$size <- ifelse(forpredict$Beach_Name=="narragansett town beach", "Large", ifelse(forpredict$Beach_Name=="covells beach"|forpredict$Beach_Name=="dowses beach", "Medium", "Small"))
forpredict$size <- factor(forpredict$size, c("Small", "Medium", "Large"))
forpredict$source <- ifelse(forpredict$Beach_Name=="narragansett town beach", "Narr", ifelse(forpredict$Beach_Name=="covells beach"|forpredict$Beach_Name=="dowses beach"|forpredict$Beach_Name=="john's pond (public)"|forpredict$Beach_Name=="mill creek"|forpredict$Beach_Name=="hamblin pond"|forpredict$Beach_Name=="wequaquet lake town", "Town", "3Bays"))
forpredict$ppl[forpredict$source=="Narr"] <- forpredict$ppl[forpredict$source=="Narr"]*(1+gansettfactor)

forpredict <- forpredict[, c("cell_9to3", "ppl", "Area_sq_m", "temp", "wind", "precip", "jun","jul", "aug", "tue", "wed", "thu", "fri", "sat", "sun", "wkend","source","POI","date2")]
forpredict$source=as.factor(forpredict$source)


rf4=ranger(ppl ~ ., data = ppl_comp[1:17],num.trees=ntrees,mtry=10,keep.inbag=TRUE,respect.unordered.factors=TRUE)

#break into 4 so as not to break "predict" with rf from Ranger
brk=4
parts <-cut(seq(1,nrow(forpredict)),breaks=brk,labels=FALSE)

predict2=NULL
se2=NULL

for (i in 1:brk)
{
  Indexes <- which(parts==i,arr.ind=TRUE)
  predict=predict(rf4,forpredict[Indexes,],type="se",se.method = 'infjack',respect.unordered.factors=TRUE)
  predict2=rbind(predict2, data.frame(predict$predictions))
  se2=rbind(se2,data.frame(predict$se))
}

predict_rf1=cbind(predict2,se2) #ranger's prediction and errors
forpredict$prediction=predict_rf1$predict.predictions
forpredict$se=predict_rf1$predict.se

#####save daily visitation predictions#####
write.csv(forpredict, file="L:/Public/SHC 4.61/Economics/Cell data project/Manuscript/Figs and Tables/All Predictions_withpaper.csv")

####collapse by season and month#####
#dont need this for paper but nice to have for other uses

##by season###
#colnames(forpredict)
#cols= c("POI","ppl","date2","prediction","cell_9to3")
#seasonpredict= forpredict[,cols]
#seasonpredict<-seasonpredict %>% group_by(POI) %>% summarise_all(funs(sum(., na.rm=FALSE))) 

#save predicted output seasonal#
#write.csv(seasonpredict, file="seasonpredict.csv")

#### remaining figures and tables for paper#####

#plot predicted Narragansett Beach for whole season with SE
forpredict$plus_se <- forpredict$prediction+forpredict$se#*1.96
forpredict$minus_se <- forpredict$prediction-forpredict$se#*1.96
narr <- forpredict[forpredict$POI==4, ]
narr <- narr[order(as.Date(narr$date2)), ]


#with interval
png(filename = "L:/Public/SHC 4.61/Economics/Cell data project/Manuscript/Figs and Tables/Narr Season CI 2.png", 
    type="cairo", units = "in", width = 8, height = 6, pointsize = 16, res = 300)
narr_ploti <- ggplot(narr, aes(date2, plus_se)) + 
  geom_line(color="#808080", size=0.5) +
  geom_line(aes(y=minus_se), color="#808080", size=0.5) +
  geom_ribbon(data=data.frame(narr), aes(ymin=minus_se, ymax=plus_se), fill="grey") + 
  labs(x="Date", y="Predicted Visits", title="Narragansett Beach Predicted Visitation for Summer 2017") + 
  theme_classic() + theme(plot.title = element_text(hjust=0.5), text = element_text(size=12)) 
narr_ploti + geom_line(aes(y=prediction), color="#3B9AB2", size=0.75) 
dev.off()

#without interval and observed counts added as points
png(filename = "L:/Public/SHC 4.61/Economics/Cell data project/Manuscript/Figs and Tables/Narr Season 3 with legend.png", 
    type="cairo", units = "in", width = 8, height = 6, pointsize = 16, res = 300)
cols <- c("Predicted Visits"="#3B9AB2", "Observed Visits"="#205562")
narr_plot <- ggplot(narr, aes(x=date2)) + geom_line(aes(y=prediction,group=1, colour="Predicted Visits"), size=1) +
  geom_point(aes(y=ppl, group=1, colour="Observed Visits"), size=1.5) +
  labs(x="Date", y="Visits", title="Narragansett Beach Predicted Visitation for Summer 2017") + 
  theme_classic() + theme(plot.title = element_text(hjust=0.5), text = element_text(size=12), legend.position = "right") +
  scale_colour_manual(name="", values = cols) + 
  guides(colour=guide_legend(override.aes=list(linetype=c("blank", "solid"), shape=c(16,NA))))
narr_plot
dev.off()

## regression table output##
reg2=lm(ppl~cell_9to3+Area_sq_m+Area_sq_m+source,data=ppl_comp)
reg2ln=lm(ln_ppl~cell_9to3+Area_sq_m+source,data=ppl_comp)
reg3 =lm(ppl~cell_9to3+Area_sq_m+source+jun+jul+aug+tue+wed+thu+fri+sat+sun, data=ppl_comp) 
reg3ln =lm(ln_ppl~cell_9to3+Area_sq_m+source+jun+jul+aug+tue+wed+thu+fri+sat+sun, data=ppl_comp) 
reg4=lm(ppl~cell_9to3+Area_sq_m+source+jun+jul+aug+tue+wed+thu+fri+sat+sun+temp+precip,data=ppl_comp)
reg4ln=lm(ln_ppl~cell_9to3+Area_sq_m+source+jun+jul+aug+tue+wed+thu+fri+sat+sun+temp+precip,data=ppl_comp)

stargazer(reg1,reg1ln,reg4,reg4ln, type="html", out="L:/Public/SHC 4.61/Economics/Cell data project/Manuscript/Figs and Tables/regtable.html",ci.level=0.95,digits=3, digits.extra=1)
