library(MASS)
library(boot)
mag_annual = read.csv("C:/Users/aroy29/Dropbox (ASU)/Strava Analysis/Strava_Bias_Correction/Data/MAG_Train_Data.csv")
str(mag_annual)

#Scale the covariates to interpretable ranges

#Income in the order of $1000
mag_annual$MEDIAN_HOUSEHOLD_INCOME = (mag_annual$MEDIAN_HOUSEHOLD_INCOME)/10000
#Distance in miles
mag_annual$dist_to_residential_areas = (mag_annual$dist_to_residential_areas)*0.00062
mag_annual$dist_to_green_space = (mag_annual$dist_to_green_space)*0.00062
#Speed in the order of 10 kmph
mag_annual$Avg_segment_speed_limit = (mag_annual$Avg_segment_speed_limit)/10
mag_annual$PCT_WHITE_NONHISP = (mag_annual$PCT_WHITE_NONHISP)/10

str(mag_annual)

#Fit a GLM to the data using a Poisson Distribution
mag_model = glm(MAG_Daily~.,data=mag_annual[,-c(1,2)],family=poisson(link=log))
summary(mag_model)

'''

Call:
glm(formula = MAG_Daily ~ ., family = poisson(link = log), data = mag_annual[, 
-c(1, 2)])

Deviance Residuals: 
Min       1Q   Median       3Q      Max  
-8.1511  -2.8996  -0.4736   1.4005  12.8392  

Coefficients:
                            Estimate Std. Error z value Pr(>|z|)    
(Intercept)                3.778917   0.074907  50.448   <2e-16 ***
PCT_WHITE_NONHISP          0.110213   0.012455   8.849   <2e-16 ***
MEDIAN_HOUSEHOLD_INCOME   -0.094726   0.008277 -11.445   <2e-16 ***
Strava_Daily               0.167095   0.007300  22.891   <2e-16 ***
Avg_segment_speed_limit   -0.094548   0.008752 -10.803   <2e-16 ***
dist_to_residential_areas -0.511465   0.039927 -12.810   <2e-16 ***
dist_to_green_space       -0.734574   0.074200  -9.900   <2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for poisson family taken to be 1)

Null deviance: 3270.1  on 95  degrees of freedom
Residual deviance: 1343.5  on 89  degrees of freedom
AIC: 1832.9

Number of Fisher Scoring iterations: 5
'''

'''
Confidence Interval of coefficients

                                 2.5 %      97.5 %
(Intercept)                3.630890798  3.92453525
PCT_WHITE_NONHISP          0.008590694  0.01347314
MEDIAN_HOUSEHOLD_INCOME   -0.110983061 -0.07853685
Strava_Daily               0.152755868  0.18137172
Avg_segment_speed_limit   -0.111670904 -0.07736081
dist_to_residential_areas -0.590591336 -0.43404842
dist_to_green_space       -0.881099038 -0.59021442
> exp(-0.0947)

'''

#Exponents of model coefficients - to obtain observed counts from log link
coef_exp = data.frame(exp(coef(mag_model)))
coef_exp
'''
                          exp.coef.mag_model..
(Intercept)                         43.7686293
PCT_WHITE_NONHISP                    1.1165153
MEDIAN_HOUSEHOLD_INCOME              0.9096219
Strava_Daily                         1.1818667
Avg_segment_speed_limit              0.9097842
dist_to_residential_areas            0.4382596
dist_to_green_space                  0.3058084
'''

#Use negative binomial to tackle the problem of heteroskedasticity
#Poisson GLM assumes that the mean and variance of the predictor varibles are constant
#Since this is not ideally the case the negative binomial is a better predictor of the estimates.
mag_model2 <- glm.nb(MAG_Daily~.,data=mag_annual[,-c(1,2)])
summary(mag_model2)

'''
Call:
glm.nb(formula = MAG_Daily ~ ., data = mag_annual[, -c(1, 2)], 
init.theta = 2.419003708, link = log)

Deviance Residuals: 
Min       1Q   Median       3Q      Max  
-2.6721  -0.9022  -0.2064   0.3264   2.2742  

Coefficients:
Estimate Std. Error z value Pr(>|z|)    
(Intercept)                3.60406    0.28637  12.585  < 2e-16 ***
PCT_WHITE_NONHISP          0.09618    0.04449   2.162 0.030618 *  
MEDIAN_HOUSEHOLD_INCOME   -0.08877    0.03329  -2.667 0.007664 ** 
Strava_Daily               0.16171    0.03262   4.957 7.16e-07 ***
Avg_segment_speed_limit   -0.05669    0.03607  -1.572 0.116032    
dist_to_residential_areas -0.75870    0.19605  -3.870 0.000109 ***
dist_to_green_space       -0.92629    0.40043  -2.313 0.020709 *  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for Negative Binomial(2.419) family taken to be 1)

Null deviance: 193.29  on 95  degrees of freedom
Residual deviance: 102.25  on 89  degrees of freedom
AIC: 826.75

Number of Fisher Scoring iterations: 1


Theta:  2.419 
Std. Err.:  0.369 

2 x log-likelihood:  -810.747 
'''

#Lets check how the coefficients vary in this case
nb_coef_exp = data.frame(exp(coef(mag_model2)))
colnames(nb_coef_exp) = c("Exp(coefficients)")
nb_coef_exp

'''
                         Exp(coefficients)
(Intercept)                      36.7470049
PCT_WHITE_NONHISP                 1.1009626
MEDIAN_HOUSEHOLD_INCOME           0.9150571
Strava_Daily                      1.1755172
Avg_segment_speed_limit           0.9448843
dist_to_residential_areas         0.4682763
dist_to_green_space               0.3960187

'''

#Sample data for model interpretation
PCT_WHITE_NONHISP = c(65,0,0,0,0,0,66,0,0,0,0,0)
MEDIAN_HOUSEHOLD_INCOME = c(0,41.2,0,0,0,0,0,42.2,0,0,0,0)
Strava_Daily = c(0,0,5,0,0,0,0,0,6,0,0,0)
Avg_segment_speed_limit = c(0,0,0,7,0,0,0,0,0,8,0,0)
dist_to_residential_areas = c(0,0,0,0,2.5,0,0,0,0,0,3.5,0)
dist_to_green_space = c(0,0,0,0,0,1.5,0,0,0,0,0,2.5)

df1 = data.frame(PCT_WHITE_NONHISP[1:6],MEDIAN_HOUSEHOLD_INCOME[1:6],Strava_Daily[1:6],
                Avg_segment_speed_limit[1:6],dist_to_residential_areas[1:6],
                dist_to_green_space[1:6])
colnames(df1) = c("PCT_WHITE_NONHISP","MEDIAN_HOUSEHOLD_INCOME","Strava_Daily",
                  "Avg_segment_speed_limit","dist_to_residential_areas",
                  "dist_to_green_space")
df1
df2 = data.frame(PCT_WHITE_NONHISP[7:12],MEDIAN_HOUSEHOLD_INCOME[7:12],Strava_Daily[7:12],
                 Avg_segment_speed_limit[7:12],dist_to_residential_areas[7:12],
                 dist_to_green_space[7:12])
colnames(df2) = c("PCT_WHITE_NONHISP","MEDIAN_HOUSEHOLD_INCOME","Strava_Daily",
                  "Avg_segment_speed_limit","dist_to_residential_areas",
                  "dist_to_green_space")
df2

#Predict observed counts for each variable holding other covariates constant
pred_PCT_WHITE1 = predict(mag_model,df1[1,],type="response")
pred_MED_HH_INC1 = predict(mag_model,df1[2,],type="response")
pred_strava1 = predict(mag_model,df1[3,],type="response")
pred_Avg_Speed1 = predict(mag_model,df1[4,],type="response")
pred_dist_res1 = predict(mag_model,df1[5,],type="response")
pred_dist_green1 = predict(mag_model,df1[6,],type="response")

#Increase the value of each variable by 1 unit and predict again 
pred_PCT_WHITE2 = predict(mag_model,df2[1,],type="response")
pred_MED_HH_INC2 = predict(mag_model,df2[2,],type="response")
pred_strava2 = predict(mag_model,df2[3,],type="response")
pred_Avg_Speed2 = predict(mag_model,df2[4,],type="response")
pred_dist_res2 = predict(mag_model,df2[5,],type="response")
pred_dist_green2 = predict(mag_model,df2[6,],type="response")

#Compare the ratio of preicted counts for one unit increase to base predicted counts 
ratio_pred_PCT_WH = pred_PCT_WHITE2/pred_PCT_WHITE1
ratio_pred_MED_INC = pred_MED_HH_INC2/pred_MED_HH_INC1
ratio_pred_strava = pred_strava2/pred_strava1
ratio_pred_Avg_spd = pred_Avg_Speed2/pred_Avg_Speed1
ratio_pred_dist_res = pred_dist_res2/pred_dist_res1
ratio_pred_dist_green = pred_dist_green2/pred_dist_green1

pred_ratio = data.frame(ratio_pred_PCT_WH,ratio_pred_MED_INC,ratio_pred_strava,
                        ratio_pred_Avg_spd,ratio_pred_dist_res,ratio_pred_dist_green)

coef_exp
pred_ratio


#Predict counts using the fitted model
mag_annual$ypred_glm = predict(mag_model,mag_annual[,-c(1,7)],type = "response")


# Predicted vs observed counts - MAG : In sample fit plot
#mag_annual = mag_annual[mag_annual$MAG_Daily<100,]
True_counts = mag_annual$MAG_Daily
Predicted_counts = mag_annual$ypred_glm
plot(True_counts,Predicted_counts,pch=16,main="Poisson Model Fit for MAG",xlab="Observed MAG Counts",ylab ="Predicted Counts")
lmfit = lm(Predicted_counts~True_counts)
r2 = format(summary(lmfit)$adj.r.squared, digits=3)
coef = format(lmfit$coefficients[1],digits=3)
abline(lmfit,col="red",lwd=3)
legend("topleft", bty = "n", legend = paste("R-squared:", r2,"\nSlope:",coef ) )



#Prediction accuracy using Tempe counts data from TBAG

tbag_data = read.csv("C:/Users/aroy29/Dropbox (ASU)/Strava Analysis/Strava_Bias_Correction/Data/Tempe/Tempe_test_data.csv")
str(tbag_data)
tbag_data = tbag_data[,-c(1)]
str(tbag_data)

#Scale the covariates to interpretable ranges

#Income in the order of $1000
tbag_data$MEDIAN_HOUSEHOLD_INCOME = (tbag_data$MEDIAN_HOUSEHOLD_INCOME)/10000
tbag_data$dist_to_residential_areas = (tbag_data$dist_to_residential_areas)*1000
tbag_data$dist_to_green_space = (tbag_data$dist_to_green_space)*1000
#Speed in the order of 10 kmph
tbag_data$Avg_segment_speed_limit = (tbag_data$Avg_segment_speed_limit)/10
tbag_data$PCT_WHITE_NONHISP = (tbag_data$PCT_WHITE_NONHISP)/10
str(tbag_data)

tbag_data$ypred_glm = floor(predict(mag_model,tbag_data[,c(-1)],type = "response"))
tbag_data$diff_counts = abs(round(tbag_data$MAG_Daily - tbag_data$ypred_glm,0))
tbag_data_sorted = tbag_data[order(tbag_data$diff_counts),]
tbag_data_sorted
head(tbag_data_sorted)
nrow(tbag_data_sorted)

setwd("C:/Users/aroy29/Dropbox (ASU)/Strava Analysis/Strava_Bias_Correction/Results/Tempe")
write.csv(tbag_data,"Tempe_Bias_corrected_AADB_Feb25_2019.csv")
tbag_data$Count = 1
tbag_predicted_counts_grouped = aggregate(Count~diff_counts,data=tbag_data,FUN=sum)
tbag_predicted_counts_grouped$cum_pct_Count = 100*cumsum(tbag_predicted_counts_grouped$Count)/sum(tbag_predicted_counts_grouped$Count)
tbag_predicted_counts_grouped$label = ""
tbag_predicted_counts_grouped$label[tbag_predicted_counts_grouped$diff_counts==10] <- paste(round(tbag_predicted_counts_grouped$cum_pct_Count[tbag_predicted_counts_grouped$diff_counts==10],0),"%")
tbag_predicted_counts_grouped$label[tbag_predicted_counts_grouped$diff_counts==30] <- paste(round(tbag_predicted_counts_grouped$cum_pct_Count[tbag_predicted_counts_grouped$diff_counts==30],0),"%")
tbag_predicted_counts_grouped$label[tbag_predicted_counts_grouped$diff_counts==58] <- paste(round(tbag_predicted_counts_grouped$cum_pct_Count[tbag_predicted_counts_grouped$diff_counts==58],0),"%")
tbag_predicted_counts_grouped$label[tbag_predicted_counts_grouped$diff_counts==74] <- paste(round(tbag_predicted_counts_grouped$cum_pct_Count[tbag_predicted_counts_grouped$diff_counts==74],0),"%")
tbag_predicted_counts_grouped$label[tbag_predicted_counts_grouped$diff_counts==113] <- paste(round(tbag_predicted_counts_grouped$cum_pct_Count[tbag_predicted_counts_grouped$diff_counts==113],0),"%")
tbag_predicted_counts_grouped$label[tbag_predicted_counts_grouped$diff_counts==152] <- paste(round(tbag_predicted_counts_grouped$cum_pct_Count[tbag_predicted_counts_grouped$diff_counts==152],0),"%")
tbag_predicted_counts_grouped$label[tbag_predicted_counts_grouped$diff_counts==209] <- paste(round(tbag_predicted_counts_grouped$cum_pct_Count[tbag_predicted_counts_grouped$diff_counts==209],0),"%")
head(tbag_predicted_counts_grouped)

plot(tbag_predicted_counts_grouped$diff_counts,tbag_predicted_counts_grouped$cum_pct_Count,
     main="Overall Model Prediction Accuracy for Tempe",
     xlab="Difference in observed vs predicted counts",
     ylab = "% of predicted segments",pch=16,cex=1.25,type="p",col="black")
lines(tbag_predicted_counts_grouped$diff_counts,tbag_predicted_counts_grouped$cum_pct_Count,col="blue")
text(tbag_predicted_counts_grouped$diff_counts,tbag_predicted_counts_grouped$cum_pct_Count, labels=tbag_predicted_counts_grouped$label, cex= 0.85,pos=2)

# Quantify relationship between top 10% and bottom 10% segment for TBAG with GIS covariates
tbag_data_sorted = tbag_data[order(tbag_data[,c(13)]),]
tbag_data_sorted = aggregate(.~diff_counts,data=tbag_data_sorted,FUN=mean)
tbag_data_sorted$Pct_Count = 100*cumsum(tbag_data_sorted$Count)/sum(tbag_data_sorted$Count)

# ridership volume difference values for edge ids
rows = floor(0.1*nrow(tbag_data_sorted))
top10pct_segments = subset(tbag_data_sorted,diff_counts>0 & diff_counts<30)
top10pct_segments
bottom10pct_segments = subset(tbag_data_sorted,diff_counts>100)
bottom10pct_segments
#Visualize mean of GIS covariates with top 10% and bottom 10% predicted segments
par(mfrow=c(3,3))
plot(top10pct_segments$diff_counts,top10pct_segments$AADT,
     xlab = "Difference between observed and predicted counts",
     ylab = "Mean Annual Traffic Volume")
lines(loess.smooth(top10pct_segments$diff_counts,top10pct_segments$AADT),col="blue",type="l",lwd=2)
plot(top10pct_segments$diff_counts,top10pct_segments$dist_to_residential_areas,
     xlab = "Difference between observed and predicted counts",
     ylab = "Mean distance to residential areas",
     main = "Variation of GIS covariates with most accurately predicted segments")
lines(loess.smooth(top10pct_segments$diff_counts,top10pct_segments$dist_to_residential_areas),col="blue",type="l",lwd=2)
plot(top10pct_segments$diff_counts,top10pct_segments$Avg_segment_speed_limit,
     xlab = "Difference between observed and predicted counts",
     ylab = "Mean Segment Speed Limit")
lines(loess.smooth(top10pct_segments$diff_counts,top10pct_segments$PCT_WHITE_NONHISP),col="blue",type="l",lwd=2)


plot(bottom10pct_segments$diff_counts,bottom10pct_segments$AADT,
     xlab = "Difference between observed and predicted counts",
     ylab = "Mean Annual Traffic Volume")
lines(loess.smooth(bottom10pct_segments$diff_counts,bottom10pct_segments$AADT),col="blue",type="l",lwd=2)
plot(bottom10pct_segments$diff_counts,bottom10pct_segments$dist_to_residential_areas,
     xlab = "Difference between observed and predicted counts",
     ylab = "Mean distance to residential areas",
     main = "Variation of GIS covariates with least accurately predicted segments")
lines(loess.smooth(bottom10pct_segments$diff_counts,bottom10pct_segments$dist_to_residential_areas),col="blue",type="l",lwd=2)
plot(bottom10pct_segments$diff_counts,bottom10pct_segments$Avg_segment_speed_limit,
     xlab = "Difference between observed and predicted counts",
     ylab = "Mean Segment Speed Limit")
lines(loess.smooth(bottom10pct_segments$diff_counts,bottom10pct_segments$PCT_WHITE_NONHISP),col="blue",type="l",lwd=2)

#Generate boxplots for top 10% and bottom 10% predicted segments
boxplot(100 * (cumsum(diff_counts)/sum(diff_counts)) ~ Avg_segment_speed_limit, data=tbag_data_sorted,xlab = "Average Segment Speed Limit (kmph)",ylab = "%Error")
boxplot(100 * (cumsum(diff_counts)/sum(diff_counts)) ~ MEDIAN_HOUSEHOLD_INCOME, data=tbag_data_sorted,xlab = "Median Household Income (US Dollars)",ylab = "%Error in Prediction")

top_10_pct_segments = subset(tbag_data_sorted,diff_counts<=15)
top_10_pct_segments$Accuracy = "High"
bottom_10_pct_segments = subset(tbag_data_sorted,diff_counts>=100 & diff_counts<=200)
bottom_10_pct_segments$Accuracy = "Low"

df = rbind(top_10_pct_segments,bottom_10_pct_segments)

library(ggplot2)
library(gridExtra)
par(mfrow=c(2,3))
g1 <- ggplot(df, aes(x=diff_counts, y=Avg_segment_speed_limit, fill=Accuracy)) + 
  geom_boxplot() + labs(x="Absolute difference in true vs predicted counts", y = "Average Segment Speed Limit")
g2 <- ggplot(df, aes(x=diff_counts, y=MEDIAN_HOUSEHOLD_INCOME, fill=Accuracy)) + 
  geom_boxplot() + labs(x="Absolute difference in true vs predicted counts", y = "Median Household Income (in 1000 dollars)")
g3 <- ggplot(df, aes(x=diff_counts, y=dist_to_residential_areas, fill=Accuracy)) + 
  geom_boxplot() + labs(x="Absolute difference in true vs predicted counts", y = "Distance From Residential Areas (in meters)")
g4 <- ggplot(df, aes(x=diff_counts, y=dist_to_green_space, fill=Accuracy)) + 
  geom_boxplot() + labs(x="Absolute difference in true vs predicted counts", y = "Distance From Green Spaces (in meters)")
g5 <- ggplot(df, aes(x=diff_counts, y=PCT_WHITE_NONHISP, fill=Accuracy)) + 
  geom_boxplot() + labs(x="Absolute difference in true vs predicted counts", y = "% White Population")
g6 <- ggplot(df, aes(x=diff_counts, y=Strava_Daily, fill=Accuracy)) + 
  geom_boxplot() + labs(x="Absolute difference in true vs predicted counts", y = "Avg no of Strava riders")

grid.arrange(g1,g2,g3,g4,g5,g6, nrow = 2)
