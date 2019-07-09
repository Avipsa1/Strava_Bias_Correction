#Import required libraries for LASSO analysis using scikit-learn package

import time
from sklearn.linear_model import RandomizedLasso,LassoCV,LassoLarsIC
import pandas as pd
import numpy as np
import os
import matplotlib.pyplot as plt
import seaborn as sns

#Set the working directory
os.chdir("C:/Users/aroy29/Dropbox (ASU)/Strava Analysis/Strava_Bias_Correction/Data/MAG/")

#Read the list of covariates for the MAG count locations
df = pd.read_csv("MAG_bike_counts_with_covariates.csv")
#List the column names
df.columns

#In the training data use all the covariates with VIF < 7.5
#Use variables that have VIF <8 to avoid multicollinearity
VIF_features = ['edge_id','AADT',
 'Avg_segment_speed_limit',
 'Count_Month',
 'EDU_ABV_HS',
 'MEDIAN_HOUSEHOLD_INCOME',
 'PCT_BICYCLE',
 'PCT_VETERANS',
 'PCT_WHITE_NONHISP',
 'Pop_per_sq_mile',
 'Strava_Daily',
 'dist_to_commercial_areas',
 'dist_to_green_space',
 'dist_to_residential_areas']
#Group data based on edge_id
X = df[VIF_features]
Y = df["MAG_Daily"]

#Define plot function for AIC and BIC criterion
def plot_ic_criterion(model,name,color):
    alpha_ = model.alpha_
    alphas_ = model.alphas_
    criterion_ = model.criterion_
    plt.plot(alphas_, criterion_, '--', color=color,
             linewidth=3, label='%s criterion' % name)
    plt.axvline(alpha_, color=color, linewidth=3,
                label='alpha: %s estimate' % name)
    plt.xlabel('alpha')
    plt.ylabel('criterion')

#Choose the optimal alpha for LASSO regression using the AIC & BIC criterion
model_bic = LassoLarsIC(criterion='bic')
t1 = time.time()
model_bic.fit(X, Y)
t_bic = time.time() - t1
alpha_bic_ = model_bic.alpha_

model_aic = LassoLarsIC(criterion='aic')
model_aic.fit(X, Y)
alpha_aic_ = model_aic.alpha_

plt.figure()
plot_ic_criterion(model_aic, 'AIC', 'b')
plot_ic_criterion(model_bic, 'BIC', 'r')
plt.legend()
plt.title('Information-criterion for model selection (training time %.3fs)'% t_bic)

#Use the Randomized LASSO module to choose the best alpha representing the data
names = ['Edge Id','Average Annual Traffic Volume','Average Segment Speed Limit',
        '% Population with atleast High School Education',
        'Median Household Income','%Veterans Population',
        '%White Population','Population Density','No. of Strava Riders',
        'Distance to commercial areas', 'Distance to green spaces','Distance to residential areas']
y=0
alpha,scores = [],[]
for i in np.arange(0.2,0.25,2.25):
    y= y+1
    rlasso = RandomizedLasso(alpha=i)
    rlasso = rlasso.fit(X, Y)
    #print(names)
    alpha.append(i)
    pd.DataFrame(sorted(zip(map(lambda x: round(x, 4), rlasso.scores_), 
                 names), reverse=True)).to_csv("LASSO_"+str(y)+"results.csv")
print(alpha)

# Using the chosen alpha after cross-validation, plot the selected variables with LASSO scores
rlasso = RandomizedLasso(alpha=1.21)
rlasso = rlasso.fit(X, Y)
df=pd.DataFrame(sorted(zip(map(lambda x: round(x, 4), rlasso.scores_), 
                 names), reverse=True))
df.columns = ["Rank","Variable"]
sns.set(rc={'figure.figsize':(15,10)},font_scale=2,style='white')
sns.barplot(x="Rank",y="Variable",data=df,color="black").figure.savefig("LASSO_Output.png",dpi=300)
