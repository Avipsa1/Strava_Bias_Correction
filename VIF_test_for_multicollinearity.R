setwd("C:/Users/aroy29/Dropbox (ASU)/Strava Analysis/Data/MAG2016/Analysis_Generated_Data/")
data = read.csv("MAG_Strava_Reduced_Covariates_OLS_Input.csv")
data = data[,1:ncol(data)]
head(data)
library(GGally)
X = data[,names(data) != "MAG_Daily"]
X = X[,7:ncol(X)]
head(X)
#ggpairs(X)
#Check for multicollinearity among the predictor variables
library(mctest)
MAG_Daily = data$MAG_Daily
omcdiag(X,MAG_Daily)
imcdiag(X,MAG_Daily,method="VIF",vif = 7.5)

'''
VIF detection
Month                          2.776500e+00         0
Strava_Daily                   1.914400e+00         0
Pop_per_sq_mile                7.534000e+00         1
Sidewalk_factor                5.905700e+00         0
Avg_segment_speed_limit        6.089000e+00         1
Bike_facility                  3.847900e+00         0
Connectivity_to_other_segments 4.284500e+00         0
PCT_MALE                       1.374950e+01         1
PCT_FEMALE                     1.389420e+01         1
PCT_VETERANS                   3.166100e+00         0
PCT_WHITE_NONHISP              4.093270e+01         1
AADT                           3.498100e+00         0
dist_to_green_space            3.027500e+00         0
dist_to_commercial_areas       3.589100e+00         0
dist_to_residential_areas      2.257500e+00         0
dist_to_public_transit         6.325300e+00         1
MEDIAN_AGE                     8.074800e+00         1
MEDIAN_HOUSEHOLD_INCOME        7.299400e+00         1
PCT_PUBLIC_TRANSPORTATION      1.129250e+01         1
PCT_BICYCLE                    5.255900e+00         0
PCT_NO_VEHICLE                 1.632981e+11         1
PCT_INC_BLW_POV                9.758100e+00         1
PCT_ONE_OR_MORE_VEH            1.632952e+11         1
PCT_NON_WHITE                  6.015660e+01         1
EDU_BLW_HS                     2.390470e+01         1
EDU_ABV_HS                     1.095950e+01         1
'''
#Removing variables PCT_VETERANS
X_1 = X[,-c(8:11,19,21,25)]
imcdiag(X_1,MAG_Daily,method="VIF",vif = 7.5)
'''
Call:
imcdiag(x = X_1, y = MAG_Daily, method = "VIF", vif = 7.5)


VIF Multicollinearity Diagnostics

VIF detection
Month                          2.270900e+00         0
Strava_Daily                   1.444100e+00         0
Pop_per_sq_mile                3.560100e+00         0
Sidewalk_factor                4.651800e+00         0
Avg_segment_speed_limit        5.005500e+00         0
Bike_facility                  2.491200e+00         0
Connectivity_to_other_segments 2.686800e+00         0
AADT                           2.489200e+00         0
dist_to_green_space            2.140300e+00         0
dist_to_commercial_areas       3.283900e+00         0
dist_to_residential_areas      2.010200e+00         0
dist_to_public_transit         4.552700e+00         0
MEDIAN_AGE                     6.377400e+00         0
PCT_PUBLIC_TRANSPORTATION      9.445000e+00         1
PCT_BICYCLE                    3.610700e+00         0
PCT_NO_VEHICLE                 1.400874e+11         1
PCT_INC_BLW_POV                7.908000e+00         1
PCT_ONE_OR_MORE_VEH            1.400918e+11         1
PCT_NON_WHITE                  9.142700e+00         1
EDU_BLW_HS                     1.588210e+01         1
EDU_ABV_HS                     8.216300e+00         1

Multicollinearity may be due to PCT_PUBLIC_TRANSPORTATION PCT_NO_VEHICLE PCT_INC_BLW_POV PCT_ONE_OR_MORE_VEH PCT_NON_WHITE EDU_BLW_HS EDU_ABV_HS regressors

1 --> COLLINEARITY is detected by the test 
0 --> COLLINEARITY is not detected by the test

===================================
'''
X_2 = X_1[,c(1:13,15:21)]
imcdiag(X_2,MAG_Daily,method="VIF",vif = 7.5)
'''
Call:
imcdiag(x = X_2, y = MAG_Daily, method = "VIF", vif = 7.5)


VIF Multicollinearity Diagnostics

VIF detection
Month                          2.135900e+00         0
Strava_Daily                   1.401400e+00         0
Pop_per_sq_mile                2.559500e+00         0
Sidewalk_factor                4.498800e+00         0
Avg_segment_speed_limit        4.792000e+00         0
Bike_facility                  2.038700e+00         0
Connectivity_to_other_segments 2.431600e+00         0
AADT                           2.350600e+00         0
dist_to_green_space            2.128000e+00         0
dist_to_commercial_areas       2.617400e+00         0
dist_to_residential_areas      1.887700e+00         0
dist_to_public_transit         4.435900e+00         0
MEDIAN_AGE                     4.549500e+00         0
PCT_BICYCLE                    3.510200e+00         0
PCT_NO_VEHICLE                 1.294714e+11         1
PCT_INC_BLW_POV                5.954200e+00         0
PCT_ONE_OR_MORE_VEH            1.294733e+11         1
PCT_NON_WHITE                  9.139900e+00         1
EDU_BLW_HS                     1.513860e+01         1
EDU_ABV_HS                     7.791800e+00         1

Multicollinearity may be due to PCT_NO_VEHICLE PCT_ONE_OR_MORE_VEH PCT_NON_WHITE EDU_BLW_HS EDU_ABV_HS regressors

1 --> COLLINEARITY is detected by the test 
0 --> COLLINEARITY is not detected by the test

===================================
'''
X_3 = X_2[,-c(15,19)]
imcdiag(X_3,MAG_Daily,method="VIF",vif = 7.5)
'''
Call:
  imcdiag(x = X_3, y = MAG_Daily, method = "VIF", vif = 7.5)


VIF Multicollinearity Diagnostics

VIF detection
Month                          1.9493         0
Strava_Daily                   1.3873         0
Pop_per_sq_mile                2.1731         0
Sidewalk_factor                4.0410         0
Avg_segment_speed_limit        4.6601         0
Bike_facility                  1.9423         0
Connectivity_to_other_segments 2.0776         0
AADT                           2.3422         0
dist_to_green_space            2.0949         0
dist_to_commercial_areas       2.3947         0
dist_to_residential_areas      1.6847         0
dist_to_public_transit         3.2747         0
MEDIAN_AGE                     3.6833         0
PCT_BICYCLE                    2.9152         0
PCT_INC_BLW_POV                3.8959         0
PCT_ONE_OR_MORE_VEH            3.2365         0
PCT_NON_WHITE                  4.5117         0
EDU_ABV_HS                     4.4118         0

NOTE:  VIF Method Failed to detect multicollinearity


0 --> COLLINEARITY is not detected by the test

===================================
'''
