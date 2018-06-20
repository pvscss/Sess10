# Read the file in Zip format and get it into R.
dfzip<-file.choose()
dffile<-unzip(dfzip,exdir = ".",unzip="internal")
library(xlsx)
df<-read.xlsx("AirQualityUCI.xlsx",header = TRUE,sheetName = "AirQualityUCI")
View(df)

#Create Univariate for all the columns.
install.packages("Publish")
library(Publish)

UN_DF<-univariateTable(Date~Time+CO.GT.+PT08.S1.CO.+NMHC.GT.+C6H6.GT.+PT08.S2.NMHC.+NOx.GT.+
                         PT08.S3.NOx.+NO2.GT.+PT08.S5.O3.+T+RH+AH,data=df)

View(UN_DF)

#Check for missing values in all columns.
sum(is.na(df))   #  [1] 1710
sapply(df, function(x) sum(is.na(x)))

#  Impute the missing values using appropriate methods.
library(mice)
init = mice(df, maxit=0) 

set.seed(103)
imputed <- mice(df, method='cart', predictorMatrix=predM, m=1)
imputed<-complete(imputed)
sapply(imputed, function(x) sum(is.na(x)))

#  Create bi-variate analysis for all relationships.
plot( CO.GT.~ NOx.GT. | NO2.GT., data=df, 
            xlab="Weight of Car", ylab="Miles Per Gallon", 
            main="Enhanced Scatter Plot", 
            labels=row.names(mtcars))

#  Create cross tabulations with derived variables
ct<-with(df, tapply(CO.GT., list(Treatment=NMHC.GT.,Stage=C6H6.GT.), mean) )
View(ct)
