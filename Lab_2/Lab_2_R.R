# Lab 2 

ameslist <- read.table("https://msudataanalytics.github.io/SSC442/Labs/data/ames.csv",
                       header = TRUE,
                       sep = ",")

#exercise one 

amesmapp <- mapply(is.integer,ameslist)
dropped_amesmapp <- subset(amesmapp, amesmapp == TRUE)
Ames <- subset(ameslist, select = amesmapp)

#txt file regarding variables 
# 12 Variables for scatterplot 
#LotArea, OverallQual, OverallCond, TotalBsmntSF, X1stFlrSF, X2ndFlrSf, 
#GarageCars, FullBath, HalfBath, TotRmsAbvGrd, PoolArea, WoodDeckSf

#sets for scatter plot 

set1 <- data.frame(Ames$LotArea,Ames$OverallQual,Ames$OverallCond, Ames$TotalBsmtSF, Ames$X1stFlrSF, Ames$X2ndFlrSF, Ames$GarageCars, Ames$FullBath, Ames$HalfBath, Ames$TotRmsAbvGrd, Ames$PoolArea, Ames$WoodDeckSF)

pairs(set1)

cor(set1)


plot(Ames$SalePrice, Ames$GrLivArea, main="Sale Price vs Living Area", 
xlab="SalePrice", ylab="General Living Area ", pch=15)

reg_model <- lm(GrLivArea ~ SalePrice, data = Ames)

abline(reg_model, col = 'steelblue')

test <- order(Ames$GrLivArea, decreasing = TRUE)[1:2]
 

Ames$GrLivArea[1299] # Index of outlier 

set1[1299,1:12] # 12 Factor variables for outlies

#Exercise 2

attach(ameslist) # makes the function the data that is being used

lm.fit = lm(SalePrice ~ GrLivArea)
summary(lm.fit)
plot(lm.fit)

lm.fit = lm(SalePrice ~ GrLivArea + LotArea)
plot(lm.fit)

garagevalue = lm(SalePrice ~ GarageTemp)

allvariables = lm(SalePrice ~ ., ameslist)

summary(allvariables)

plot(allvariables)

lm_ex_four = lm(SalePrice ~ OverallQual * OverallCond )
lm_ex_four1 = lm(SalePrice ~ OverallQual + OverallCond + OverallQual:OverallCond)

plot(lm_ex_four)
plot(lm_ex_four1)

summary(lm_ex_four)
summary(lm_ex_four1)

lm_ex_five= lm(SalePrice ~ . + log(BsmtFinSF1) + sqrt(BsmtUnfSF) + (TotalBsmtSF^2))

plot(lm_ex_five)

summary(lm_ex_five)




  