#problem #3
library(MASS)
View(Boston)
?Boston
#the dataset has 506 rows and 14 columns, representing, per capita crime rate by town, proportion of residential land zoned for lots over 25,000 sq ft, proportion of non-retail business acres per town, Charles River dummy variable (= 1 if tract bounds river; 0 otherwise)., nitrogen oxides concentration (parts per 10 million).
#rm average number of rooms per dwelling. proportion of owner-occupied units built prior to 1940.weighted mean of distances to five Boston employment centres.index of accessibility to radial highways.full-value property-tax rate per $10,000.pupil-teacher ratio by town.1000(Bk−0.63) 
#1000(Bk−0.63)^2 where Bk is the proportion of blacks by town.
#lower status of the population (percent)., median value of owner-occupied homes in $1000s.
sapply(Boston,class)
pairs(Boston[,1:14])
#some observations the median value of owner occupied homes increases and the crime rate appear to have an inverse relationship, there does not seem to be a relationship between the charles river and any of the variables in the dataset, lower income population and median value appear to have an inverse relationship 
library(stargazer)
Boston_model_1<- lm(crim~zn+indus+nox+rm+chas+age+dis+rad+tax+ptratio+black+lstat+medv, data=Boston)
summary(Boston_model_1)
#Zn has a positive relationship,nox has a negative relationship,  dis has a negative, rad has a positive, black negative, lstat positive, medv negative. indus, rm, chas, age, tax, and ptratio are not statistically signiicant 
summary(Boston[,c("crim","tax","ptratio")])
range(Boston[,"crim"])
#yes, some of the suburbs have a disporportionately high crime rate, even at the 3rd quartile the crime rate is only 3.677, but the range for the crime rate data is 0.00632 to 88.97620, which means there are some suburbs that are skewing the data 
range(Boston[,"tax"])
#for tax, there is more of a bimodal (not exaclty) distribution across boston, with some neighborhoods paying at the highest rate 711, and the lowest 187
range(Boston[,"ptratio"])
#the pupil teacher ratio ranges from 12.6 to 22, and while there are some neighborhoods with a higher ptratio, with a huge spike at the 20-20.5 bin, generally they are distributed more equally than crime
hist(Boston$tax, breaks=30, col="darkred")
hist(Boston$crim, breaks= 30, col="hotpink")
hist(Boston$ptratio, breaks=30, col="orange")
dim(Boston[Boston$chas==1,])
#35 suburbs are bound to the charles river
median(Boston$ptratio)
#19.05
which.min(Boston$medv)
#399
print(Boston[399,])
sapply(Boston, range)
#for neighborhood 399, the crime rate is disporportioantely high, zn= is the bottom of the range, indus is closer to the top of the range, it is not bound to the charles river, nox is near the top of the range, rm is at the bottom half of the data, it is at the max age, dis is near the bottom of the range, rad at the maximum, tax near the top, ptratio near the top, black at max, lstat near the top, medv at min
which.max(Boston$medv)
summary(Boston$rm>7)
#64 suburbs average more than 7 rooms
summary(Boston$rm>8)
#13 suburbs average dwellings over 8
summary(Boston[Boston$rm>8,])
#for the suburbs with an average of over 8 rooms, the median crime rate is 0.52014, zn range 0-95, indus range is 2.68-19.58, most aren't biund to the charles river,
#nox range is 0.4161 to 0.718, age = 8.40 to93.90, dis 1.8 to 8.907, rad 2 to 24, tax 224-666, pt ratio 13-20.2, black 354.6-396.9, lstat 2.47 to7.44, medv= 21.9 to 50