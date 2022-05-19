
## Basic matrix operations
a <- matrix(c(1,3,4,13), nrow = 4, ncol = 1)
a

aT <- t(a)
aT

b <- matrix(c(4,33,18,0), nrow = 4, ncol = 1)
b

a+b

a-b

a %*% b

t(a) %*% b
# 1 x 4 and 4 x 1 = 1 x 1, a scalar

a %*% t(b)
# 4 x 1 and 1 x 4 = 4 x 4 matrix

t(b) %*% a

## Matrix and a vector of ones
u <- matrix(c(1,1,1,1), nrow = 4, ncol = 1)
u

t(u) %*% a

sum(a)

a %*% t(u)

u %*% t(u)

t(u) %*% u
# a scalar

## Types of matrices
SymmetricMatrix <- matrix(c(1,2,-3,2,6,7,-3,7,-6), nrow=3, byrow = T)
SymmetricMatrix

DiagonalMatrix <- matrix(c(1,0,0,0,6,0,0,0,6), nrow=3, byrow = T)
DiagonalMatrix

IdentityMatrix <- matrix(c(1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1), nrow=4, byrow = T)
IdentityMatrix

a %*% IdentityMatrix

t(a) %*% IdentityMatrix

IdentityMatrix %*% a

IdentityMatrix %*% t(a)

A <- matrix(c(1,2,3,16,44,4), nrow=2, ncol=3, byrow=T)
A

## Inverse and Square Matrix
square <- matrix(c(1,4,5,7,8,2,3,5,7,3,5,6,1,4,9,3), nrow=4, ncol = 4)
square

inverse <- solve(square)
inverse
#solve() for the inverse of a matrix

square %*% inverse
round(square %*% inverse, 1)
#matrix multiplied by its inverse gives an Identity matrix

## Simple Linear Regression without Matrix algebra
myData <- data.frame(c(1,3,10,16,26,36), c(42,50,75,100,150,200))

colnames(myData) <- c("Age (x)", "Weight (y)")

myData

sumOfx <- sum(myData[,1])
sumOfx

sumOfy <- sum(myData[,2])
sumOfy

sumOfxSquared <- sum(myData[,1]^2)
sumOfxSquared

sumOfxy <- sum(myData$`Age (x)`*myData$`Weight (y)`)
sumOfxy

n <- nrow(myData)
n

a <- (n*sumOfxy - sumOfx*sumOfy)/(n*sumOfxSquared - sumOfx^2)
a

b <- mean(myData[,2]) - a * mean(myData[,1])
b

print(paste("y = ", round(a,3), "x + ", round(b,3), sep = " "))

myModel <- lm(myData$`Weight (y)` ~ myData$`Age (x)`)
print(paste("y = ", round(myModel$coefficients[2],3), "x + ", round(myModel$coefficients[1],3), sep = " "))


## Simple Linear Regression with Matrix algebra
myData <- data.frame(c(49,69,89,99,109), c(124,95,71,45,18))
myData

colnames(myData) <- c("Price (x)", "Demand (y)")
myData

x <- matrix(c(rep(1, length(myData$`Price (x)`)), myData$`Price (x)`), nrow = length(myData$`Price (x)`))
x

y <- matrix(myData$`Demand (y)`, ncol = 1)
y

xTx <- t(x) %*% x
xTx

xTxInv <- solve(xTx)
xTxInv

xTx %*% xTxInv

xTy <- t(x) %*% y
xTy

betaCap <- xTxInv %*% xTy
betaCap

intercept <- betaCap[1]
intercept

slope <- betaCap[2]
slope

#y-hat and residuals (~errors)
yPred <- matrix(slope*myData[,1]+intercept, ncol=1)
yPred

e <- myData[,2]-yPred
e

#Find SSE or sum of squared of errors
SSE <- t(e) %*% e
SSE

RMSE <- sqrt(SSE)
RMSE

#Finding vector y using the y-hat and e
y <- x %*% betaCap + e
y

#Expected value of y_i
y_i <- x %*% betaCap
y_i

sigmaSq <- (t(e) %*% e)/(length(e)-(ncol(x)-1)-1)
sigmaSq

#Var-covar of beta-hat vector
var_covar <- as.vector(sigmaSq)*xTxInv
var_covar


## Multiple Linear Regression with Matrix Algebra
rm(list=ls())

myData <- data.frame(c(49,69,89,99,109),c(14,12,9,8,4),c(124,95,71,45,18))
myData

colnames(myData) <- c("Price", "Cost_Index", "Demand")
myData

x <- matrix(c(rep(1,length(myData$Price)), myData$Price, myData$Cost_Index), nrow=length(myData$Price))
x

y <- matrix(myData$Demand, ncol = 1)
y

xTx <- t(x) %*% x
xTx

xTxInverse <- solve(xTx)
xTxInverse

xTx %*% xTxInverse

xTy <- t(x) %*% y 
xTy

betaCap <- xTxInverse %*% xTy
betaCap

intercept <- betaCap[1,]
intercept

x1Slope <- betaCap[2,]
x1Slope

x2Slope <- betaCap[3,]
x2Slope

mlr1 <- lm(myData$Demand ~ myData$Price + myData$Cost_Index)
mlr1
print(paste("R shows: y = ", round(mlr1$coefficients[2],3), "Price +", round(mlr1$coefficients[3],3), "Cost_Index + ", round(mlr1$coefficients[1],3), sep = " "))

print(paste("We show: y = ", round(x1Slope,3), "Price + ", round(x2Slope, 3), "Cost_Index + ", round(intercept, 3), sep = " "))

yPred <- matrix(x1Slope*x[,2]+x2Slope*x[,3]+intercept, ncol = 1)
yPred

e <- y - yPred
e
#e for residuals

SSE <- t(e) %*% e
SSE

RMSE <- sqrt(SSE)
RMSE

x %*% betaCap + e

y

x %*% betaCap
#expected value of y_i

y - x %*%betaCap 
#residuals

sigmaSq <- (t(e) %*% e)/(length(e)-(ncol(x)-1)-1)
sigmaSq

as.vector(sigmaSq) * xTxInverse
#var_cov matrix

#-------------Multivariate: Eigenvalues and Eigenvectors---------------
# proving (A - λI)a = 0 or 0 column vector in 2 X 2 matrix

matrixA1 <- matrix(c(3,1,1,2), nrow = 2, byrow = T)
matrixA1

IdentityMat <- matrix(c(1,0,0,1), nrow=2, byrow = T)
IdentityMat

eigen(matrixA1)
# gives eigenvalues and eigenvectors

eigenMat <- matrix(c(3.618034, 1.381966), byrow = T)
eigenMat
#2 values of eigenvalues

3.618034*IdentityMat  

matrixA1 - (3.618034*IdentityMat)

eigenVect <- matrix(c(-0.8506508, -0.5257311))
eigenVect

(matrixA1 - (3.618034*IdentityMat)) %*% eigenVect 
#gives 0 column vector

# 3 X 3 matrix
matrixA <- matrix(c(3,2,4,7,5,0,1,0,8),ncol=3,byrow=T) 
matrixA

matrixB <- matrix(c(6,1,0,2,8,7,3,4,5),ncol=3,byrow=T) 

det(matrixA)

eigen(matrixA)

IdentityMatrix <- matrix(c(1,0,0,0,1,0,0,0,1), nrow=3, byrow = T)
IdentityMatrix

eigenVal <- matrix(c(9.3079687, 6.8794331, -0.1874018))
eigenVal

9.3079687 * IdentityMatrix

matrixA - (9.3079687 * IdentityMatrix)

eigenVector <- matrix(c(-0.4865150, -0.7905361, -0.3719622))

(matrixA - (9.3079687 * IdentityMatrix)) %*%  eigenVector
# 0 column vector 

