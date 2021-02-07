
#import the data set
data("airquality")
data <- airquality

#We want to build a contingency table 
#with dichotomous variables
#The samples we care about are
#Temperature : temp (v)
#Ozone : ozone (u)
#Month : month (w)

#first step : drop all NA values

data <- data[!is.na(data$Ozone),]
data <- data[!is.na(data$Solar.R),]

#111 row now
rownames(data) <- 1:111
data <- cbind(data$Ozone, data$Temp, data$Month)

#second step : process u et v into dichotomous samples

#u process
#first class [1, 31] #0
#second class (31, 168] #1

u <- rep(0, length(data$Ozone))
for (i in 1:length(data$Ozone)){
  if (data$Ozone[i]>31) 
  {u[i] <- 1
    }
  else u[i] <- u[i]
}

#v process
#first class [57, 79] #0
#second class (79, 97] #1

v <- rep(0, length(data$Temp))
for (i in 1:length(data$Temp)){
  if (data$Temp[i]>79) 
  {v[i] <- 1
  }
  else v[i] <- v[i]
}

#last sample we care about for our contingency table
#month

df <- cbind(u,v, data$Month)

#contingency table 
cont <- table(df[,1], df[,2], df[,3])

#else we just copy the table in instructions

copy <- cbind(c(17,0,6,1),c(4,2,1,2),c(2,3,0,21),c(5,3,3,12),c(18,2,1,8))

#y vector 

y <- c(17, 4, 2, 5, 18, 0, 2, 3, 3, 2, 6, 1, 0, 3, 1, 1, 2, 21, 12, 8)

#design matrix X 

X <- matrix(data = 0, nrow = 111, ncol = 16)
colnames(X) <- c('X1', 'X2', 'X3', 'X4', 'X5', 'X6', 'X7', 'X8', 'X9', 
                 'X10', 'X11', 'X12', 'X13', 'X14', 'X15', 'X16')

#X1 intercept 
for (i in 1:111){
  if ((df[i,1]==0)&(df[i,2]==0)&(df[i,3]==5)){
    X[i,1] <- 1
  }
}

#X2
for (i in 1:111){
  if ((df[i,1]==1)){
    X[i,2] <- 1
  }
}

#X3
for (i in 1:111){
  if ((df[i,2]==1)){
    X[i,3] <- 1
  }
}

#X4  
for (i in 1:111){
  if (df[i,3]==6){
    X[i,4] <- 1
  }
}

#X5  
for (i in 1:111){
  if (df[i,3]==7){
    X[i,5] <- 1
  }
}

#X6 
for (i in 1:111){
  if (df[i,3]==8){
    X[i,6] <- 1
  }
}

#X7  
for (i in 1:111){
  if (df[i,3]==9){
    X[i,7] <- 1
  }
}

#X8  
for (i in 1:111){
  if ((df[i,1]==1)&(df[i,2]==1)){
    X[i,8] <- 1
  }
}

#X9  
for (i in 1:111){
  if ((df[i,1]==1)&(df[i,3]==6)){
    X[i,9] <- 1
  }
}

#X10  
for (i in 1:111){
  if ((df[i,1]==1)&(df[i,3]==7)){
    X[i,10] <- 1
  }
}

#X11  
for (i in 1:111){
  if ((df[i,1]==1)&(df[i,3]==8)){
    X[i,11] <- 1
  }
}

#X12  
for (i in 1:111){
  if ((df[i,1]==1)&(df[i,3]==9)){
    X[i,12] <- 1
  }
}

#X13  
for (i in 1:111){
  if ((df[i,2]==1)&(df[i,3]==6)){
    X[i,13] <- 1
  }
}

#X14  
for (i in 1:111){
  if ((df[i,2]==1)&(df[i,3]==7)){
    X[i,14] <- 1
  }
}

#X15  
for (i in 1:111){
  if ((df[i,2]==1)&(df[i,3]==8)){
    X[i,15] <- 1
  }
}

#X16  
for (i in 1:111){
  if ((df[i,2]==1)&(df[i,3]==9)){
    X[i,16] <- 1
  }
}












