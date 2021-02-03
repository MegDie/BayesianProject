
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

