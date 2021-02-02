
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
#first class [1, 31]
#second class (31, 168]

u <- rep(0, length(data$Ozone))
for (i in 1:length(data$Ozone)){
  if (data$Ozone[i]>31) 
  {u[i] <- 1
    }
  else u[i] <- u[i]
}

#v process
#first class [57, 79]
#second class (79, 97]

v <- rep(0, length(data$Temp))
for (i in 1:length(data$Temp)){
  if (data$Temp[i]>79) 
  {v[i] <- 1
  }
  else v[i] <- v[i]
}
