###### looking at control only ####################################################################################################

path <- "/Users/lwrogers/Desktop/Macrophomina Project(s)/Recovery Data/Plated_Root_Weights_2018-20.xlsx"

library(readxl)
library(agricolae)

df <- read_xlsx(path, 6) # control data from 2018-2020

chgtoletter <- function(x){
  if(x == 0.5){
    print("Equal")
  } else {
    print("Less")
  }
} # function if splitting groups based on weights = 0.5 and < 0.5

chgtoletter <- function(x){
  if(x > 0.46){
    print("Greater")
  } else {
    print("Less")
  }
} # function if splitting groups based on weights > 0.4 and < 0.4

chgtoletter2 <- function(x){
  if(x == 0.00){
    print("none")
  } else if(x == 0.33) {
      print("one third")
  } else if(x == 0.67) {
      print("two thirds")
  } else if(x == 1.00) {
      print("all")
  }
} # function for changing recovery values to factors

## format data.frame

df2$recovery <- sapply(df2$recovery, round, 2)
df2$recvalue <- sapply(df2$recovery, chgtoletter2)

df$weight_value <- sapply(df$weight, chgtoletter)
df$weight_value <- as.factor(df$weight_value)

df <- as.data.frame(df)

df$recovery <- df$recovery*100 # make into percents

## split df based on month #################################

split_month <- split.data.frame(df, df$date)
split_month_less <- split.data.frame(df_less, df_less$date)
split_month_greater <- split.data.frame(df_greater, df_greater$date)

## mean and std error for greater than 0.46 weight group

df_greater <- subset(df, weight_value == "Greater")
df_greater_sd <- sd(df_greater$recovery)

df_sd <- sd(df$recovery)
df_mean <- mean(df$recovery)
samp <- sqrt(24)

for (i in 1:length(split_month_greater)){
    print(c(round(mean(split_month_greater[[i]]$recovery), 4), "plusminus", 
          round(df_greater_sd/sqrt(nrow(split_month_greater[[i]])), 3)))
}

## mean and std error for less than 0.46 weight group

df_less <- subset(df, weight_value == "Less")
df_less_sd <- sd(df_less$recovery)

for (i in 1:length(split_month_less)){
  print(c(round(mean(split_month_less[[i]]$recovery), 4), "plusminus", 
          round(df_less_sd/sqrt(nrow(split_month_less[[i]])), 3)))
}

for (i in 1:length(split_month)){
  print(t.test(split_month[[i]]$recovery ~ split_month[[i]]$weight_value, split_month[[i]]))
}

### mean recovery +- std error for each month

for (i in 1:length(split_month)){
  print(c(round(mean(split_month[[i]]$recovery), 3), "+-",
        round(sd(split_month[[i]]$recovery)/sqrt(nrow(split_month[[i]])), 3)))
}

### mean weight +- std error for each month

for (i in 1:length(split_month)){
  print(c(round(mean(split_month[[i]]$weight), 3), "+-",
          round(sd(split_month[[i]]$weight)/sqrt(nrow(split_month[[i]])), 3)))
}

### t.test analysis for each month for each month (two groups corresponding to weight)

for (i in 1:length(split_month)){
  print(t.test(recovery ~ weight_value, split_month[[i]]))
}

### ANOVA for recovery ~ date

c_aov1 <- aov(recovery ~ date, data = df)
summary(c_aov1)

LSD.test(c_aov1, "date", console = T)

### ANOVA for weight ~ date

c_aov2 <- aov(weight ~ date, data = df)
summary(c_aov2)

LSD.test(c_aov2, "date", console = T)

####################
###################
##################
#################
################
###############
##############
#############
############
###########
##########
#########
########
#######
######
#####
####
###
##
#

path <- "/Users/Layne/Desktop/Macrophomina Project(s)/Recovery Data/Plated_Root_Weights_2018-20.xlsx"

library(readxl)

root1 <- read_xlsx(path, 1)
root2 <- read_xlsx(path, 2)

root1$rep <- as.factor(root1$rep)
root1$fumtrt <- as.factor(root1$fumtrt)
root1$fungtrt <- as.factor(root1$fungtrt)
root1$plant <- as.factor(root1$plant)

root2$rep <- as.factor(root2$rep)
root2$fungtrt <- as.factor(root2$fungtrt)
root2$plant <- as.factor(root2$plant)

### split data based on treatment (2018-19 data)

root1_split <- split(root1, list(root1$fungtrt, root1$fumtrt))

### same thing for 2019-20 data

root2_split <- split(root2, levels(root2$fungtrt))

### checking for relationships; 2018-2019

# 3 * 4
aov1 <- aov(recovery ~ weight, root1_split[[1]])
summary(aov1)

## mean weight & recovery for 3 * 4
mean(root1_split[[1]]$weight)
mean(root1_split[[1]]$recovery)

# 3 * 5
aov2 <- aov(recovery ~ weight, root1_split[[2]])
summary(aov2)

## mean weight & recovery for 3 * 5
mean(root1_split[[2]]$weight)
mean(root1_split[[2]]$recovery)

# 6 * 4
aov3 <- aov(recovery ~ weight, root1_split[[3]])
summary(aov3)

## mean weight & recovery for 6 * 4
mean(root1_split[[3]]$weight)
mean(root1_split[[3]]$recovery)

# 6 * 5
aov4 <- aov(recovery ~ weight, root1_split[[4]])
summary(aov4)

## mean weight & recovery for 6 * 5
mean(root1_split[[4]]$weight)
mean(root1_split[[4]]$recovery)

### checking for relationships between recovery and root weight (2019-2020)

cor(root2_split_fung[[1]]$recovery, root2_split_fung[[1]]$weight)

cor(root2_split_fung[[2]]$recovery, root2_split_fung[[2]]$weight)

# # # # # # # #

# trt 1
aov5 <- aov(recovery ~ weight, root2_split[[1]])
summary(aov5)

lm5 <- lm(recovery ~ weight, root2_split[[1]])

mean(root2_split[[1]]$weight)
mean(root2_split[[1]]$recovery)

# trt 2
aov6 <- aov(recovery ~ weight, root2_split[[2]])
summary(aov6)

mean(root2_split[[2]]$weight)
mean(root2_split[[2]]$recovery)

# trt 5
aov7 <- aov(recovery ~ weight, root2_split[[3]])
summary(aov7)

mean(root2_split[[3]]$weight)
mean(root2_split[[3]]$recovery)
