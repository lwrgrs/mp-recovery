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

#######
##########
#############
################
#############
##########
#######

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

##########################################################################################################

path <- "/Users/lwrogers/Desktop/Macrophomina Project(s)/Recovery Data/Plated_Root_Weights_2018-20.xlsx"

library(readxl)
library(agricolae)

df <- read_xlsx(path, 6) # data containing overlapping fungicide treatment from 2018/2019/2020 sampling dates

chgtoletter <- function(x){
  if(x == 0.5){
    print("Equal")
  } else {
    print("Less")
  }
} # function if splitting groups based on weights = 0.5 and < 0.5

chgtoletter <- function(x){
  if(x > 0.4){
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

df$recovery <- sapply(df$recovery, round, 2)
df$recvalue <- sapply(df$recovery, chgtoletter2)

df$weight_value <- sapply(df$weight, chgtoletter)

df <- as.data.frame(df)

## looking at control only

split_month <- split.data.frame(df, df$date)

t.test(recovery ~ weight_value, data = df) # control treatment across all months

# looking at t test analysis for each month using lapply?
### lapply(split_month, t.test(recovery ~ weight_value))

###
#### looking at t test analysis for each month manually
###

# 10.10.18

t.test(recovery ~ weight_value, split_month[[1]])

m1 <- aov(weight ~ recvalue, split_month[[1]]) # looking at effect of recovery on weight
summary(m1)
LSD.test(m1, recvalue)

# 10.29.18

t.test(recovery ~ weight_value, split_month[[2]])

# 12.18.18

t.test(recovery ~ weight_value, split_month[[3]])

# 2.1.19

t.test(recovery ~ weight_value, split_month[[4]])

# 3.7.19

t.test(recovery ~ weight_value, split_month[[5]])

# 4.23.19

t.test(recovery ~ weight_value, split_month[[6]])

# 10.18.19

t.test(recovery ~ weight_value, split_month[[7]])

# 11.22.19

t.test(recovery ~ weight_value, split_month[[8]])

# 12.13.19

t.test(recovery ~ weight_value, split_month[[9]])

# 1.24.19

t.test(recovery ~ weight_value, split_month[[10]])
