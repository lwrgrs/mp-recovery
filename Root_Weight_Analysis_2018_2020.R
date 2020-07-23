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

### histograms

library(ggplot2)

p <- ggplot(root1, aes(x = recovery)) + geom_density()

p + facet_grid(fungtrt ~ fumtrt, labeller = labeller(fungtrt = labels, fumtrt = labels)) +
  labs(title = "Density Distribution of M. phaseolina Recovery for 2018 Field Trial", x = "Recovery Proportion", y = "Density")

root1$fumtrt <- as.character(root1$fumtrt)
root1$fungtrt <- as.character(root1$fungtrt)

labels <- c("3" = "Chloropicrin", "6" = "No Fumigation Treatment", "4" = "2X Priaxor", "5" = "No Fungicide Treatment")

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

### some box plots

boxplot(root1_split[[4]]$weight, horizontal = T)


##########################################################################################################


#### 7.14.2020 two sample t test

path <- "/Users/lwrogers/Desktop/Macrophomina Project(s)/Recovery Data/Plated_Root_Weights_2018-20.xlsx"

library(readxl)

df <- read_xlsx(path, 6) ## data containing overlapping fungicide treatment from 2018/2019/2020 sampling dates

df$fungtrt <- as.factor(df$fungtrt)

chgtoletter <- function(x){
  if(x == 0.5){
    print("Equal")
  } else {
    print("Less")
  }
}

df$weight_value <- sapply(df$weight, chgtoletter)
# df$weight_value <- unlist(df$weight_value, use.names = T)
# df$weight_value <- as.factor(df$weight_value)

df <- as.data.frame(df)

## looking at control only

split_month <- split.data.frame(df, df$date)

t.test(recovery ~ weight_value, data = df) # control treatment across all months

# looking at t test analysis for each month using lapply?

lapply(split_month, t.test(recovery ~ weight_value))

# looking at t test analysis for each month manually

# 10.10.18

t.test(recovery ~ weight_value, split_month[[1]])

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


### layered plot attempt -- sample month on x axis, recovery on one y axis and root sample
### weight on other y axis

# treatments

trt1$date <- as.factor(trt1$date)

trt_means <- as.data.frame(aggregate(trt1[,5:6], list(trt1$date), FUN = mean))
trt_means$recovery <- c(trt_means$recovery * 100)

trt_means$Group.1 <- as.factor(trt_means$Group.1)

# controls

control$date <- as.factor(control$date)

control_means <- as.data.frame(aggregate(control[,5:6], list(control$date), FUN = mean))
control_means$recovery <- c(control_means$recovery * 100)

control_means$Group.1 <- as.factor(control_means$Group.1)

library(ggplot2)
library(dplyr)
library(patchwork)
library(hrbrthemes)

### individual plots

t1 <- ggplot(trt_means, aes(x = Group.1, y = recovery, group = 1)) +
  geom_line(color="#69b3a2", size=2) +
  ggtitle("Mean M. phaseolina Recovery for Priaxor Treatment") +
  theme_ipsum()

t2 <- ggplot(trt_means, aes(x = Group.1, y = weight, group = 1)) +
  geom_line(color="grey",size=2) +
  ggtitle("Mean Root Sample Weight for each sampling date") +
  theme_ipsum()

t1 + t2

### to layer the plots: treatment data

ggplot(trt_means, aes(x = Group.1, group = 1)) +
  geom_line(aes(y=recovery), size=2, color="#69b3a2") +
  geom_line(aes(y=weight*100), size=2, color="blue") +
  scale_y_continuous(name = "Mean Percent Recovery of M. phaseolina",
                     sec.axis = sec_axis(trans=~./100, name="Mean Root Sample Weight")) +
  theme_ipsum() +
  xlab("Sampling Date") +
  ggtitle("Mean Recovery and Sample Weight for each Sampling Date: 2X Priaxor at 2 Applications") +
  theme(axis.title.y = element_text(size = 13, hjust = 0.5, vjust = 2, color = "#69b3a2"),
        axis.title.y.right = element_text(size = 13, hjust = 0.5, vjust = 2, angle = 270, color = "blue"),
        axis.title.x = element_text(size = 13, hjust = 0.5, vjust = 2),
        plot.title = element_text(color = "black", hjust = 0.5))

### write to file

library(devEMF)

setwd("/Users/lwrogers/Desktop/Macrophomina Project(s)/PH Breif/")

emf(file = "Treatment_rec_wt.EMF", width = 10, height = 5)

ggplot(trt_means, aes(x = Group.1, group = 1)) +
  geom_line(aes(y=recovery), size=2, color="#69b3a2") +
  geom_line(aes(y=weight*100), size=2, color="blue") +
  scale_y_continuous(name = "Mean Recovery of M. phaseolina (%)",
                     sec.axis = sec_axis(trans=~./100, name="Mean Root Sample Weight (g)")) +
  theme_ipsum() +
  xlab("Sampling Date") +
  ggtitle("Mean Recovery and Sample Weight for each Sampling Date: 2X Priaxor at 2 Applications") +
  theme(axis.title.y = element_text(size = 13, hjust = 0.5, vjust = 2, color = "#69b3a2"),
        axis.title.y.right = element_text(size = 13, hjust = 0.5, vjust = 2, angle = 270, color = "blue"),
        axis.title.x = element_text(size = 13, hjust = 0.5, vjust = 2),
        plot.title = element_text(color = "black", hjust = 0.5))

dev.off()


### control data

ggplot(control_means, aes(x = Group.1, group = 1)) +
  geom_line(aes(y=recovery), size=2, color="#69b3a2") +
  geom_line(aes(y=weight*100), size=2, color="blue") +
  scale_y_continuous(name = "Mean Percent Recovery of M. phaseolina",
                     sec.axis = sec_axis(trans=~./100, name="Mean Root Sample Weight")) +
  theme_ipsum() +
  xlab("Sampling Date") +
  ggtitle("Mean Recovery and Sample Weight for each Sampling Date: Control") +
  theme(axis.title.y = element_text(size = 13, hjust = 0.5, vjust = 2, color = "#69b3a2"),
        axis.title.y.right = element_text(size = 13, hjust = 0.5, vjust = 2, angle = 270, color = "blue"),
        axis.title.x = element_text(size = 13, hjust = 0.5, vjust = 2),
        plot.title = element_text(color = "black", hjust = 0.5))

emf(file = "control_rec_wt.emf", width = 10, height = 5)

ggplot(control_means, aes(x = Group.1, group = 1)) +
  geom_line(aes(y=recovery), size=2, color="#69b3a2") +
  geom_line(aes(y=weight*100), size=2, color="blue") +
  scale_y_continuous(name = "Mean Recovery of M. phaseolina (%)",
                     sec.axis = sec_axis(trans=~./100, name="Mean Root Sample Weight (g)")) +
  theme_ipsum() +
  xlab("Sampling Date") +
  ggtitle("Mean Recovery and Sample Weight for each Sampling Date: Control") +
  theme(axis.title.y = element_text(size = 13, hjust = 0.5, vjust = 2, color = "#69b3a2"),
        axis.title.y.right = element_text(size = 13, hjust = 0.5, vjust = 2, angle = 270, color = "blue"),
        axis.title.x = element_text(size = 13, hjust = 0.5, vjust = 2),
        plot.title = element_text(color = "black", hjust = 0.5))

dev.off()
