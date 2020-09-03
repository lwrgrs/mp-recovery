### layered plot attempt -- sample month on x axis, recovery on one y axis and root sample
### weight on other y axis

# treatments

trt1$date <- as.factor(trt1$date)

trt_means <- as.data.frame(aggregate(trt1[,5:6], list(trt1$date), FUN = mean))
trt_means$recovery <- c(trt_means$recovery * 100)

trt_means$Group.1 <- as.factor(trt_means$Group.1)

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


### control data plots

path <- "/Users/lwrogers/Desktop/Macrophomina Project(s)/Recovery Data/Plated_Root_Weights_2018-20.xlsx"

library(ggplot2)
library(patchwork)
library(hrbrthemes)
library(readxl)
library(ggplot2)
library(devEMF)
library(dplyr)

control <- read_xlsx(path, 6)

control$date <- as.factor(control$date)

control_means <- as.data.frame(aggregate(control[,5:6], list(control$date), FUN = mean))
control_means$recovery <- c(control_means$recovery * 100)

control_means$Group.1 <- as.factor(control_means$Group.1)

## distribution of root weights for each month

chgtoletter <- function(x){
  if(x > 0.46){
    print("Greater")
  } else {
    print("Less")
  }
}

control$wt_factor <- sapply(control$weight, chgtoletter)
control$wt_factor <- as.factor(control$wt_factor)

ggplot(control, 
       aes(wt_factor, 
           fill = wt_factor)) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylab("Frequency") +
  geom_bar(position = "dodge", 
           stat = "count") + 
  facet_grid(~date) +
  scale_fill_brewer(type = "qual", 
                    palette = "Dark2", 
                    name = "Root Weight Group",
                    labels = c("> 0.46 g", "< 0.46 g"))

## write to EMF

setwd("~/Desktop/Macrophomina Project(s)/PH Breif/")

emf(file = "rootwt_bymonth.emf", width = 10, height = 6)

ggplot(control, 
       aes(wt_factor, 
           fill = wt_factor)) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylab("Frequency") +
  geom_bar(position = "dodge", 
           stat = "count") + 
  facet_grid(~date) +
  scale_fill_brewer(type = "qual", 
                    palette = "Dark2", 
                    name = "Root Weight Group",
                    labels = c("> 0.46 g", "< 0.46 g"))

dev.off()

## to see what the plot parameters produce before writing to file -- below is for a double axis layered plot

ggplot(control_means, aes(x = Group.1, group = 1)) +
  geom_line(aes(y=recovery), size=2, color="#69b3a2") +
  geom_line(aes(y=weight*100), size=2, color="blue") +
  scale_y_continuous(name = "Mean Recovery of M. phaseolina for each date (%)",
                     sec.axis = sec_axis(trans=~./100, name="Mean Root Sample Weight for each date (g)")) +
  theme_ipsum() +
  xlab("Sampling Date") +
  ggtitle("Mean Recovery and Sample Weight for each Sampling Date") +
  theme(axis.title.y = element_text(size = 13, hjust = 0.5, vjust = 2, color = "#69b3a2"),
        axis.title.y.right = element_text(size = 13, hjust = 0.5, vjust = 2, angle = 270, color = "blue"),
        axis.title.x = element_text(size = 13, hjust = 0.5, vjust = 2),
        plot.title = element_text(color = "black", hjust = 0.5))

## write to EMF file

setwd("/Users/lwrogers/Desktop/Macrophomina Project(s)/PH Breif/")

emf(file = "control_rec_wt.emf", width = 10, height = 6)

ggplot(control_means, aes(x = Group.1, group = 1)) +
  geom_line(aes(y=recovery), size=2, color="#69b3a2") +
  geom_line(aes(y=weight*100), size=2, color="blue") +
  scale_y_continuous(name = "Mean Recovery of M. phaseolina for each date (%)",
                     sec.axis = sec_axis(trans=~./100, name="Mean Root Sample Weight for each date (g)")) +
  theme_ipsum() +
  xlab("Sampling Date") +
  ggtitle("Mean Recovery and Sample Weight for each Sampling Date") +
  theme(axis.title.y = element_text(size = 13, hjust = 0.5, vjust = 2, color = "#69b3a2"),
        axis.title.y.right = element_text(size = 13, hjust = 0.5, vjust = 2, angle = 270, color = "blue"),
        axis.title.x = element_text(size = 13, hjust = 0.5, vjust = 2),
        plot.title = element_text(color = "black", hjust = 0.5))

dev.off()

## write to TIFF

setwd("/Users/lwrogers/Desktop/Macrophomina Project(s)/PH Breif/")

tiff("control_rec_wt.tiff", units = "in", width = 10, height = 6, res = 300)

ggplot(control_means, aes(x = Group.1, group = 1)) +
  geom_line(aes(y=recovery), size=2, color="#69b3a2") +
  geom_line(aes(y=weight*100), size=2, color="blue") +
  scale_y_continuous(name = "Mean Recovery of M. phaseolina for each date (%)",
                     sec.axis = sec_axis(trans=~./100, name="Mean Root Sample Weight for each date (g)")) +
  theme_ipsum() +
  xlab("Sampling Date") +
  ggtitle("Mean Recovery and Sample Weight for each Sampling Date") +
  theme(axis.title.y = element_text(size = 13, hjust = 0.5, vjust = 2, color = "#69b3a2"),
        axis.title.y.right = element_text(size = 13, hjust = 0.5, vjust = 2, angle = 270, color = "blue"),
        axis.title.x = element_text(size = 13, hjust = 0.5, vjust = 2),
        plot.title = element_text(color = "black", hjust = 0.5))

dev.off()
