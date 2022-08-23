rm(list=ls())
library("tidyverse")
library('reshape2')
library("ggplot2")

dlcrnet_ms5_5000<- read.csv('/Volumes/Paul/Resnets/Tubes/dlcrnet_ms5/dist_5000.csv', header = FALSE)
dlcrnet_ms5_8000<- read.csv('/Volumes/Paul/Resnets/Tubes/dlcrnet_ms5/dist_8000.csv', header = FALSE)
dlcrnet_ms5_9000<- read.csv('/Volumes/Paul/Resnets/Tubes/dlcrnet_ms5/dist_9000.csv', header = FALSE)
dlcrnet_ms5_7000<- read.csv('/Volumes/Paul/Resnets/Tubes/dlcrnet_ms5/dist_7000.csv', header = FALSE)
dlcrnet_ms5_10000<- read.csv('/Volumes/Paul/Resnets/Tubes/dlcrnet_ms5/dist_10000.csv', header = FALSE)
dlcrnet_ms5_6000<- read.csv('/Volumes/Paul/Resnets/Tubes/dlcrnet_ms5/dist_6000.csv', header = FALSE)
dlcrnet_ms5_11000<- read.csv('/Volumes/Paul/Resnets/Tubes/dlcrnet_ms5/dist_11000.csv', header = FALSE)
dlcrnet_ms5_12000<- read.csv('/Volumes/Paul/Resnets/Tubes/dlcrnet_ms5/dist_12000.csv', header = FALSE)
dlcrnet_ms5_13000<- read.csv('/Volumes/Paul/Resnets/Tubes/dlcrnet_ms5/dist_13000.csv', header = FALSE)
dlcrnet_ms5_14000<- read.csv('/Volumes/Paul/Resnets/Tubes/dlcrnet_ms5/dist_14000.csv', header = FALSE)
dlcrnet_ms5_15000<- read.csv('/Volumes/Paul/Resnets/Tubes/dlcrnet_ms5/dist_15000.csv', header = FALSE)
dlcrnet_ms5_16000<- read.csv('/Volumes/Paul/Resnets/Tubes/dlcrnet_ms5/dist_16000.csv', header = FALSE)
dlcrnet_ms5_17000<- read.csv('/Volumes/Paul/Resnets/Tubes/dlcrnet_ms5/dist_17000.csv', header = FALSE)
dlcrnet_ms5_18000<- read.csv('/Volumes/Paul/Resnets/Tubes/dlcrnet_ms5/dist_18000.csv', header = FALSE)
dlcrnet_ms5_19000<- read.csv('/Volumes/Paul/Resnets/Tubes/dlcrnet_ms5/dist_19000.csv', header = FALSE)
dlcrnet_ms5_20000<- read.csv('/Volumes/Paul/Resnets/Tubes/dlcrnet_ms5/dist_20000.csv', header = FALSE)

#read in data

data1 <- dlcrnet_ms5_5000
data2 <- dlcrnet_ms5_6000
data3 <- dlcrnet_ms5_7000
data4 <- dlcrnet_ms5_8000
data5 <- dlcrnet_ms5_9000
data6 <- dlcrnet_ms5_10000
data7 <- dlcrnet_ms5_11000
data8 <- dlcrnet_ms5_12000
data9 <- dlcrnet_ms5_13000
data10 <- dlcrnet_ms5_14000
data11 <- dlcrnet_ms5_15000
data12 <- dlcrnet_ms5_16000
data13 <- dlcrnet_ms5_17000
data14 <- dlcrnet_ms5_18000
data15 <- dlcrnet_ms5_19000
data16 <- dlcrnet_ms5_20000

#changing all blank cells to NAs
data1 <- data1 %>%
  mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))))

data2 <- data2 %>%
  mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))))

data3 <- data3 %>%
  mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))))

data4 <- data4 %>%
  mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))))

data5 <- data5 %>%
  mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))))

data6 <- data6 %>%
  mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))))

data7 <- data7 %>%
  mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))))

data8 <- data8 %>%
  mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))))

data9 <- data9 %>%
  mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))))

data10 <- data10 %>%
  mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))))

data11 <- data11 %>%
  mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))))

data12 <- data12 %>%
  mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))))

data13 <- data13 %>%
  mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))))

data14 <- data14 %>%
  mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))))

data15 <- data15 %>%
  mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))))

data16 <- data16 %>%
  mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))))

#removing rows with only NAs
data1 <- data1[!apply(is.na(data1[,2:121]), 1, all),]
data2 <- data2[!apply(is.na(data2[,2:121]), 1, all),]
data3 <- data3[!apply(is.na(data3[,2:121]), 1, all),]
data4 <- data4[!apply(is.na(data4[,2:121]), 1, all),]
data5 <- data5[!apply(is.na(data5[,2:121]), 1, all),]
data6 <- data6[!apply(is.na(data6[,2:121]), 1, all),]
data7 <- data7[!apply(is.na(data7[,2:121]), 1, all),]
data8 <- data8[!apply(is.na(data8[,2:121]), 1, all),]
data9 <- data9[!apply(is.na(data9[,2:121]), 1, all),]
data10 <- data10[!apply(is.na(data10[,2:121]), 1, all),]
data11 <- data11[!apply(is.na(data11[,2:121]), 1, all),]
data12 <- data12[!apply(is.na(data12[,2:121]), 1, all),]
data13 <- data13[!apply(is.na(data13[,2:121]), 1, all),]
data14 <- data14[!apply(is.na(data14[,2:121]), 1, all),]
data15 <- data15[!apply(is.na(data15[,2:121]), 1, all),]
data16 <- data16[!apply(is.na(data16[,2:121]), 1, all),]

#removing scorer name in first row and the first column
data1 <- data1[-1,]
data1 <- data1[,-1]
data2 <- data2[-1,]
data2 <- data2[,-1]
data3 <- data3[-1,]
data3 <- data3[,-1]
data4 <- data4[-1,]
data4 <- data4[,-1]
data5 <- data5[-1,]
data5 <- data5[,-1]
data6 <- data6[-1,]
data6 <- data6[,-1]
data7 <- data7[-1,]
data7 <- data7[,-1]
data8 <- data8[-1,]
data8 <- data8[,-1]
data9 <- data9[-1,]
data9 <- data9[,-1]
data10 <- data10[-1,]
data10 <- data10[,-1]
data11 <- data11[-1,]
data11 <- data11[,-1]
data12 <- data12[-1,]
data12 <- data12[,-1]
data13 <- data13[-1,]
data13 <- data13[,-1]
data14 <- data14[-1,]
data14 <- data14[,-1]
data15 <- data15[-1,]
data15 <- data15[,-1]
data16 <- data16[-1,]
data16 <- data16[,-1]

#combined individual, metric, and body part to make identifying column names
data1[1,] <- paste(data1[1,],data1[2,], data1[3,], sep="/") # first row has the name of the headers 
colnames(data1) <- data1[1,] #makes this header the new colname
data1 <- data1[-c(1:3),] # remove the first three rows
rownames(data1) <- seq(1,nrow(data1),1) # readjusting the rownames with the new data

data2[1,] <- paste(data2[1,],data2[2,], data2[3,], sep="/")
colnames(data2) <- data2[1,]
data2 <- data2[-c(1:3),]
rownames(data2) <- seq(1,nrow(data2),1)

data3[1,] <- paste(data3[1,],data3[2,], data3[3,], sep="/")
colnames(data3) <- data3[1,]
data3 <- data3[-c(1:3),]
rownames(data3) <- seq(1,nrow(data3),1)

data4[1,] <- paste(data4[1,],data4[2,], data4[3,], sep="/")
colnames(data4) <- data4[1,]
data4 <- data4[-c(1:3),]
rownames(data4) <- seq(1,nrow(data4),1)

data5[1,] <- paste(data5[1,],data5[2,], data5[3,], sep="/")
colnames(data5) <- data5[1,]
data5 <- data5[-c(1:3),]
rownames(data5) <- seq(1,nrow(data5),1)

data6[1,] <- paste(data6[1,],data6[2,], data6[3,], sep="/")
colnames(data6) <- data6[1,]
data6 <- data6[-c(1:3),]
rownames(data6) <- seq(1,nrow(data6),1)

data7[1,] <- paste(data7[1,],data7[2,], data7[3,], sep="/")
colnames(data7) <- data7[1,]
data7 <- data7[-c(1:3),]
rownames(data7) <- seq(1,nrow(data7),1)

data8[1,] <- paste(data8[1,],data8[2,], data8[3,], sep="/")
colnames(data8) <- data8[1,]
data8 <- data8[-c(1:3),]
rownames(data8) <- seq(1,nrow(data8),1)

data9[1,] <- paste(data9[1,],data9[2,], data9[3,], sep="/")
colnames(data9) <- data9[1,]
data9 <- data9[-c(1:3),]
rownames(data9) <- seq(1,nrow(data9),1)

data10[1,] <- paste(data10[1,],data10[2,], data10[3,], sep="/")
colnames(data10) <- data10[1,]
data10 <- data10[-c(1:3),]
rownames(data10) <- seq(1,nrow(data10),1)

data11[1,] <- paste(data11[1,],data11[2,], data11[3,], sep="/")
colnames(data11) <- data11[1,]
data11 <- data11[-c(1:3),]
rownames(data11) <- seq(1,nrow(data11),1)

data12[1,] <- paste(data12[1,],data12[2,], data12[3,], sep="/")
colnames(data12) <- data12[1,]
data12 <- data12[-c(1:3),]
rownames(data12) <- seq(1,nrow(data12),1)

data13[1,] <- paste(data13[1,],data13[2,], data13[3,], sep="/")
colnames(data13) <- data13[1,]
data13 <- data13[-c(1:3),]
rownames(data13) <- seq(1,nrow(data13),1)

data14[1,] <- paste(data14[1,],data14[2,], data14[3,], sep="/")
colnames(data14) <- data14[1,]
data14 <- data14[-c(1:3),]
rownames(data14) <- seq(1,nrow(data14),1)

data15[1,] <- paste(data15[1,],data15[2,], data15[3,], sep="/")
colnames(data15) <- data15[1,]
data15 <- data15[-c(1:3),]
rownames(data15) <- seq(1,nrow(data15),1)

data16[1,] <- paste(data16[1,],data16[2,], data16[3,], sep="/")
colnames(data16) <- data16[1,]
data16 <- data16[-c(1:3),]
rownames(data16) <- seq(1,nrow(data16),1)

#pivoting the data to a longer format 
data1 <- data1 %>%
  pivot_longer(cols=1:107,
               names_to=c("individuals"),
               values_to=c("RMSE"))

data2 <- data2 %>%
  pivot_longer(cols=1:107,
               names_to=c("individuals"),
               values_to=c("RMSE"))

data3 <- data3 %>%
  pivot_longer(cols=1:107,
               names_to=c("individuals"),
               values_to=c("RMSE"))

data4 <- data4 %>%
  pivot_longer(cols=1:107,
               names_to=c("individuals"),
               values_to=c("RMSE"))

data5 <- data5 %>%
  pivot_longer(cols=1:107,
               names_to=c("individuals"),
               values_to=c("RMSE"))

data6 <- data6 %>%
  pivot_longer(cols=1:107,
               names_to=c("individuals"),
               values_to=c("RMSE"))

data7 <- data7 %>%
  pivot_longer(cols=1:107,
               names_to=c("individuals"),
               values_to=c("RMSE"))

data8 <- data8 %>%
  pivot_longer(cols=1:107,
               names_to=c("individuals"),
               values_to=c("RMSE"))

data9 <- data9 %>%
  pivot_longer(cols=1:107,
               names_to=c("individuals"),
               values_to=c("RMSE"))

data10 <- data10 %>%
  pivot_longer(cols=1:107,
               names_to=c("individuals"),
               values_to=c("RMSE"))

data11 <- data11 %>%
  pivot_longer(cols=1:107,
               names_to=c("individuals"),
               values_to=c("RMSE"))

data12 <- data12 %>%
  pivot_longer(cols=1:107,
               names_to=c("individuals"),
               values_to=c("RMSE"))

data13 <- data13 %>%
  pivot_longer(cols=1:107,
               names_to=c("individuals"),
               values_to=c("RMSE"))

data14 <- data14 %>%
  pivot_longer(cols=1:107,
               names_to=c("individuals"),
               values_to=c("RMSE"))

data15 <- data15 %>%
  pivot_longer(cols=1:107,
               names_to=c("individuals"),
               values_to=c("RMSE"))

data16 <- data16 %>%
  pivot_longer(cols=1:107,
               names_to=c("individuals"),
               values_to=c("RMSE"))

#selected the data of interest
data1 <- data1[,c(14,15)]
data2 <- data2[,c(14,15)]
data3 <- data3[,c(14,15)]
data4 <- data4[,c(14,15)]
data5 <- data5[,c(14,15)]
data6 <- data6[,c(14,15)]
data7 <- data7[,c(14,15)]
data8 <- data8[,c(14,15)]
data9 <- data9[,c(14,15)]
data10 <- data10[,c(14,15)]
data11 <- data11[,c(14,15)]
data12 <- data12[,c(14,15)]
data13 <- data13[,c(14,15)]
data14 <- data14[,c(14,15)]
data15 <- data15[,c(14,15)]
data16 <- data16[,c(14,15)]

#split the columns that were previously merged
data1[c('Individual', 'bodypart', 'metric')] <- str_split_fixed(data1$individuals, '/', 3)
data1 <- data1[,-1]

data2[c('Individual', 'bodypart', 'metric')] <- str_split_fixed(data2$individuals, '/', 3)
data2 <- data2[,-1]

data3[c('Individual', 'bodypart', 'metric')] <- str_split_fixed(data3$individuals, '/', 3)
data3 <- data3[,-1]

data4[c('Individual', 'bodypart', 'metric')] <- str_split_fixed(data4$individuals, '/', 3)
data4 <- data4[,-1]

data5[c('Individual', 'bodypart', 'metric')] <- str_split_fixed(data5$individuals, '/', 3)
data5 <- data5[,-1]

data6[c('Individual', 'bodypart', 'metric')] <- str_split_fixed(data6$individuals, '/', 3)
data6 <- data6[,-1]

data7[c('Individual', 'bodypart', 'metric')] <- str_split_fixed(data7$individuals, '/', 3)
data7 <- data7[,-1]

data8[c('Individual', 'bodypart', 'metric')] <- str_split_fixed(data8$individuals, '/', 3)
data8 <- data8[,-1]

data9[c('Individual', 'bodypart', 'metric')] <- str_split_fixed(data9$individuals, '/', 3)
data9 <- data9[,-1]

data10[c('Individual', 'bodypart', 'metric')] <- str_split_fixed(data10$individuals, '/', 3)
data10 <- data10[,-1]

data11[c('Individual', 'bodypart', 'metric')] <- str_split_fixed(data11$individuals, '/', 3)
data11 <- data11[,-1]

data12[c('Individual', 'bodypart', 'metric')] <- str_split_fixed(data12$individuals, '/', 3)
data12 <- data12[,-1]

data13[c('Individual', 'bodypart', 'metric')] <- str_split_fixed(data13$individuals, '/', 3)
data13 <- data13[,-1]

data14[c('Individual', 'bodypart', 'metric')] <- str_split_fixed(data14$individuals, '/', 3)
data14 <- data14[,-1]

data15[c('Individual', 'bodypart', 'metric')] <- str_split_fixed(data15$individuals, '/', 3)
data15 <- data15[,-1]

data16[c('Individual', 'bodypart', 'metric')] <- str_split_fixed(data16$individuals, '/', 3)
data16 <- data16[,-1]

######## Do a scaterplot with the iterations 

data1<-subset(data1, metric !="conf")
data2<-subset(data2, metric !="conf")
data3<-subset(data3, metric !="conf")
data4<-subset(data4, metric !="conf")
data5<-subset(data5, metric !="conf")
data6<-subset(data6, metric !="conf")
data7<-subset(data7, metric !="conf")
data8<-subset(data8, metric !="conf")
data9<-subset(data9, metric !="conf")
data10<-subset(data10, metric !="conf")
data11<-subset(data11, metric !="conf")
data12<-subset(data12, metric !="conf")
data13<-subset(data13, metric !="conf")
data14<-subset(data14, metric !="conf")
data15<-subset(data15, metric !="conf")
data16<-subset(data16, metric !="conf")

# adding the iteration count for each model

data1[,5] <- 5000
data2[,5] <- 6000
data3[,5] <- 7000
data4[,5] <- 8000
data5[,5] <- 9000
data6[,5] <- 10000
data7[,5] <- 11000
data8[,5] <- 12000
data9[,5] <- 13000
data10[,5] <- 14000
data11[,5] <- 15000
data12[,5] <- 16000
data13[,5] <- 17000
data14[,5] <- 18000
data15[,5] <- 19000
data16[,5] <- 20000

# Changing the column names and adding the iters

colnames(data1) <- c('RMSE', 'Individual', 'bodypart', 'metric', 'Iters')
colnames(data2) <- c('RMSE', 'Individual', 'bodypart', 'metric', 'Iters')
colnames(data3) <- c('RMSE', 'Individual', 'bodypart', 'metric', 'Iters')
colnames(data4) <- c('RMSE', 'Individual', 'bodypart', 'metric', 'Iters')
colnames(data5) <- c('RMSE', 'Individual', 'bodypart', 'metric', 'Iters')
colnames(data6) <- c('RMSE', 'Individual', 'bodypart', 'metric', 'Iters')
colnames(data7) <- c('RMSE', 'Individual', 'bodypart', 'metric', 'Iters')
colnames(data8) <- c('RMSE', 'Individual', 'bodypart', 'metric', 'Iters')
colnames(data9) <- c('RMSE', 'Individual', 'bodypart', 'metric', 'Iters')
colnames(data10) <- c('RMSE', 'Individual', 'bodypart', 'metric', 'Iters')
colnames(data11) <- c('RMSE', 'Individual', 'bodypart', 'metric', 'Iters')
colnames(data12) <- c('RMSE', 'Individual', 'bodypart', 'metric', 'Iters')
colnames(data13) <- c('RMSE', 'Individual', 'bodypart', 'metric', 'Iters')
colnames(data14) <- c('RMSE', 'Individual', 'bodypart', 'metric', 'Iters')
colnames(data15) <- c('RMSE', 'Individual', 'bodypart', 'metric', 'Iters')
colnames(data16) <- c('RMSE', 'Individual', 'bodypart', 'metric', 'Iters')

###### test 11 a 19 k jiters
#test1 <- data15

#test1$RMSE <- jitter(as.numeric(data16$RMSE))
#test2$RMSE <- jitter(as.numeric(data16$RMSE))
#test2<- data16

#b <- jitter(a, 4)
#a <- c(2,3,6)
#c<- jitter(a,10)

#data15$RMSE <- jitter(as.numeric(data15$RMSE))
##data14$RMSE <- data14$RMSE* 1.2 
#data13$RMSE <- jitter(as.numeric(data13$RMSE),30)
#data12$RMSE <- jitter(as.numeric(data12$RMSE),5)
#data11$RMSE <- jitter(as.numeric(data11$RMSE),0.5)
#data10$RMSE <- jitter(as.numeric(data10$RMSE),0.5)
#data9$RMSE <- jitter(as.numeric(data9$RMSE),13)
#data8$RMSE <- jitter(as.numeric(data8$RMSE),12)
#ßdata7$RMSE <- data7$RMSE* 3.1


#data15$RMSE == data14$RMSE

# par de 20000 et rajoute en poids

######


dlcrnet_ms5 <- rbind(data1, data2, data3, data4, data5, data6, data7, data8, data9, data10, data11, data12, data13, data14, data15, data16) # Merging all the iterations together 
dlcrnet_ms5$RMSE <- as.numeric(dlcrnet_ms5$RMSE)

dlcrnet_ms5[,6]<- c('dlcrnet_ms5')

colnames(dlcrnet_ms5) <- c('RMSE', 'Individual', 'bodypart', 'metric', 'Iters','Model')

dlcrnet_ms5 <- na.omit(dlcrnet_ms5)

saveRDS(Trap_dlcrnet_ms5, file = 'dlcrnet_ms5.rds')



############### creating a data frame with other models added


ResNet101 <- readRDS(file = 'ResNet101.rds')
ResNet50 <- readRDS(file = 'ResNet50.rds')
dlcrnet_ms5 <- readRDS(file = 'dlcrnet_ms5.rds')
ResNet50 <- na.omit(ResNet50)
ResNet101 <- na.omit(ResNet101)
ResNets <- rbind(dlcrnet_ms5, ResNet50,ResNet101) # combining all the datasets together for each model 
ResNets <- na.omit(ResNets) # removing all NAs


#### using ggplot with Dlcrent_ms5
library(ggpubr)

ggp <- ggplot(dlcrnet_ms5, aes(Iters, RMSE)) + 
  ggtitle('Performance change of Dlcrnet_ms5 with iteration parameter') + 
  xlab("Iterations") + 
  ylab("Pixel error (RMSE)") +    
  #geom_point() +
  geom_jitter(alpha = 0.5)+
  ylim(0, 30) +
  scale_x_continuous(breaks = seq(from =5000, to =20000, by =1000) ) + 
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(size = 10))

ggp <- ggp + # Add polynomial regression curve
stat_smooth(method = "lm",formula = y ~ poly(x, 4), color = c('red'), se = F)+
  stat_regline_equation(label.y = 30, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 26, aes(label = ..rr.label..))

##### Resnet_50

ggp2 <- ggplot(ResNet50, aes(Iters, RMSE)) + 
  ggtitle('Performance change of ResNet50 with iteration parameter') + 
  xlab("Iterations") + 
  ylab("Pixel error (RMSE)") +    
  #geom_point() +
  geom_jitter(alpha = 0.5)+
  ylim(0, 30) +
  scale_x_continuous(breaks = seq(from =5000, to =20000, by =1000) ) + 
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(size = 10))

ggp2 <- ggp2 + # Add polynomial regression curve
  stat_smooth(method = "lm",formula = y ~ poly(x, 4), color = c('red'), se = F)+ 
    stat_regline_equation(label.y = 30, aes(label = ..eq.label..)) +
    stat_regline_equation(label.y = 26, aes(label = ..rr.label..))

####### Resnet101

ggp3 <- ggplot(ResNet101, aes(Iters, RMSE)) + 
  ggtitle('Performance change of ResNet101 with iteration parameter') + 
  xlab("Iterations") + 
  ylab("Pixel error (RMSE)") +    
  #geom_point() +
  geom_jitter(alpha = 0.5)+
  ylim(0, 30) +
  scale_x_continuous(breaks = seq(from =5000, to =20000, by =1000) ) + 
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(size = 10))


ggp3 <- ggp3 + # Add polynomial regression curve
  stat_smooth(method = "lm",formula = y ~ poly(x, 4), color = c('red'), se = F)+
  stat_regline_equation(label.y = 30, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 26, aes(label = ..rr.label..))

###### Finding the best Iteration for each model

library(cowplot)
library(grDevices)

rud_both <- ggdraw() +
  draw_plot(ggp, x = 0, y = 0, width = 1, height = 0.33333) +
  draw_plot(ggp2, x = 0, y = 0.33333, width = 1, height = 0.33333) +
  draw_plot(ggp3, x = 0, y = 0.66666, width = 1, height = 0.33333) 

rud_both

###### Finding the best Model using a GLM

model <- glm(ResNets$RMSE ~ ResNets$Model* ResNets$Iters,  family = 'poisson') # the magnitude of change in rmse within models differ between models

summary(model)

######
######

a <- subset.data.frame(ResNet50, ResNet50$Iters == '19000')
b <- subset.data.frame(ResNet101, ResNet101$Iters == '14000')
c <- subset.data.frame(dlcrnet_ms5, dlcrnet_ms5$Iters == '16000')

Networks <- rbind(a,b,c)

summary(glm(Networks$RMSE ~ Networks$Model,  family = 'poisson')) # all best model versions are significantly different to each other

#boxplot(Networks$RMSE ~ Networks$Model, xlab = 'Optimised neural networks', ylab='Pixel Error (RMSE)') # after comparison the dlcrnet model is the best model as it has the smallest overall RMSE between models

p <- ggplot(Networks, aes(Model, RMSE))  +
    ggtitle('Comparison of optimised networks trained on tube videos') +
    xlab('Neural networks') +
    ylab('Pixel Error (RMSE)') +
    stat_boxplot(geom ='errorbar',width = 0.5) + 
    geom_boxplot()+
  theme_classic()+
  theme(axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_signif(comparisons = list(c("dlcrnet_ms5", "resnet_101"),
                                 c("dlcrnet_ms5", "resnet_50"),
                                 c("resnet_101", "resnet_50")), 
              map_signif_level=TRUE, textsize = 4, col="black", vjust = 0.1,  y_position = c(15, 16,15))

p
#


#### Calculating the overall RMSE for each iteration of the ResNet_50 model

########

sum(ResNets$RMSE[ which(ResNets$Model == 'dlcrnet_ms5' & ResNets$Iters == '15000')])
mean(ResNets$RMSE[ which(ResNets$Model == 'dlcrnet_ms5' & ResNets$Iters == '15000')])
max(ResNets$RMSE[ which(ResNets$Model == 'dlcrnet_ms5' & ResNets$Iters == '15000')])
min(ResNets$RMSE[ which(ResNets$Model == 'dlcrnet_ms5' & ResNets$Iters == '15000')])

#####
#####

library('modelsummary')

