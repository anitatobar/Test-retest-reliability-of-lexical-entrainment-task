setwd("C:/Users/s1552951/Dropbox/U/Papers/Consistency/Data&Analysis")
#setwd("/home/anita/Dropbox/U/Papers/Consistency/Data&Analysis")

library(dplyr)
library(lmerTest)
library(psych)
library(multilevel)
library(nlme)
library(psychometric)
library(rptR)
library(ggplot2)
library(BBmisc)
library(car)
library(lawstat)
library(irr)
library(gridExtra)


##############################################################
#                         Exp 1 
##############################################################


Exp1_data1 <- read.csv(file= "Exp1_new.csv", header = TRUE, sep=",")
Exp1_data1 <- Exp1_data1 %>%
  dplyr::select(-c(X))

Exp1_data2 <- read.csv(file= "Exp1Data2_new", header = TRUE, sep=",")
Exp1_data2 <- Exp1_data2 %>%
  dplyr::select(-c(X))

summary(Exp1_data1)
summary(Exp1_data2)

Exp1 <- rbind(Exp1_data1 %>%
                dplyr::select(ID, ALIGN, CORRECT, TARGET, ROUND, condition, align_code), 
                Exp1_data2)

targetinfo <- distinct(Exp1_data1, TARGET, freq1, rating) 

Exp1_new <- merge(Exp1, targetinfo, by = "TARGET")

summary(Exp1_new)

Exp1_R1 <- Exp1_new %>%
  filter(ROUND == "1")

Exp1_R2 <- Exp1_new %>%
  filter(ROUND == "2")

Exp1_R1_sum <- Exp1_R1 %>% 
  dplyr::group_by(ID=as.factor(ID)) %>%
  dplyr::summarise(entrain1 = sum(as.numeric(as.character(align_code)), na.rm = TRUE))

Exp1_R2_sum <- Exp1_R2 %>% 
  dplyr::group_by(ID=as.factor(ID)) %>%
  dplyr::summarise(entrain2 = sum(as.numeric(as.character(align_code)), na.rm = TRUE))

align_rounds_new <- merge(Exp1_R1_sum, Exp1_R2_sum, by.y="ID", by.x="ID")

# export
# write.csv(align_rounds_new, file = "align_rounds_new.csv")

#-------------------------------#
#      Visualising data         #
#-------------------------------#

# 0. Proportion of use of disfavoured labels

EntrainmentProportion1 <- Exp1_new %>%
  group_by(ID, ROUND) %>%
  summarise(mean.ID = sum(as.numeric(as.character(align_code)), na.rm = TRUE)) %>%
  summarise(mean = mean(mean.ID), 
            part = n())

proportions_Entrainment1_rounds = Exp1_new %>%
  dplyr::group_by(ROUND, ID) %>%
  dplyr::summarise(align = mean(as.numeric(as.character(align_code)), na.rm=TRUE)) %>%
  dplyr::group_by(ROUND) %>%
  dplyr::summarise(Entrain.m = mean(align),
                   Entrain.sd = sd(align),
                   N = n(),
                   Entrain.se = Entrain.sd/sqrt(n()))
proportions_Entrainment1_rounds

proportions_Entrainment1 = Exp1_new %>%
  dplyr::group_by(ID) %>%
  dplyr::summarise(align = mean(as.numeric(as.character(align_code)), na.rm=TRUE)) %>%
  dplyr::summarise(Entrain.m = mean(align),
                   Entrain.sd = sd(align),
                   N = n(),
                   Entrain.se = Entrain.sd/sqrt(n()))
proportions_Entrainment1


# 1. Entrainment Effect 

align_target <- Exp1_new %>% 
  dplyr::group_by(TARGET) %>%
  dplyr::summarise(align_target = sum(as.numeric(as.character(align_code)), na.rm = TRUE))

summary(align_target)

align_target$align_target <- (align_target$align_target *100/ (length(Exp1_new$ID)/28) )

freq1 <- Exp1_new %>%
  dplyr::select(c("TARGET", "freq1")) 
freq1$freq1 <- as.numeric(freq1$freq1)

compare <- merge(freq1, align_target, by.y="TARGET", by.x="TARGET")
compare$align_target <- as.numeric(compare$align_target)
compare$freq1 <- as.numeric(compare$freq1)
compare <- compare %>% distinct(TARGET, .keep_all = TRUE)
compare

target <- rep(compare$TARGET, 2)
target

freq  <- c(compare$freq1, compare$align_target)
length(freq)
freq

c <- rep("spontaneous", 28)
d <- rep("primed", 28)

Answer <- c(c,d)

plot.table <- data.frame(target,freq,Answer)
head(plot.table)
summary(plot.table)
plot.table$freq <- as.numeric(plot.table$freq)
plot.table$target <- as.factor(plot.table$target)

plot1 <- ggplot(plot.table, aes(x=target, y=freq, group=Answer, color=Answer, linetype=Answer)) +
  geom_line(size=1) +
  geom_point() +
  theme(legend.position="right") + 
  labs(x = "Targets", y = "Percentage of use of disfavoured label", color = "Answer") + 
  theme_bw() +
  theme(
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.title.x = element_blank(), 
    axis.title.y = element_text(color="black", size=12, face="bold"), 
    legend.title = element_text(color="black", size=12, face="bold"), 
    axis.text.x = element_text(color="black", size=12, angle = 45, hjust = 1), 
    axis.text.y = element_text(color="black", size=12), 
    legend.text = element_text(color="black", size=12)) +
  expand_limits(y=c(0,100)) +
  scale_colour_manual(values=c("red3", "black"), 
                      name  ="Task",
                      breaks=c("primed", "spontaneous"),
                      labels=c("matching-and-naming", "spontaneous naming")) +
  scale_linetype_manual(values=c("solid", "dashed"), 
                     name  ="Task",
                     breaks=c("primed", "spontaneous"),
                     labels=c("matching-and-naming", "spontaneous naming"))
plot1


# 2. Entrainment per participant, per session 

length(levels(as.factor(Exp1_new$ID)))
ID <- rep(as.factor(align_rounds_new$ID), 2)
summary(ID)

align  <- c(align_rounds_new$entrain1, align_rounds_new$entrain2)
length(align)
align

session1 <- rep("Session1", 55)
session2 <- rep("Session2", 55)

Session <- c(session1,session2)

plot.table.ID <- data.frame(ID,align,Session)
head(plot.table.ID)
summary(plot.table.ID)

plotID <- ggplot(plot.table.ID, aes(x=reorder(ID, align), y=align, group=Session)) +
  geom_line(aes(linetype=Session), size=.5) +
  geom_point(aes(col=Session)) +
  scale_color_manual(values=c("black", "grey42")) + 
  scale_y_discrete(limits= c(0:14), breaks=c("2","4", "6", "8", "10", "12", "14")) +
  labs(x = "Participants ID", y = "Frequency of use of disfavoured label", 
       title = "B") +
  theme_bw() +
  theme(
    title = element_text(color="black", size=14, face="bold"),
    legend.position="right", 
    axis.title.x = element_text(color="black", size=10, face="bold"), 
    axis.title.y = element_text(color="black", size=10, face="bold"), 
    legend.title = element_text(color="black", size=10, face="bold"), 
    axis.text.x = element_blank(), 
    axis.text.y = element_text(color="black", size=10), 
    legend.text = element_text(color="black", size=10), 
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"))
plotID


# 3. Correlation

cor1 <- ggplot(align_rounds_new, aes(x=entrain1, y=entrain2))+
  geom_point(position = "jitter", size=1.5) + 
  geom_smooth(method=lm, size=1, col = "red", fullrange=TRUE) + 
  labs(x = "Frequency of use of disfavoured label in Session 1", 
       y = "Frequency of use of disfavoured label in Session 2", 
       title = "A")+
  theme_bw() +
  theme(
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"),
    title = element_text(color="black", size=14, face="bold"),
    axis.title.x = element_text(color="black", size=10, face="bold"), 
    axis.title.y = element_text(color="black", size=10, face="bold"), 
    axis.text.x = element_text(color="black", size=10), 
    axis.text.y = element_text(color="black", size=10)) +
  scale_x_discrete(limit = c(1:14)) +
  scale_y_discrete(limit = c(1:14)) 
cor1 

source("http://peterhaschke.com/Code/multiplot.R")
multiplot(cor1,plotID,cols=2)

# Items description
mean(Exp1_new$rating)
sd(Exp1_new$rating)

Exp1_C1_R1 <- Exp1_new %>%
  dplyr::filter(condition == "C1" &
                ROUND == "1")
mean(Exp1_C1_R1$freq1)
sd(Exp1_C1_R1$freq1)

Exp1_C1_R2 <- Exp1_new %>%
  dplyr::filter(condition == "C1" &
                  ROUND == "2")
mean(Exp1_C1_R2$freq1)
sd(Exp1_C1_R2$freq1)


#-----------------------------#
#       Analysis              #
#-----------------------------#

# Entrainment

mean(compare$freq1)
sd(compare$freq1)

test <- wilcox.test(compare$freq1, compare$align_target, paired = TRUE, exact=FALSE)
test

# Correlation

first_pearson <- cor.test(car::logit(align_rounds_new$entrain1/14, adjust=1/28),
                          car::logit(align_rounds_new$entrain2/14, adjust=1/28), 
                          paired =TRUE, 
                          method = ("pearson"))
first_pearson # 0.73 (.57 - .83)

# ICC
summary(plot.table.ID)
entrainment_exp1 <- plot.table.ID %>%
  dplyr::mutate(CenSession = ifelse(Session %in% c("Session1"), -1, 1), 
                CenAlignAdjusted = car::logit(align/14, adjust = 1/28),
                CenAlign = car::logit(align/14), 
                ID= as.factor(ID))


plot(entrainment_exp1$align, entrainment_exp1$CenAlignAdjusted, main="adjusted")
plot(entrainment_exp1$align, entrainment_exp1$CenAlign, main="default")


ICCdata_Exp1_logit <- align_rounds_new %>%
  na.omit() %>%
  dplyr::select(entrain1, entrain2) %>%
  dplyr::mutate(entrain1 = car::logit(entrain1/14, adjust = 1/28), 
                entrain2 = car::logit(entrain2/14, adjust = 1/28))

icc_exp1_logit <- icc(ICCdata_Exp1_logit, model = c("twoway"), 
                type = c("agreement"), 
                unit = c("single"), 
                conf.level = 0.95)
icc_exp1_logit


targets.order <- read.csv(file="TargetsOrderExp1&2.csv", header = TRUE, sep=",")
head(targets.order)

# ICC by hand: Repeatability with rptR ###

rpt_Exp1_gaussian_mean <- rpt(CenAlignAdjusted ~ (1|CenSession) + (1|ID),
                              grname = c("Fixed", "ID", "Residual"),
                              data = entrainment_exp1, 
                              datatype = "Gaussian", 
                              nboot = 1000, 
                              npermut = 0)
summary(rpt_Exp1_gaussian_mean)
print(rpt_Exp1_gaussian_mean)
plot(rpt_Exp1_gaussian_mean, grname = "ID", scale = "link", cex.main = 0.8)
rpt_Exp1_gaussian_mean$mod
qqnorm(resid(rpt_Exp1_gaussian_mean$mod), main="ResidualsExp1")
qqline(resid(rpt_Exp1_gaussian_mean$mod))
shapiro.test(resid(rpt_Exp1_gaussian_mean$mod)) # not normal, but should not be an issue since we're not reporting effect size here 
levene.test(resid(rpt_Exp1_gaussian_mean$mod), Session)


# Resampling analysis 

# Function takes a number of samples, a dataset1 (which should be by trial)
# and a dataset 2 (which should already be summarised)
# and returns a correlation 

sample_and_correlate = function(n_samples, 
                                dataset1 = Exp1_R1, 
                                dataset2 = Exp1_R2_sum){
  sample_Exp1_R1_sum = dataset1 %>%
    dplyr::group_by(ID=as.factor(ID)) %>%
    dplyr::sample_n(n_samples, replace=FALSE) %>%
    dplyr::summarise(entrain1 = sum(as.numeric(as.character(align_code)), na.rm = TRUE))
  
  align_rounds_new <- merge(sample_Exp1_R1_sum, 
                            dataset2, 
                            by.y="ID", 
                            by.x="ID")

  library("BBmisc")
  align_rounds_new$entrain1_normal <- scale(car::logit(align_rounds_new$entrain1/n_samples, adjust=1/28), center=TRUE)
  align_rounds_new$entrain2_normal <- scale(car::logit(align_rounds_new$entrain2/14, adjust=1/28), center=TRUE)
  
  first_pearson_normal <- cor.test(align_rounds_new$entrain1_normal, 
                                   align_rounds_new$entrain2_normal, 
                                   paired =TRUE, 
                                   method = ("pearson"))
  
  return(first_pearson_normal$estimate)
}


###Use the function

# Create a table with the number of samples that you want to take
a = tibble(n = rep(seq(1:14), each = 1000))
# Use apply to apply those number of samples to the function above
a$corr = apply(a[,1],1,FUN = sample_and_correlate) 

# generate means and 95% CIs
Exp1_Cors <- a %>%
  dplyr::group_by(n) %>%
  summarise(corr.m = mean(corr),
            corr.low.ci = quantile(corr, c(.025)),
            corr.high.ci = quantile(corr, c(.975)))

Exp1_Cors_plot = ggplot() + 
  geom_errorbar(data=Exp1_Cors, mapping=aes(x=n, ymin=corr.low.ci, ymax=corr.high.ci), width=0.2, size=1, color="black") + 
  geom_point(data=Exp1_Cors, mapping=aes(x=n, y=corr.m), size=4, shape=21, fill="white") +
  theme(legend.position="right") + 
  labs(x = "Number of Session 1 Trials", y = "Correlation Mean", title="Study 1") +  
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.title.x = element_text(color="black", size=12), 
    axis.title.y = element_text(color="black", size=12), 
    axis.text.x = element_text(color="black", size=12), 
    axis.text.y = element_text(color="black", size=12), 
    legend.text = element_text(color="black", size=12), 
    plot.title = element_text(color="black", size=12, face = "bold", hjust = 0.5)) +
  scale_y_continuous(breaks=seq(0,1,by=0.1), limits=c(0,1))+
  scale_x_continuous(
    breaks = c(1:14))
Exp1_Cors_plot

grid.table(Exp1_Cors)

## Resampling number of participants

# Function takes a number of samples, a dataset1 (complete df)
# and a dataset 2 (sampled df)
# and returns the same correlation that you were already 
# conducting


Exp1_part_resample <- align_rounds_new %>%
  na.omit() %>%
  dplyr::select(entrain1, entrain2) %>%
  dplyr::mutate(entrain1 = car::logit(entrain1/14, adjust = 1/28), 
                entrain2 = car::logit(entrain2/14, adjust = 1/28))

sample_and_correlate_part = function(n_samples, 
                                     fulldataset = Exp1_part_resample){ 
  
  samples <- fulldataset %>% #sample 10:55 rows 
    dplyr::select(entrain1, entrain2) %>%
    dplyr::sample_n(n_samples, replace=TRUE) 
  
  first_pearson_samples <- cor.test(samples$entrain1, #calculate pearson correlation 
                                   samples$entrain2, 
                                   paired =TRUE, 
                                   method = ("pearson"))
  return(first_pearson_samples$estimate) #return estimate
}


###Use the function

# Create a table with the number of samples that you want to take
a2 = tibble(n = rep(15:55, each = 1000))
# Use apply to apply those number of samples to the function above
a2$corr = apply(a2, 1, FUN = function(x) sample_and_correlate_part(x)) 

# generate means and 95% CIs
Exp1_Cors_Part <- a2 %>%
  na.omit() %>%
  dplyr::group_by(n) %>%
  summarise(corr.m = mean(corr, na.rm =TRUE),
            corr.low.ci = quantile(corr, c(.025)),
            corr.high.ci = quantile(corr, c(.975)))

Exp1_Cors_plot_part = ggplot() + 
  geom_errorbar(data=Exp1_Cors_Part, mapping=aes(x=n, ymin=corr.low.ci, ymax=corr.high.ci), width=0.2, size=.8, color="black") + 
  geom_point(data=Exp1_Cors_Part, mapping=aes(x=n, y=corr.m), size=3, shape=21, fill="white") +
  theme(legend.position="right") + 
  labs(x = "Sample Size", y = "Correlation Mean", title="Study 1") +  
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.title.x = element_text(color="black", size=12), 
    axis.title.y = element_text(color="black", size=12), 
    axis.text.x = element_text(color="black", size=9), 
    axis.text.y = element_text(color="black", size=12), 
    legend.text = element_text(color="black", size=12), 
    plot.title = element_text(color="black", size=12, face = "bold", hjust = 0.5)) +
  scale_y_continuous(breaks=seq(0,1,by=0.1), limits=c(0,1))+
  scale_x_continuous(breaks = seq(15, 55, by=5))
Exp1_Cors_plot_part

# Proportion of use of entrainment by trial 

Exp1_ordered <- merge(Exp1_new, 
                      targets.order, 
                      by.x= "TARGET", 
                      by.y = "target")
Exp1_ordered$order <- as.factor(Exp1_ordered$order)
summary(Exp1_ordered$order)

mean_target_Exp1 <- Exp1_ordered %>%
  dplyr::select(TARGET, align_code, order) %>%
  dplyr::group_by(order) %>%
  dplyr::summarise(align.target.m = mean(as.numeric(as.character(align_code)), na.rm = TRUE), 
                   align.target.sd = sd(as.numeric(as.character(align_code)), na.rm = TRUE),
                   n = n(),
                   align.target.se = align.target.sd/sqrt(n())) 

mean_target_plot_Exp1 = ggplot(mean_target_Exp1, aes(x = factor(order), y = align.target.m*100)) +
  geom_errorbar(aes(ymax=align.target.m*100+align.target.se*100,
                    ymin=align.target.m*100-align.target.se*100),
                position=position_dodge(width=0.5),
                width=0.2, 
                size=1, 
                color="black")+
  geom_point(stat="identity", size=4, shape=21, fill="white")+
  labs(x = "Trial", 
       y = "Proportion of Entrainment", 
       title = "Study 1") +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.title.x = element_text(color="black", size=12, face="bold"), 
    axis.title.y = element_text(color="black", size=12, face="bold"), 
    legend.title = element_text(color="black", size=12, face="bold"), 
    axis.text.x = element_text(color="black", size=12), 
    axis.text.y = element_text(color="black", size=12), 
    legend.text = element_text(color="black", size=12), 
    plot.title = element_text(color = "black", size = 12, face = "bold", hjust = 0.5)) +
  expand_limits(y=c(0,100))
mean_target_plot_Exp1


# Trials order effects?

summary(Exp1_ordered)
Exp1_ordered$logorder <- log(as.numeric(as.character(Exp1_ordered$order)))
Exp1_ordered$ScaleOrder <- scale(as.numeric(as.character(Exp1_ordered$order)))

Exp1_ordered_null <- glmer(align_code ~ 1 + (1|TARGET) + (1+ScaleOrder|ID), 
                            data = Exp1_ordered, family=binomial, 
                            na.action = na.omit, 
                            glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))

Exp1_ordered_order <- glmer(align_code ~ 1 +ScaleOrder + (1|TARGET) + (1+ScaleOrder|ID), 
                           data = Exp1_ordered, family=binomial, 
                           na.action = na.omit, 
                           glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))

summary(Exp1_ordered_order)
anova(Exp1_ordered_null, Exp1_ordered_order) #no





####################################################
#                EXPERIMENT 2
####################################################

Exp2_data1 <- read.csv(file= "Exp2_new.csv", header = TRUE, sep=",")
Exp2_data1 <- Exp2_data1 %>%
  dplyr::select(-c(X)) %>%
  rename(TARGET = ITEM)

Exp2_data2 <- read.csv(file= "Exp2Data2_new", header = TRUE, sep=",")
Exp2_data2 <- Exp2_data2 %>%
  dplyr::select(-c(X))

summary(Exp2_data1)
summary(Exp2_data2)

Exp2 <- rbind(Exp2_data1 %>%
                dplyr::select(newID, ALIGN, CORRECT, TARGET, ROUND, condition, align_code), 
              Exp2_data2)

targetinfo2 <- distinct(Exp2_data1, TARGET, freq1, rating) 

Exp2_new <- merge(Exp2, targetinfo2, by = "TARGET")

Exp2_new <- within(Exp2_new, align_code [newID== "26" & TARGET== "spectacles"] <- "0")

summary(Exp2_new)

Exp2_R1 <- Exp2_new %>%
  filter(ROUND == "1")

Exp2_R2 <- Exp2_new %>%
  filter(ROUND == "2")

Exp2_R1_sum <- Exp2_R1 %>% 
  dplyr::group_by(ID=as.factor(newID)) %>%
  dplyr::summarise(entrain1 = sum(as.numeric(as.character(align_code)), na.rm = TRUE))

Exp2_R2_sum <- Exp2_R2 %>% 
  dplyr::group_by(ID=as.factor(newID)) %>% 
  dplyr::summarise(entrain2 = sum(as.numeric(as.character(align_code)), na.rm = TRUE))

align_rounds_new2 <- merge(Exp2_R1_sum, Exp2_R2_sum, by.y="ID", by.x="ID")

# export
# write.csv(align_rounds_new, file = "align_rounds_new.csv")

#-------------------------------#
#      Visualising data         #
#-------------------------------#

# 0. Proportion of Entrainment 

proportions_Entrainment2_rounds = Exp2_new %>%
  dplyr::group_by(ROUND, newID) %>%
  dplyr::summarise(align = mean(as.numeric(as.character(align_code)), na.rm=TRUE)) %>%
  dplyr::group_by(ROUND) %>%
  dplyr::summarise(Entrain.m = mean(align),
                   Entrain.sd = sd(align),
                   N = n(),
                   Entrain.se = Entrain.sd/sqrt(n()))
proportions_Entrainment2_rounds

proportions_Entrainment2 = Exp2_new %>%
  dplyr::group_by(newID) %>%
  dplyr::summarise(align = mean(as.numeric(as.character(align_code)), na.rm=TRUE)) %>%
  dplyr::summarise(Entrain.m = mean(align),
                   Entrain.sd = sd(align),
                   N = n(),
                   Entrain.se = Entrain.sd/sqrt(n()))
proportions_Entrainment2


# 1. Entrainment Effect 

entrain_target <- Exp2_new %>% 
  dplyr::group_by(TARGET) %>%
  dplyr::summarise(align_target = sum(as.numeric(as.character(align_code)), na.rm = TRUE))
entrain_target$align_target <- (entrain_target$align_target *100/ (length(Exp2_new$newID)/28) )
summary(entrain_target)

freq1_2 <- Exp2_new %>%
  dplyr::select(c("TARGET", "freq1")) 
freq1_2$freq1 <- as.numeric(freq1_2$freq1)
#freq1 <- freq1[1:28,]

compare2 <- merge(freq1_2, entrain_target, by.y="TARGET", by.x="TARGET")
compare2$align_target <- as.numeric(compare2$align_target)
compare2$freq1 <- as.numeric(compare2$freq1)
compare2 <- compare2 %>% distinct(TARGET, .keep_all = TRUE)
compare2

target2 <- rep(compare2$TARGET, 2)
target2

freq2  <- c(compare2$freq1, compare2$align_target)
length(freq2)
freq2

c2 <- rep("spontaneous", 28)
d2 <- rep("primed", 28)

answer2 <- c(c2,d2)

plot.table2 <- data.frame(target2,freq2,answer2)
head(plot.table2)
summary(plot.table2)
plot.table2$freq2 <- as.numeric(plot.table2$freq2)
plot.table2$target2 <- as.factor(plot.table2$target2)


plot1_2 <- ggplot(plot.table2, aes(x=target2, y=freq2, group=answer2, linetype=answer2)) +
  geom_line(aes(linetype=answer2, color=answer2), size=1) +
  geom_point(aes(color=answer2)) +
  theme(legend.position="right") + 
  labs(x = "Targets", y = "Percentage of use of disfavoured label") + 
  theme_bw() +
  theme(
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.title.x = element_blank(), 
    axis.title.y = element_text(color="black", size=12, face="bold"), 
    legend.title = element_text(color="black", size=12, face="bold"), 
    axis.text.x = element_text(color="black", size=12, angle = 45, hjust = 1), 
    axis.text.y = element_text(color="black", size=12), 
    legend.text = element_text(color="black", size=12)) +
  expand_limits(y=c(0,100)) +
  scale_colour_manual(values=c("red3", "black"), 
                      name  ="Task",
                      breaks=c("primed", "spontaneous"),
                      labels=c("matching-and-naming", "spontaneous naming")) +
  scale_linetype_manual(values=c("solid", "dashed"), 
                        name  ="Task",
                        breaks=c("primed", "spontaneous"),
                        labels=c("matching-and-naming", "spontaneous naming"))
plot1_2

# 2. Entrainment per participant, per session 

align2_ordered <- with(align_rounds_new2, align_rounds_new2[order(as.numeric(ID)),])
head(align2_ordered)
length(align2_ordered$ID)

ID2 <- align2_ordered$ID
ID2

align2  <- c(align2_ordered$entrain1, align2_ordered$entrain2)
length(align2)
align2

session1 <- rep("Session1", 45)
session2 <- rep("Session2", 45)

Session <- c(session1,session2)

plot.table.ID2 <- data.frame(ID2,align2,Session)
head(plot.table.ID2)
summary(plot.table.ID2)

plotID2 <- ggplot(plot.table.ID2, aes(x=reorder(ID2, align2), y=align2, group=Session)) +
  geom_line(aes(linetype=Session, color=Session), size=.5) +
  geom_point(aes(col=Session)) +
  scale_color_manual(values=c("black", "grey42")) + 
  scale_y_discrete(limits= c(0:14), breaks=c("2","4", "6", "8", "10", "12", "14")) +
  labs(x = "Participants ID", y = "Frequency of use of disfavoured label", 
       title = "B") +
  theme_bw() +
  theme(
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"),
    title = element_text(color="black", size=14, face="bold"),
    legend.position="right", 
    axis.title.x = element_text(color="black", size=10, face="bold"), 
    axis.title.y = element_text(color="black", size=10, face="bold"), 
    legend.title = element_text(color="black", size=10, face="bold"), 
    axis.text.x = element_blank(), 
    axis.text.y = element_text(color="black", size=10), 
    legend.text = element_text(color="black", size=10)) 

plotID2


# 3. Correlation

cor2 <- ggplot(align_rounds_new2, aes(x=entrain1, y=entrain2))+
  geom_point(position = "jitter", size=1.5) + 
  geom_smooth(method=lm, size=1, col = "red", fullrange=TRUE) + 
  labs(x = "Frequency of use of disfavoured label in Session 1", 
       y = "Frequency of use of disfavoured label in Session 2", 
       title = "A")+
  theme_bw() +
  theme(
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"),
    title = element_text(color="black", size=14, face="bold"),
    axis.title.x = element_text(color="black", size=10, face="bold"), 
    axis.title.y = element_text(color="black", size=10, face="bold"), 
    axis.text.x = element_text(color="black", size=10), 
    axis.text.y = element_text(color="black", size=10)) +
  scale_x_discrete(limit = c(1:14)) +
  scale_y_discrete(limit = c(1:13)) 
cor2 

source("http://peterhaschke.com/Code/multiplot.R")
multiplot(cor2,plotID2,cols=2)


#-----------------------------#
#       Analysis              #
#-----------------------------#

# Entrainment

test2 <- wilcox.test(compare2$freq1, compare2$align_target, paired = TRUE, exact=FALSE)
test2

# Correlations

second_pearson <- cor.test(car::logit(align_rounds_new2$entrain1/14, adjust = 1/28), 
                           car::logit(align_rounds_new2$entrain2/14, adjust = 1/28), paired =TRUE, method = ("pearson"))
second_pearson # 0.61 (0.39 - .77)

# ICC
summary(plot.table.ID2)
entrainment_exp2 <- plot.table.ID2 %>%
  dplyr::mutate(CenSession = ifelse(Session %in% c("Session1"), -1, 1), 
                CenAlignAdjusted = car::logit(align2/14, adjust=1/28),
                CenAlign = car::logit(align2/14),
                ID= as.factor(ID2))

plot(entrainment_exp2$align2,entrainment_exp2$CenAlign, main="default")
plot(entrainment_exp2$align2,entrainment_exp2$CenAlignAdjusted, main="adjusted")

ICCdata_Exp2_logit <- align_rounds_new2 %>%
  na.omit() %>%
  dplyr::select(entrain1, entrain2) %>%
  dplyr::mutate(entrain1 = car::logit(entrain1/14, adjust=1/28), 
                entrain2 = car::logit(entrain2/14, adjust=1/28))

icc_exp2_logit <- icc(ICCdata_Exp2_logit, model = c("twoway"), 
                      type = c("agreement"), 
                      unit = c("single"), 
                      conf.level = 0.95)
icc_exp2_logit


# ICC by hand: Repeatability with rptR ###

rpt_Exp2_gaussian_mean <- rpt(CenAlignAdjusted ~ (1|CenSession) + (1|ID),
                              grname = c("Fixed", "ID", "Residual"),
                              data = entrainment_exp2, 
                              datatype = "Gaussian", 
                              nboot = 1000, 
                              npermut = 0)
summary(rpt_Exp2_gaussian_mean)
print(rpt_Exp2_gaussian_mean)
plot(rpt_Exp2_gaussian_mean, grname = "ID", scale = "link", cex.main = 0.8)
rpt_Exp2_gaussian_mean$mod
qqnorm(resid(rpt_Exp2_gaussian_mean$mod), main = "Residuals Exp2")
qqline(resid(rpt_Exp2_gaussian_mean$mod))
shapiro.test(resid(rpt_Exp2_gaussian_mean$mod)) # not normal
levene.test(resid(rpt_Exp2_gaussian_mean$mod), Session)


# Trials 

# Function takes a number of samples, a dataset1 (which should be be trial)
# and a dataset 2 (which should already be summarised)
# and returns the same correlation that you were already 
# conducting
sample_and_correlate2 = function(n_samples, 
                                 dataset1 = Exp2_R1, 
                                 dataset2 = Exp2_R2_sum){
  sample_Exp2_R1_sum = dataset1 %>%
    dplyr::group_by(ID=as.factor(newID)) %>%
    dplyr::sample_n(n_samples, replace=FALSE) %>%
    dplyr::summarise(entrain1 = sum(as.numeric(as.character(align_code)), na.rm = TRUE))
  
  align_rounds_new2 <- merge(sample_Exp2_R1_sum, 
                             dataset2, 
                             by.y="ID", 
                             by.x="ID")
  library("BBmisc")
  align_rounds_new2$entrain1_normal <- car::logit(align_rounds_new2$entrain1/n_samples, adjust=1/28)
  align_rounds_new2$entrain2_normal <- car::logit(align_rounds_new2$entrain2/14, adjust=1/28)
  
  first_pearson_normal2 <- cor.test(align_rounds_new2$entrain1_normal, 
                                    align_rounds_new2$entrain2_normal, 
                                    paired =TRUE, 
                                    method = ("pearson"))
  
  return(first_pearson_normal2$estimate)
}


###Use the function

# Create a table with the number of samples that you want to take
b = tibble(n = rep(seq(1:14), each = 1000))
# Use apply to apply those number of samples to the function above
b$corr = apply(b[,1],1,FUN = sample_and_correlate2)

# generate means and 95% CIs
Exp2_Cors <- b %>%
  dplyr::group_by(n) %>%
  summarise(corr.m = mean(corr),
            corr.low.ci = quantile(corr, c(.025)),
            corr.high.ci = quantile(corr, c(.975)))

Exp2_Cors_plot = ggplot() + 
  geom_errorbar(data=Exp2_Cors, mapping=aes(x=n, ymin=corr.low.ci, ymax=corr.high.ci), width=0.2, size=1, color="black") + 
  geom_point(data=Exp2_Cors, mapping=aes(x=n, y=corr.m), size=4, shape=21, fill="white") +
  theme(legend.position="right") + 
  labs(x = "Number of Session 1 Trials", y = "Correlation Mean", title="Study 2") + 
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.title.x = element_text(color="black", size=12), 
    axis.title.y = element_text(color="black", size=12), 
    axis.text.x = element_text(color="black", size=12), 
    axis.text.y = element_text(color="black", size=12), 
    legend.text = element_text(color="black", size=12), 
    plot.title = element_text(color="black", size=12, face = "bold", hjust = 0.5)) +
  scale_y_continuous(breaks=seq(0,1,by=0.1), limits=c(0,1))+
  scale_x_continuous(
    breaks = c(1:14))

Exp2_Cors_plot


source("http://peterhaschke.com/Code/multiplot.R")
multiplot(Exp1_Cors_plot,Exp2_Cors_plot,cols=2)

## Resampling number of participants

# Function takes a number of samples, a dataset1 (complete df)
# and a dataset 2 (sampled df)
# and returns the same correlation that you were already 
# conducting


Exp2_part_resample <- align_rounds_new2 %>%
  na.omit() %>%
  dplyr::select(entrain1, entrain2) %>%
  dplyr::mutate(entrain1 = car::logit(entrain1/14, adjust = 1/28), 
                entrain2 = car::logit(entrain2/14, adjust = 1/28))

sample_and_correlate_part2 = function(n_samples, 
                                     fulldataset = Exp2_part_resample){ 
  
  samples <- fulldataset %>% #sample  
    dplyr::select(entrain1, entrain2) %>%
    dplyr::sample_n(n_samples, replace=TRUE) 
  
  second_pearson_samples <- cor.test(samples$entrain1, #calculate pearson correlation 
                                    samples$entrain2, 
                                    paired =TRUE, 
                                    method = ("pearson"))
  return(second_pearson_samples$estimate) #return estimate
}


###Use the function

# Create a table with the number of samples that you want to take
b2 = tibble(n = rep(15:45, each = 1000))
# Use apply to apply those number of samples to the function above
b2$corr = apply(b2, 1, FUN = function(x) sample_and_correlate_part2(x)) 

# generate means and 95% CIs
Exp2_Cors_Part <- b2 %>%
  na.omit() %>%
  dplyr::group_by(n) %>%
  summarise(corr.m = mean(corr, na.rm=TRUE),
            corr.low.ci = quantile(corr, c(.025)),
            corr.high.ci = quantile(corr, c(.975)))

Exp2_Cors_plot_part = ggplot() + 
  geom_errorbar(data=Exp2_Cors_Part, mapping=aes(x=n, ymin=corr.low.ci, ymax=corr.high.ci), width=0.2, size=.8, color="black") + 
  geom_point(data=Exp2_Cors_Part, mapping=aes(x=n, y=corr.m), size=3, shape=21, fill="white") +
  theme(legend.position="right") + 
  labs(x = "Sample Size", y = "Correlation Mean", title="Study 2") +  
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.title.x = element_text(color="black", size=12), 
    axis.title.y = element_text(color="black", size=12), 
    axis.text.x = element_text(color="black", size=9), 
    axis.text.y = element_text(color="black", size=12), 
    legend.text = element_text(color="black", size=12), 
    plot.title = element_text(color="black", size=12, face = "bold", hjust = 0.5)) +
  scale_y_continuous(breaks=seq(0,1,by=0.1), limits=c(0,1))+
  scale_x_continuous(breaks = seq(3, 45, by=5))
Exp2_Cors_plot_part

source("http://peterhaschke.com/Code/multiplot.R")
multiplot(Exp1_Cors_plot_part,Exp2_Cors_plot_part,cols=2)


# Proportions of entrainment by trial in Exp2 
Exp2_ordered <- merge(Exp2_new, 
                      targets.order, 
                      by.x= "TARGET", 
                      by.y = "target")
Exp2_ordered$order <- as.factor(Exp2_ordered$order)
summary(Exp2_ordered$order)

mean_target_Exp2 <- Exp2_ordered %>%
  dplyr::select(TARGET, align_code, order) %>%
  dplyr::group_by(order) %>%
  dplyr::summarise(align.target.m = mean(as.numeric(as.character(align_code)), na.rm = TRUE), 
                   align.target.sd = sd(as.numeric(as.character(align_code)), na.rm = TRUE),
                   n = n(),
                   align.target.se = align.target.sd/sqrt(n())) 

mean_target_plot_Exp2 = ggplot(mean_target_Exp2, aes(x = factor(order), y = align.target.m*100)) +
  geom_errorbar(aes(ymax=align.target.m*100+align.target.se*100,
                    ymin=align.target.m*100-align.target.se*100),
                position=position_dodge(width=0.5),
                width=0.2, 
                size=1, 
                color="black")+
  geom_point(stat="identity", size=4, shape=21, fill="white")+
  labs(x = "Trial", 
       y = "Proportion of Entrainment", 
       title = "Study 2") +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.title.x = element_text(color="black", size=12, face="bold"), 
    axis.title.y = element_text(color="black", size=12, face="bold"), 
    legend.title = element_text(color="black", size=12, face="bold"), 
    axis.text.x = element_text(color="black", size=12), 
    axis.text.y = element_text(color="black", size=12), 
    legend.text = element_text(color="black", size=12), 
    plot.title = element_text(color = "black", size = 12, face = "bold", hjust = 0.5)) +
  expand_limits(y=c(0,100))
mean_target_plot_Exp2


source("http://peterhaschke.com/Code/multiplot.R")
multiplot(mean_target_plot_Exp1,mean_target_plot_Exp2,cols=2)

# trials effect??

summary(Exp2_ordered)
Exp2_ordered$newID <- as.factor(Exp2_ordered$newID)
Exp2_ordered$align_code <- as.factor(Exp2_ordered$align_code)
Exp2_ordered$ScaleOrder <- scale(as.numeric(as.character(Exp2_ordered$order)))

Exp2_ordered_null <- glmer(align_code ~ 1 + (1|TARGET) + (1+ScaleOrder|newID), 
                           data = Exp2_ordered, family=binomial, 
                           na.action = na.omit, 
                           glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))

Exp2_ordered_order <- glmer(align_code ~ 1 +ScaleOrder + (1|TARGET) + (1+ScaleOrder|newID), 
                            data = Exp2_ordered, family=binomial, 
                            na.action = na.omit, 
                            glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))

summary(Exp2_ordered_order)
anova(Exp2_ordered_null, Exp2_ordered_order) # nope: if anything, entrainment decreases, thus the task does not 
#lead to maximised effects


#################################################
#           Exp 1 and Exp 2 together            #
#################################################

ICCdata_all <- rbind(ICCdata_Exp1_logit, ICCdata_Exp2_logit)

entrainment_exp1and2 <- rbind(entrainment_exp1 %>%
                                dplyr::select(ID,align,CenSession,CenAlignAdjusted) %>%
                                dplyr::mutate(Exp = 1), 
                              entrainment_exp2 %>%
                                dplyr::select(ID,align2,CenSession,CenAlignAdjusted) %>%
                                dplyr::rename(align = align2) %>%
                                dplyr::mutate(Exp = 2)) %>%
  dplyr::mutate(CenExp = ifelse(Exp %in% c("1"), -1, 1))
summary(entrainment_exp1and2)

entrainment_exp1and2_Delta <- rbind(plot.table.ID2 %>%
  tidyr::spread(Session, align2) %>%
  dplyr::mutate(Exp = 2) %>%
  dplyr::rename(ID = ID2), 
    plot.table.ID %>%
  tidyr::spread(Session, align) %>%
  dplyr::mutate(Exp = 1)) %>%
  dplyr::mutate(CenSession1 = car::logit(Session1/14, adjust = 1/28), 
                CenSession2 = car::logit(Session2/14, adjust = 1/28), 
                CenDelta = CenSession1 - CenSession2, 
                Delta = Session1-Session2, 
                CenExp = ifelse(Exp %in% c("1"), -1, 1))
summary(entrainment_exp1and2_Delta)


pearson_all <- cor.test(ICCdata_all$entrain1,  
                        ICCdata_all$entrain2, 
                        paired =TRUE, method = ("pearson"))
pearson_all

ICC(ICCdata_all)

icc_exp1and2 <- icc(ICCdata_all, model = c("twoway"), 
                    type = c("agreement"), 
                    unit = c("single"), 
                    conf.level = 0.95)
icc_exp1and2

# ICC by hand: Repeatability with rptR ###

rpt_Exp1and2_gaussian_mean <- rpt(CenAlignAdjusted ~ (1|CenExp) + (1|CenSession) + (1|ID),
                              grname = c("ID", "Residual", "CenSession", "CenExp"),
                              data = entrainment_exp1and2, 
                              datatype = "Gaussian", 
                              nboot = 1000, 
                              npermut = 0)
summary(rpt_Exp1and2_gaussian_mean)
print(rpt_Exp1and2_gaussian_mean)
plot(rpt_Exp1and2_gaussian_mean, grname = "ID", scale = "link", cex.main = 0.8)
rpt_Exp1and2_gaussian_mean$mod
qqnorm(resid(rpt_Exp1and2_gaussian_mean$mod), main="Residuals Exp1and2")
qqline(resid(rpt_Exp1and2_gaussian_mean$mod))
shapiro.test(resid(rpt_Exp1and2_gaussian_mean$mod)) # not normal
levene.test(resid(rpt_Exp1and2_gaussian_mean$mod), entrainment_exp1and2$CenSession)
levene.test(resid(rpt_Exp1and2_gaussian_mean$mod), entrainment_exp1and2$CenExp)

rpt_Exp1and2_gaussian_mean_fixed <- rpt(CenAlignAdjusted ~ CenExp + (1|CenSession) + (1|ID),
                                  grname = c("ID", "Residual", "CenSession", "Fixed"),
                                  data = entrainment_exp1and2, 
                                  datatype = "Gaussian", 
                                  nboot = 1000, 
                                  npermut = 0)
print(rpt_Exp1and2_gaussian_mean_fixed)
plot(rpt_Exp1and2_gaussian_mean_fixed, grname = "ID", scale = "link", cex.main = 0.8)
rpt_Exp1and2_gaussian_mean$mod
qqnorm(resid(rpt_Exp1and2_gaussian_mean$mod), main="Residuals Exp1and2")
qqline(resid(rpt_Exp1and2_gaussian_mean$mod))
shapiro.test(resid(rpt_Exp1and2_gaussian_mean$mod)) # not normal
levene.test(resid(rpt_Exp1and2_gaussian_mean$mod), entrainment_exp1and2$CenSession)
levene.test(resid(rpt_Exp1and2_gaussian_mean$mod), entrainment_exp1and2$CenExp)

lmer_all <- lm(CenDelta ~ CenExp, 
                 data = entrainment_exp1and2_Delta)
summary(lmer_all)

DeltaExp <- entrainment_exp1and2_Delta %>%
  dplyr::select(ID, Exp, Delta) %>%
  dplyr::group_by(Exp) %>%
  dplyr::summarise(MeanDelta = mean(Delta), 
                SdDelta = sd(Delta))


ICC1.lme(CenAlignAdjusted, CenExp, data=entrainment_exp1and2)


#--- 
describe(ICCdata_all)
summary(ICCdata_all)

proportions <- ICCdata_all %>%
  dplyr::mutate(mean1 = entrain1*100/14, 
                mean2 = entrain2*100/14) %>%
  dplyr::mutate(session1 = mean(mean1),
                session2 = mean(mean2), 
                sd1 = sd(mean1), 
                sd2 = sd(mean2))


#######################################################################
#            Extra Analyses in response to reviewers' comments         #
#######################################################################

# 1.a WordLength and Entrainment, in reponse to reviewr's suggestion

WordLength <- read.csv(file= "WordLength.csv", header = TRUE, sep=",")
describe(WordLength)

Exp1and2 <- merge(rbind(Exp1_new %>% 
                    dplyr::select(ID,TARGET, align_code),
                  Exp2_new %>% 
                    dplyr::select(newID,TARGET, align_code)%>%
                    dplyr::rename(ID=newID)) %>%
  dplyr::group_by(TARGET) %>%
  dplyr::summarise(entrain = mean(as.numeric(as.character(align_code)), na.rm = TRUE)*100,
                   part = n()), 
  WordLength %>%
    dplyr::select(Target, FavLetters, DisfLetters, DiffLetters), 
  by.x="TARGET", by.y="Target")


cor.test(scale(as.numeric(as.character(Exp1and2$entrain))),scale(as.numeric(as.character(Exp1and2$DiffLetters))))
testLength <- wilcox.test(Exp1and2$DisfLetters, Exp1and2$entrain, paired = TRUE, exact=FALSE)
testLength


Exp1and2_regression <- merge(rbind(Exp1_new %>% 
                          dplyr::select(ID,TARGET, align_code),
                        Exp2_new %>% 
                          dplyr::select(newID,TARGET, align_code)%>%
                          dplyr::rename(ID=newID)), 
                  WordLength %>%
                    dplyr::select(Target, FavLetters, DisfLetters, DiffLetters, Diff) %>%
                    dplyr::mutate(CenDiff = ifelse(Diff %in% c("yes"), -1, 1)), 
                  by.x="TARGET", by.y="Target") %>%
  dplyr::mutate(TARGET = as.factor(TARGET), 
                ID = as.factor(ID))

cor_wordlength <- ggplot(Exp1and2, 
                         aes(x=as.numeric(as.character(Exp1and2$entrain)), 
                             y=as.numeric(as.character(Exp1and2$DiffLetters))))+
  geom_point(position = "jitter", size=1.5) + 
  geom_smooth(method=lm, size=1, col = "red", fullrange=TRUE) + 
  labs(x = "Frequency of use of disfavoured label in Session 1", 
       y = "Length of Disfavoured label (number of characters)", 
       title = "A")+
  theme_bw() +
  theme(
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"),
    title = element_text(color="black", size=14, face="bold"),
    axis.title.x = element_text(color="black", size=10, face="bold"), 
    axis.title.y = element_text(color="black", size=10, face="bold"), 
    axis.text.x = element_text(color="black", size=10), 
    axis.text.y = element_text(color="black", size=10))
cor_wordlength 


entrain_wordlength <- glmer(as.factor(align_code) ~ CenDiff + (1|TARGET) + (1+CenDiff|ID), 
                            data = Exp1and2_regression, family=binomial, 
                            na.action = na.omit, 
                            glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(entrain_wordlength)


#Length <- LengthTable %>%
#  dplyr::select(target, freq, FavLetters, DisfLetters)

#LengthTable_long <- Length %>%
#  tidyr::gather(Response, Frequency, freq:DisfLetters)

# Frequence of use of disfavoured names

align_target_freq1 <- rbind(Exp1_new %>% dplyr::select(TARGET,freq1,align_code),
                            Exp2_new %>% dplyr::select(TARGET,freq1,align_code)) %>%
                              dplyr::group_by(TARGET, freq1) %>%
                              dplyr::summarise(entrain = mean(as.numeric(as.character(align_code)), na.rm = TRUE)*100)
summary(align_target_freq1)


cor_freq <- ggplot(align_target_freq1, aes(x=freq1, y=entrain))+
  geom_point(position = "jitter", size=1.5) + 
  geom_smooth(method=lm, size=1, col = "red", fullrange=TRUE) + 
  labs(x = "Frequency of use of disfavoured label in spontaneous task", 
       y = "Frequency of use of disfavoured label in main experiment", 
       title = "A")+
  theme_bw() +
  theme(
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"),
    title = element_text(color="black", size=14, face="bold"),
    axis.title.x = element_text(color="black", size=10, face="bold"), 
    axis.title.y = element_text(color="black", size=10, face="bold"), 
    axis.text.x = element_text(color="black", size=10), 
    axis.text.y = element_text(color="black", size=10))

cor_freq

cor.test(align_target_freq1$freq1, align_target_freq1$entrain, method="Pearson", significant)

align_target_freq1_corrected <- rbind(Exp1_new %>% dplyr::select(TARGET,freq1,align_code),
                                      Exp2_new %>% dplyr::select(TARGET,freq1,align_code)) %>%
  dplyr::filter(freq1 < 10) %>%
  dplyr::group_by(TARGET, freq1) %>%
  dplyr::summarise(entrain = mean(as.numeric(as.character(align_code)), na.rm = TRUE)*100)
summary(align_target_freq1_corrected)

cor_freq_corrected <- ggplot(align_target_freq1_corrected, aes(x=freq1, y=entrain))+
  geom_point(position = "jitter", size=1.5) + 
  geom_smooth(method=lm, size=1, col = "red", fullrange=TRUE) + 
  labs(x = "Frequency of use of disfavoured label in spontaneous task", 
       y = "Frequency of use of disfavoured label in main experiment", 
       title = "A")+
  theme_bw() +
  theme(
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"),
    title = element_text(color="black", size=14, face="bold"),
    axis.title.x = element_text(color="black", size=10, face="bold"), 
    axis.title.y = element_text(color="black", size=10, face="bold"), 
    axis.text.x = element_text(color="black", size=10), 
    axis.text.y = element_text(color="black", size=10))
cor_freq_corrected

cor.test(scale(align_target_freq1_corrected$freq1), scale(align_target_freq1_corrected$entrain), method="pearson")

align_target_freq1_regress <- merge(rbind(Exp1_new %>% dplyr::select(ID, TARGET,freq1,align_code,condition),
                            Exp2_new %>% dplyr::select(newID, TARGET,freq1,align_code,condition) %>%
                              dplyr::rename(ID=newID)), 
                            targets.order, 
                            by.x = "TARGET", 
                            by.y = "target") %>%
  dplyr::mutate(ID=as.factor(ID), 
                align_code=as.factor(align_code),
                order = as.factor(order))
summary(align_target_freq1_regress)

Exp1_ordered_freq1 <- glmer(align_code ~ freq1 + (1|TARGET) + (1|ID), 
                            data = subset(align_target_freq1_regress, condition == "C1"), family=binomial, 
                            na.action = na.omit, 
                            glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))

summary(Exp1_ordered_freq1)

Exp1_ordered_freq2 <- glmer(align_code ~ freq1 + (1|TARGET) + (1+freq1|ID), 
                            data = subset(align_target_freq1_regress, condition == "C2"), family=binomial, 
                            na.action = na.omit, 
                            glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))

summary(Exp1_ordered_freq2)
