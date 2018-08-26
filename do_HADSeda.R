# Table of HADS count

require(dplyr)
detach(package:plyr)
dat1 <- dat %>% group_by(pid) %>% summarise(hads_count = sum(!is.na(hads)))
dat2 <- left_join(dat.id, dat1)
tab.hads.count <- as.data.frame(table(dat2$hads_count)) # For the big 5 cancers only
write_csv(tab.hads.count, "tabhads.csv")

# Table of demographics ------
source("./scripts/tabbig5.R")

# Create table of HADS freq #####

a.dat.id <- a.dat %>% distinct(pid, .keep_all=T) %>% 
  select(pid:mdt, dep:t3)

require(dplyr)
detach(package:plyr)
dat1 <- dat %>% group_by(pid) %>% summarise(hads_count = sum(!is.na(hads)))
dat2 <- left_join(dat.id, dat1)
tab.hads.count <- as.data.frame(table(dat2$hads_count)) # For the big 5 cancers only
write_csv(tab.hads.count, "tabhads.csv")

# Tab of HADS freq by dep status #####

dat1 <- dat 
dat1 <- dat1 %>% group_by(pid) %>% mutate(penul_dep = nth(dep, -2))
dat1 <- dat1 %>% group_by(pid) %>% 
  mutate(penul_dep = ifelse(hads_count<2, NA, penul_dep))
dat1 <- dat1 %>% group_by(pid) %>% mutate(last_dep = last(dep))
dat2 <- dat1 %>% distinct(pid, .keep_all=T)

tab.pdep <- dat2 %>% group_by(vstat) %>% count(penul_dep)
tab.ldep <- dat2 %>% group_by(vstat) %>% count(last_dep)
tab.depfreq <- bind_cols(tab.pdep, tab.ldep)
tab.depfreq
write.csv(tab.depfreq, "./tabdepfreq.csv")

rm(list=ls(pattern="tab"))

# Plots of HADS durations

ctrldat <- readRDS("./output_data/ctrldat.rds")
library(e1071)

d.t1 <- ggplot(d.dat, aes(t1))
d.t2 <- ggplot(d.dat, aes(t2))
d.t3 <- ggplot(d.dat, aes(t3))
d.age <- ggplot(d.dat, aes(age))

# t1
ggplot(ctrldat, aes(t1, colour = vstat)) + 
  geom_histogram(binwidth = 180) + 
  labs(x="Time (days)", y = "Frequency") +
  scale_colour_discrete(name = "Vital Status")

#t2
ggplot(ctrldat, aes(t2, colour = vstat)) + 
  geom_histogram(binwidth = 30) + 
  labs(x="Time (days)", y = "Frequency") + 
  scale_colour_discrete(name = "Vital Status")

#t3
ggplot(ctrldat, aes(t3, colour = vstat)) + 
  geom_histogram(binwidth = 30)+ 
  labs(x="Time (days)", y = "Frequency") +
  scale_colour_discrete(name = "Vital Status")

# Age
ggplot(ctrldat, aes(age, colour = vstat)) + 
  geom_histogram(binwidth = 5) + 
  labs(x="Age (years)", y = "Frequency") +
  scale_colour_discrete(name = "Vital Status")

d.t1 + geom_histogram(binwidth = 180) # 300
d.t2 + geom_histogram(binwidth = 25) # 100
d.t3 + geom_histogram(binwidth = 25) #100
d.age + geom_histogram(binwidth = 5)

skewness(a.dat$t1)
skewness(a.dat$t2)
skewness(a.dat$t3)
kurtosis(a.dat$t1)
kurtosis(a.dat$t2)
kurtosis(a.dat$t3)

summary(d.dat$t1)
summary(d.dat$t2)
summary(d.dat$t3)
sd(d.dat$t1)
sd(d.dat$t2)
sd(d.dat$t3)
skewness(d.dat$t1)
skewness(d.dat$t2)
skewness(d.dat$t3)
kurtosis(d.dat$t1)
kurtosis(d.dat$t2)
kurtosis(d.dat$t3)