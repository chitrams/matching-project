# Create long format data for plots

ggdat.wide <- subset(dat.id, select=c(pid, vstat, obj, penul_hads:hads_change, t1:t3))

ggdat.long <- gather(ggdat.wide, hads, score, penul_hads:last_hads, factor_key = T)
ggdat.long <- gather(ggdat.long, period, duration, t1:t3, factor_key = T)

ggdat.long$HADScode <- revalue(ggdat.long$hads, c("penul_hads"="Penultimate Score", "last_hads"="Last Score"))
ggdat.long$vstat <- revalue(ggdat.long$vstat, c("Dead"="Died"))

ggplot(ggdat.long, aes(x=HADScode, y=score, color=vstat)) + 
  geom_boxplot(fill=NA) + coord_cartesian(ylim=c(0,42)) +
  labs(color = "Vital status", x = "HADS observation", y = "HADS score") +
  scale_color_manual(values = c("deepskyblue", "tomato"))
  
ggdat.wide$y1 <- with(ggdat.wide, t1/365)
ggdat.wide$y2 <- with(ggdat.wide, t2/365)
ggdat.wide$y3 <- with(ggdat.wide, t3/365)

ggplot(ggdat.wide, aes(y1)) + geom_histogram(binwidth = 1/12) +
  labs(x="Time (years)", y = "Frequency")
ggplot(ggdat.wide, aes(t2)) + geom_histogram(binwidth = 10) +
  labs(x="Days between last HADS observation and death", y = "Frequency")
ggplot(ggdat.wide, aes(t3)) + geom_histogram(binwidth = 10) +
  labs(x="Days between penultimate HADS observation and death", y = "Frequency")

ggplot(ggdat.wide, aes(y2)) + geom_histogram(binwidth = 1/12) +
  labs(x="Time (years)", y = "Frequency")
ggplot(ggdat.wide, aes(y3)) + geom_histogram(binwidth = 1/12) +
  labs(x="Time (years)", y = "Frequency") +
  coord_cartesian(xlim=c(0,3))

gg.case <- ggdat.wide %>% filter(vstat=="Dead")
ggplot(gg.case, aes(y3)) + geom_histogram(binwidth = 1/12) +
  labs(x="Time (years)", y = "Frequency") +
  coord_cartesian(xlim=c(0,3))

# Table summaries 

summary(ggdat.wide)
sapply(ggdat.wide, sd, na.rm = TRUE)

dat.summ <- dat.id
dat.summ <- subset(dat.summ, select=c(1, 7, 2, 8, 12:20))
dat.summ$y1 <- with(dat.summ, t1/365)
dat.summ$y2 <- with(dat.summ, t2/365)
dat.summ$y3 <- with(dat.summ, t3/365)

summ.a <- dat.summ %>% filter(vstat=="Alive")
sum.d <- dat.summ %>% filter(vstat=="Dead")

require(fBasics)

summary(summ.a)
sapply(summ.a, sd, na.rm = TRUE)
sapply(summ.a, skewness, na.rm = TRUE)
sapply(summ.a, kurtosis, na.rm = TRUE)

summary(sum.d)
sapply(sum.d, sd, na.rm = TRUE)
sapply(sum.d, skewness, na.rm = TRUE)
sapply(sum.d, kurtosis, na.rm = TRUE)

detach("package:fBasics")
detach("package:timeSeries")
detach("package:timeDate")
detach("package:stats")
detach("package:dplyr")
require(stats)
require(dplyr)

# Qs to ask Chris #####

summary(ggdat.wide)
t2 <- subset(ggdat.wide, t2<0)

t1 <- subset(dat1, t1<=0)
t3 <- subset(dat1, t3<=0)
t2 <- subset(dat1, t2<=0)

t2 <- t2[c("pid","dt_death","dt_inc","dt_appt","dt", "hads_count", "t2")]
t3 <- t3[c(1,12,14:26)]

t3 <- arrange(t3, pid, dt)

colSums(!is.na(dat))

# On scid conducted and depression diagnosis

table(dat$dep, dat$vstat, useNA = "ifany")
table(dat$vstat, dat$scid_stat, useNA = "ifany") # If they completed interview

dep0 <- dat %>% filter(dep==0)

dep1 <- dat %>% filter(dep==1)
dep1$scidprogress <- factor(dep1$scidprogress)
table(dep1$scidprogress)
table(dep1$vstat, dep1$scid_stat)

dep2 <- dat %>% filter(dep==2)
dep2$scidprogress <- factor(dep2$scidprogress)
table(dep2$scidprogress)
table(dep2$vstat, dep2$scid_stat)

# 1 diagnosed as major depression
# 2 conducted SCID but was not diagnosed as major depression due to reasons

table(dat$vstat)

table(dat$dep, dat$scid_stat)
table(dat$scid_stat)

exc <- raw.dat %>% filter(mdt=="breast" | mdt=="genito-urinary" | mdt=="gynae" | mdt=="lung" | mdt=="lower GI")
exc <- raw.dat %>% filter(vstat)