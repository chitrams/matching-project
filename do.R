### HADS desc plots big 5 ---------------------------------------------------------------------

# Distribution of HADS Scores
ggplot(dat.pid.hads, aes(x=first_hads)) + 
  geom_histogram(binwidth=1, color="grey", fill="lightpink") +
  geom_vline(xintercept=15)
ggplot(dat.pid.hads, aes(x=penul_hads)) + 
  geom_histogram(binwidth=1, color="grey", fill="lightpink") +
  geom_vline(xintercept=15)
ggplot(dat.pid.hads, aes(x=last_hads)) + 
  geom_histogram(binwidth=1, color="grey", fill="lightpink") +
  geom_vline(xintercept=15)

# Distribution of HADS Scores on the same scale
ggplot(dat.pid.hads, aes(x=first_hads)) + 
  geom_histogram(binwidth=1, color="grey", fill="lightpink") +
  geom_vline(xintercept=15) +
  coord_cartesian(ylim=c(0, 1300))
ggplot(dat.pid.hads, aes(x=penul_hads)) + 
  geom_histogram(binwidth=1, color="grey", fill="lightpink") +
  geom_vline(xintercept=15) +
  coord_cartesian(ylim=c(0, 1300))
ggplot(dat.pid.hads, aes(x=last_hads)) + 
  geom_histogram(binwidth=1, color="grey", fill="lightpink") +
  geom_vline(xintercept=15) +
  coord_cartesian(ylim=c(0, 1300))

ggplot(datm, aes(rank, hads)) +
  geom_boxplot(aes(group=rank))

# Reasons for non-acceptance
summary(raw.dat$excludedreason)
summary(raw.dat$refusedreason)


# HADS scores --------------------------------------------------------------------------------
# Tabulating HADS score frequencies
require(dplyr)
detach(package:plyr)
dat1 <- dat0
dat2 <- dat1 %>% group_by(pid) %>% summarise(hads_count = sum(!is.na(hads)))
dat3 <- left_join(datm.id, dat2)
tabm.hads <- as.data.frame(table(dat3$hads_count)) # For the big 5 cancers only
write.csv(tabm.hads, file="tabm_hads.csv")

# Create graph of HADS scores
# Rank in descending order, titled rank
dat1 <- datm %>% group_by(pid) %>% mutate(rank=rank(pid, ties.method="last"))
datm <- dat1

table(dat$first_hads, useNA = "ifany") 
table(dat$penul_hads, useNA = "ifany") # 20,747
table(dat$last_hads, useNA = "ifany")

# Weird shit #####
# Create table of frequency of these hads to use for ggplot.
# n_days vs first, penul, last

require(reshape)
dat.z <- subset(dat.pid.hads, select=-(c(pid:hads_count)))
dat.z.1 <- as.data.frame(table(dat.z$first_hads))
dat.z.2 <- as.data.frame(table(dat.z$penul_hads))
dat.z.3 <- as.data.frame(table(dat.z$last_hads))
dat.zz <- left_join(dat.z.1, dat.z.2, by="Var1")
dat.zz <- left_join(dat.zz, dat.z.3, by="Var1")

dat.zz <- rename(dat.zz, c(
  "Freq.x" = "n_firs_hads",
  "Freq.y" = "n_penu_hads",
  "Freq" = "n_last_hads"
))

dat.zz.long <- rename(dat.zz.long, c(
  "Var1" = "hads_scr_count",
  "variable" = "hads_type",
  "value" = "freq"
))

dat.zz.long <- melt(dat.zz)
dat.zz.long$hads_scr_count <- as.factor(dat.zz.long$hads_scr_count)
dat.zz.long$hads_scr_count <- factor(dat.zz.long$hads_scr_count, levels=sort_labs)
dat.zz.long$sort_labs <- paste(sort(as.integer(levels(dat.zz.long$hads_scr_count))))
dat.zz.long <- transform(dat.zz.long, hads_scr_count=reorder(hads_scr_count, sort_labs) ) 
ggplot(dat.zz.long, aes(hads_scr_count, freq)) + geom_bar(aes(fill=hads_type), stat="identity") +
  geom_vline(xintercept=15)
# 15+ considered depressed

tab.a <- dat.id %>% filter(vstat=="Alive")
tab.d <- dat.id %>% filter(vstat=="Dead")
 summary(tab.d$age)
 sd(tab.a$age)
 sd(tab.d$age)

 