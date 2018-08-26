# Basic demographics -------------------------------------------------------------------------

### Count of cancer type for all and dead pID ++++++++++++++++++++++++++++++++++++++++++++++++
dat.id <- dat.id %>% mutate(mdt=replace(mdt, mdt=="genitourinary", "genito-urinary"))
dat.id$mdt <- factor(dat.id$mdt)
table(dat.id$mdt, useNA = "always")

tab.mdt.dead <- dat.id %>% filter(vstat=="Dead") %>% count(mdt)
tab.mdt.all <- dat.id %>% count(mdt)

### Age, sex, marital, ethnic, vstat ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

dat.id %>% count(vstat) # Now to remove non Dead / Alive vstats
dat1 <- dat.id[ !(dat.id$vstat %in% c("Moved to England or Wales", "Immortal/not known")), ]
dat1$vstat <- factor(dat1$vstat)
dat.id <- dat1
rm(dat1)

datm %>% count(vstat) # Now to remove non Dead / Alive vstats
dat1 <- datm[ !(datm$vstat %in% c("Moved to England or Wales", "Immortal/not known")), ]
dat1$vstat <- factor(dat1$vstat)
datm <- dat1
rm(dat1)

# Collapsing missing values together

# For only id's
dat1 <- dat.id

levels(dat1$marital)[4] <- NA
levels(dat1$ethnic) <- c("African", "Other", "Asian", "Other", "White", "Asian",
                         "Asian", "Not known", "Other", "Asian", "Not known", "Not known",
                         "White", "White", "White", "White")
levels(dat1$ethnic)[5] <- NA
levels(dat1$obj)[3] <- NA

table(dat1$urb_rur, useNA = "ifany")
levels(dat1$urb_rur)

dat.id <- dat1

# For all
dat1 <- dat0

levels(dat1$marital)[4] <- NA
levels(dat1$ethnic) <- c("African", "Other", "Asian", "Other", "White", "Asian",
                         "Asian", "Not known", "Other", "Asian", "Not known", "Not known",
                         "White", "White", "White", "White")
levels(dat1$ethnic)[5] <- NA
levels(dat1$obj)[3] <- NA

datm <- dat1

tab.dem.vstat <- dat.id %>% count(vstat)
tab.dem.sex <- dat.id %>% count(sex)
tab.dem.marital <- dat.id %>% count(marital)
tab.dem.ethnic <- dat.id %>% count(ethnic)
tab.dem.obj <- dat.id %>% count(obj)
tab.dem.urbrur <- dat.id %>% count(urb_rur)
ggplot(dat.id, aes(x=age)) + geom_histogram(binwidth = 3, fill="cadetblue1", color="dimgray")
ggplot(dat.id, aes(vstat, age)) + geom_boxplot()


# %%% Big 5 cancers -----------------------------------------------------------------------------

datm.id <- dat.id[ (dat.id$mdt %in% c("breast", "lung", "genito-urinary", "gynae", "lower GI")), ]
datm.id$mdt <- mapvalues(datm.id$mdt, from=c("breast","genito-urinary","gynae","lower GI","lung"),
                         to=c("Breast", "Genito-urinary", "Gynaecological", "Lower GI", "Lung"))
datm.id$mdt <- factor(datm.id$mdt)

dat1 <- datm[ (datm$mdt %in% c("breast", "lung", "genito-urinary", "gynae", "lower GI")), ]
dat1$mdt <- mapvalues(dat1$mdt, from=c("breast","genito-urinary","gynae","lower GI","lung"),
                      to=c("Breast", "Genito-urinary", "Gynaecological", "Lower GI", "Lung"))
datm <- dat1
rm(dat1)

# Splitting into dead and alive

tab2 <- datm.id %>% spread(vstat, obj)
tab2 <- tab2 %>% select(mdt:Dead)
tab.obj.d <- as.data.frame.matrix(table(tab2$Dead, tab2$mdt, useNA = "ifany"))
tab.obj.a <- as.data.frame.matrix(table(tab2$Alive, tab2$mdt, useNA = "ifany"))
write.csv(tab.obj.d, "tab_objd.csv")
write.csv(tab.obj.a, "tab_obja.csv")

tab2 <- datm.id %>% spread(vstat, sex)
tab2 <- tab2 %>% select(mdt:Dead)
tab.sex.d <- as.data.frame.matrix(table(tab2$Dead, tab2$mdt, useNA = "ifany"))
tab.sex.a <- as.data.frame.matrix(table(tab2$Alive, tab2$mdt, useNA = "ifany"))
write.csv(tab.sex.d, "tab_sexd.csv")
write.csv(tab.sex.a, "tab_sexa.csv")

tab2 <- datm.id %>% spread(vstat, marital)
tab2 <- tab2 %>% select(mdt:Dead)
tab.marital.d <- as.data.frame.matrix(table(tab2$Dead, tab2$mdt, useNA = "ifany"))
tab.marital.a <- as.data.frame.matrix(table(tab2$Alive, tab2$mdt, useNA = "ifany"))
write.csv(tab.marital.d, "tab_maritald.csv")
write.csv(tab.marital.a, "tab_maritala.csv")

tab2 <- datm.id %>% spread(vstat, ethnic)
tab2 <- tab2 %>% select(mdt:Dead)
tab.ethnic.d <- as.data.frame.matrix(table(tab2$Dead, tab2$mdt, useNA = "ifany"))
tab.ethnic.a <- as.data.frame.matrix(table(tab2$Alive, tab2$mdt, useNA = "ifany"))
write.csv(tab.ethnic.d, "tab_ethnicd.csv")
write.csv(tab.ethnic.a, "tab_ethnica.csv")

tab2 <- datm.id %>% spread(vstat, urb_rur)
tab2 <- tab2 %>% select(mdt:Dead)
tab.urb_rur.d <- as.data.frame.matrix(table(tab2$Dead, tab2$mdt, useNA = "ifany"))
tab.urb_rur.a <- as.data.frame.matrix(table(tab2$Alive, tab2$mdt, useNA = "ifany"))
write.csv(tab.urb_rur.d, "tab_urbrurd.csv")
write.csv(tab.urb_rur.a, "tab_urbrura.csv")

# %%% HADS Scores ------------------------------------------------------------------------------

### Tabulating HADS score frequencies by pID +++++++++++++++++++++++++++++++++++++++++++++++++++
require(dplyr)
detach(package:plyr)

# Need to ensure plyr is removed and dplyr should be loaded last
dat2 <- dat1 %>% group_by(pid) %>% summarise(hads_count = sum(!is.na(hads)))
dat3 <- left_join(dat1.id, dat2)
tab.hads.count <- as.data.frame(table(dat3$hads_count)) # For the big 5 cancers only

### Extracting HADS score for each patient +++++++++++++++++++++++++++++++++++++++++++++++++++++
dat.hads <- left_join(dat1, dat2)
dat.tmp <- dat.hads

dat.tmp <- dat.tmp %>% group_by(pid) %>% mutate(first_hads = first(hads))
dat.tmp <- dat.tmp %>% group_by(pid) %>% mutate(penul_hads = nth(hads, -2))
dat.tmp <- dat.tmp %>% group_by(pid) %>% 
  mutate(penul_hads = ifelse(hads_count<3, NA, penul_hads)) # If HADS count < 3, should be NA
dat.tmp <- dat.tmp %>% group_by(pid) %>% mutate(last_hads = last(hads))

table(dat.pid.hads$first_hads, useNA = "ifany") # 775 NA's
table(dat.pid.hads$penul_hads, useNA = "ifany") # 10,290 NA's
table(dat.pid.hads$last_hads, useNA = "ifany") # 899 NA's

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

# %%% Times between HADS scores ------------------------------------------------------------------

# Days between date of inclusion and date of screening 
sum(is.na(dat1$dt_appt)) # 10,345 NA's
sum(is.na(dat1$dt)) # 1,821 NA's
dat1$time <- dat1$dt - dat1$dt_inc # Gives this in date difference formate
dat1$time <- as.numeric(dat1$time) # Converts to numeric format
summary(dat1$time)

### Extracting HADS dates for each patient +++++++++++++++++++++++++++++++++++++++++++++++++++++++
dat.tmp <- dat.tmp %>% group_by(pid) %>% mutate(first_time = first(dt))
dat.tmp <- dat.tmp %>% group_by(pid) %>% mutate(penul_time = nth(dt, -2))
dat.tmp <- dat.tmp %>% group_by(pid) %>% mutate(last_time = last(dt))

# Doing calculations of HADS times between each
dat.tmp$t1 <- dat.tmp$first_time - dat.tmp$dt_inc
dat.tmp$t2 <- dat.tmp$penul_time - dat.tmp$first_time
dat.tmp$t3 <- dat.tmp$last_time - dat.tmp$penul_time
dat.tmp$t4 <- dat.tmp$last_time - dat.tmp$first_time

### Save HADS only dataset +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

dat1.hads <- dat.tmp

# For per-pID variables, create new patient-only dataset for efficiency
dat.tmp1 <- dat.tmp %>% select(pid, hads_count:last_time)
dat.tmp <- distinct(dat.tmp1)
dat1.pid.hads <- dat.tmp

# %%% Saving it all ------------------------------------------------------------------------------
save.image(file="0731formarkdown.rda")
rm(list=ls(pattern = "tmp"))
rm(list=ls(pattern = "tab"))
rm(list=ls(pattern = ".z"))
rm(list=ls(pattern = "dat1"))
save.image(file="0812analysis.rda")

require(readr)
write.csv(tab.dem.ethnic, file="tab_ethnic.csv")
write.csv(tab.mdt.all, file="tab_mdtall.csv")
write.csv(tab.dem.marital, file="tab_marital.csv")
write.csv(tab.dem.obj, file="tab_obj.csv")
write.csv(tab.dem.sex, file="tab_sex.csv")
write.csv(tab.dem.urbrur, file="tab_urbrur.csv")
write.csv(tab.dem.vstat, file="tab_vstat.csv")
write.csv(tabm.hads, file="tab_mhads.csv")

##### Missing HADS scores #####

# Discover what the distribution of missing values is 

table(datm$hads, useNA = 'ifany')
# 1821 missing HADS scores.

# Subset observations that does not have null HADS scores as datm
datm.na <- datm %>% filter(is.na(hads))
dat1 <- datm %>% filter(!is.na(hads))
datm <- dat1
rm(dat1)

# Count no of cases and controls again
datm.death <- datm.id %>% filter(is.na(dt_death))
# 14,983 out of 21,302 people are still alive at the end of the study
rm(datm.death)

dat2 <- datm %>% distinct(pid, .keep_all=T) %>% 
  select(pid:mdt)
# 20,913 ID's from 21,302. 389 removed. Count table of HADS scores again and see the frequencies.

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

# This plots by descending order
ggplot(datm, aes(rank, hads)) +
  geom_boxplot(aes(group=rank))

# %%% Times between HADS scores ------------------------------------------------------------------
summary(datm$dt_inc)
summary(datm$dt_appt)
summary(datm$dt)
dat1 <- datm

### Extracting HADS dates for each patient +++++++++++++++++++++++++++++++++++++++++++++++++++++++
dat1 <- dat1 %>% group_by(pid) %>% mutate(first_time = first(dt))
dat1 <- dat1 %>% group_by(pid) %>% mutate(penul_time = nth(dt, -2))
dat1 <- dat1 %>% group_by(pid) %>% mutate(last_time = last(dt))

# Doing calculations of HADS times between each
dat1$t1 <- as.numeric(dat1$first_time - dat1$dt_inc)
dat1$t2 <- as.numeric(dat1$penul_time - dat1$first_time)
dat1$t3 <- as.numeric(dat1$last_time - dat1$penul_time)
dat1$t4 <- as.numeric(dat1$last_time - dat1$first_time)

summary(dat1$t1)
summary(dat1$t2)
summary(dat1$t3)
summary(dat1$t4)

datm <- dat1

# Last HADS: rank == 1
# Penultimate HADS: rank == 2

dat1 <- datm %>% filter(t3<0)
# Need to move these observations around (penul and last obs for these two IDs)

# Create new factor
datm$Overall <- "Overall"
ggplot(datm) + geom_boxplot(aes(x=Overall, y=age)) + geom_boxplot(aes(x=vstat, y=age)) + labs(x=" ")
datm <- datm[!datm$Overall]

