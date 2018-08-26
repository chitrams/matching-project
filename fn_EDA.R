# Basic demographics -------------------------------------------------------------------------

tab.mdt.dead <- datm.id %>% filter(vstat=="Dead") %>% count(mdt)
tab.mdt.all <- dat.id %>% count(mdt)

### Age, sex, marital, ethnic, vstat ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

detach(package:dplyr)
tab.dem.vstat <- dat.id %>% count(vstat)
tab.dem.mdt <- dat.id %>% count(mdt)
tab.dem.mdt <- dat.id %>% group_by(vstat) %>% count(mdt)
tab.dem.sex <- dat.id %>% count(sex)
tab.dem.marital <- dat.id %>% count(marital)
tab.dem.ethnic <- dat.id %>% count(ethnic)
tab.dem.obj <- dat.id %>% count(obj)
tab.dem.urbrur <- dat.id %>% count(urb_rur)
ggplot(dat.id, aes(x=age)) + geom_histogram(binwidth = 3, fill="cadetblue1", color="dimgray")
ggplot(dat.id, aes(vstat, age)) + geom_boxplot()


# %%% Big 5 cancers -----------------------------------------------------------------------------

# Splitting into dead and alive for table

tab2 <- dat.id %>% spread(vstat, obj)
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

# Need to ensure plyr is removed and dplyr should be loaded last
require(dplyr)
detach(package:plyr)
dat1 <- dat %>% group_by(pid) %>% summarise(hads_count = sum(!is.na(hads)))
dat2 <- left_join(dat.id, dat1)
tab.hads.count <- as.data.frame(table(dat2$hads_count)) # For the big 5 cancers only
write_csv(tab.hads.count, "tabhads.csv")

### Extracting HADS score for each patient +++++++++++++++++++++++++++++++++++++++++++++++++++++
dat1 <- dat

dat1 <- dat1 %>% group_by(pid) %>% mutate(hads_count = sum(!is.na(hads)))
dat1 <- dat1 %>% group_by(pid) %>% mutate(penul_hads = nth(hads, -2))
dat1 <- dat1 %>% group_by(pid) %>% 
  mutate(penul_hads = ifelse(hads_count<3, NA, penul_hads)) # If HADS count < 3, should be NA
dat1 <- dat1 %>% group_by(pid) %>% mutate(last_hads = last(hads))

dat <- dat1

# %%% Times between HADS scores ------------------------------------------------------------------

# Days between date of inclusion and date of screening 
sum(is.na(dat1$dt_appt)) # 9935 NA's
sum(is.na(dat1$dt)) # 0 NA's
dat1$time <- dat1$dt - dat1$dt_inc # Gives this in date difference format
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

# Count no of cases and controls again
datm.death <- datm.id %>% filter(is.na(dt_death))
# 14,983 out of 21,302 people are still alive at the end of the study
rm(datm.death)

dat2 <- datm %>% distinct(pid, .keep_all=T) %>% 
  select(pid:mdt)
# 20,913 ID's from 21,302. 389 removed. Count table of HADS scores again and see the frequencies.

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

