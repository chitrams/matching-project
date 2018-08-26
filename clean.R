# Basic R modification ------
rawdat <- read_csv("./rmd/0726dat_stata.csv", na =c("", "n/a"))

raw.dat <- rawdat %>% dplyr::rename(
  pid = patientlinkid,
  dt_death = cr_dodth,
  age = age_at_inc,
  vstat = vital_status,
  obj = therapy_obj,
  urbrur = urbrur,
  dt_appt = appointmentdate,
  s_stat = screeningstatus,
  dt = date,
  scid_stat = scidescalationstatus,
  dt_inc = inc_date
)

rm(rawdat)
dat <- raw.dat

# Change to dates
dat1 <- dat
for (i in c("dt_death", "dt", "dt_appt", "dt_inc")) {
  dat1[[i]] <- as.Date(dat1[[i]], "%m/%d/%Y")
}

dat1 <- arrange(dat1, pid, dt)

# Change to factors
for (i in c("sex", "marital", "ethnic", "vstat", "obj", "urbrur", "mdt", "s_stat", 
            "excludedreason", "refusedreason", "scid_stat", "scidprogress")) {
  dat1[[i]] <- as.factor(dat1[[i]])
}

rm(i)

# Clean mdt
dat1 <- dat1 %>% mutate(mdt=replace(mdt, mdt=="genitourinary", "genito-urinary"))
dat1$mdt <- factor(dat1$mdt)

#%%% Collapsing missing values ------------------------------------------------------------

levels(dat1$marital)[4] <- NA
levels(dat1$ethnic) <- c("African", "Other", "Asian", "Other", "White", "Asian",
                         "Asian", "Not known", "Other", "Asian", "Not known", "Not known",
                         "White", "White", "White", "White")
levels(dat1$ethnic)[5] <- NA
levels(dat1$obj)[3] <- NA
dat <- dat1

#%%% Subsetting patients  ------------------------------------------------------------

dat1 <- dat # start subset

# vstat, 29
dat1 <- dat1[ !(dat1$vstat %in% c("Moved to England or Wales", "Immortal/not known")), ]
dat1$vstat <- factor(dat1$vstat)

# Cancer, 5889 removed
dat1 <- dat1[ (dat1$mdt %in% c("breast", "lung", "genito-urinary", "gynae", "lower GI")), ]
dat1$mdt <- mapvalues(dat1$mdt, from=c("breast","genito-urinary","gynae","lower GI","lung"),
                         to=c("Breast", "Genito-urinary", "Gynaecological", "Lower GI", "Lung"))
dat1$mdt <- factor(dat1$mdt) 

# Data of accepted patients at screening only
dat1 <- dat1 %>% filter((s_stat=="Manual E") | (s_stat=="Accepted")) %>% 
  select(-(excludedreason:refusedreason)) # 40553 Excluded (35,196), Pending (186), Refused (5171) removed
dat1$s_stat <- factor(dat1$s_stat)

# Subset observations that does not have null HADS scores
dat1 <- dat1 %>% filter(!is.na(hads))

# Saving up to this bit, 48,340 obs removed in total - adds up
dat <- dat1
rm(dat1)

# Clean weird obs -----------------

# Find instances of death < appt
er1 <- dat %>% filter(dt_death<dt) %>% select(pid, dt_death, dt) 
# 8 obs of 8, all 8 should be removed
dat1 <- dat
dat2 <- anti_join(dat1, er1, by=c("pid", "dt_death", "dt"))
dat1 <- dat2

# Find instances of penul==last for times
dat1 <- dat1 %>% group_by(pid) %>% mutate(penul_time = nth(dt, -2))
dat1 <- dat1 %>% group_by(pid) %>% mutate(last_time = last(dt))
er2 <- dat1 %>% filter(penul_time==last_time) %>% 
  select(pid, hads, penul_time, last_time) 
# 12 obs of 6, 2 + 2 + 4 should be removed

# Clean weird bits
dat1 <- dat1 %>% subset(pid != 10005641)
dat1 <- dat1 %>% subset(pid != 10023385)
dat2 <- dat1 %>% distinct()

# Redo as these would have changed
dat1 <- select_(dat2, 'pid:dep')
dat <- dat1
rm(dat1, dat2, list=ls(pattern="er"))

# Adding HADS values -----
dat1 <- dat

#%%% HADS scores per pID ------
detach(package:dplyr)
detach(package:plyr)
require(plyr)
require(dplyr)

dat1 <- dat1 %>% group_by(pid) %>% mutate(hads_count = sum(!is.na(hads)))
dat1 <- dat1 %>% group_by(pid) %>% mutate(penul_hads = nth(hads, -2))
dat1 <- dat1 %>% group_by(pid) %>% mutate(last_hads = last(hads))
dat1 <- dat1 %>% group_by(pid) %>% 
  mutate(penul_hads = ifelse(hads_count<2, NA, penul_hads)) # Penul HADS can == final HADS

# Changes in HADS scores
dat1$hads_change <- dat1$last_hads - dat1$penul_hads

#%%% HADS durations between -----

# Extract HADS dates for each patient
dat1 <- dat1 %>% group_by(pid) %>% mutate(penul_time = nth(dt, -2))
dat1 <- dat1 %>% group_by(pid) %>% mutate(last_time = last(dt))

# Doing calculations of HADS times between each, saved as no of dats
dat1$t1 <- as.numeric(dat1$dt_death - dat1$dt_inc)
dat1$t2 <- as.numeric(dat1$dt_death - dat1$last_time)
dat1$t3 <- as.numeric(dat1$last_time - dat1$penul_time)
dat <- dat1

rm(dat1)

# Saving for matching -----

#%%% Create pID only ------
dat.id <- dat %>% distinct(pid, .keep_all=T) %>% 
  select(pid:mdt, dep:t3)

saveRDS(dat, file="./output_data/dat.rds")
saveRDS(dat.id, file="./output_data/datid.rds")
saveRDS(raw.dat, file="./output_data/rawdat.rds")
saveRDS(ctrldat, file="./output_data/ctrldat.rds")

write.csv(dat, "./output_data/dat.csv")
write.csv(dat.id, "./output_data/datid.csv")
write.csv(raw.dat, "./output_data/rawdat.csv")