# Non-acceptance of HADS -------------------------------------------------------
count(distinct(dat0, pid)) # 23,085, ie all ID

dat1 %>% distinct(pid) %>% count()

dat1 %>% filter(s_stat=="Excluded") %>% 
  distinct(pid) %>% 
  count() # 13,256

dat1 %>% filter(s_stat=="Refused") %>% 
  distinct(pid) %>% 
  count() # 13,256

dat %>% count(excludedreason) # Need to rename 'Completed SCUp'

raw.dat %>% filter(s_stat=="Excluded") %>% 
  count(refusedreason)
raw.dat %>% filter(s_stat=="Excluded") %>% 
  filter(refusedreason=="Patient present but does not") %>% 
  select(pid) # 10000800

dat0 %>% filter(s_stat=="Excluded") %>% 
  filter(!is.na(hads))
dat0 %>% filter(s_stat=="Refused") %>% 
  filter(!is.na(hads)) # 0
dat0 %>% filter(s_stat=="Manual E") %>% 
  filter(!is.na(hads)) # 0
dat0 %>% filter(s_stat=="Pending") %>% 
  filter(!is.na(hads))

dat1 <- dat0 %>% filter(s_stat=="Excluded") %>% 
  filter(!is.na(hads))
dat2 <- dat0 %>% filter(s_stat=="Pending") %>% 
  filter(!is.na(hads))
dat.inc <- bind_rows(dat.exc, dat.pend)

dat1 %>% filter(s_stat=="Refused") %>% 
  count(excludedreason)
dat1 %>% filter(s_stat=="Refused") %>% 
  filter(excludedreason=="Missed patient") %>% 
  select(pid) # 10019373

dat %>% filter(pid=="10019373") %>% 
  View()
dat %>% filter(pid=="10000800") %>% 
  View()
# In this instance: Double of each reason; inconsistency?

rm(dat1, dat2)