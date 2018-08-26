### Cleaning for Stata #####

d.dat <- dat %>% filter(vstat=='Dead') %>%  
  select_('pid', 'vstat', 'obj', 'mdt', 'age', 'sex', 'urbrur',
          'starts_with("dt")', 'hads:t3')
d.dat$dt_alive <- d.dat$dt_death

a.dat <- dat %>% select_('pid', 'vstat', 'obj', 'mdt', 'age', 'sex', 'urbrur',
                            'starts_with("dt")', 'hads:t3')
a.dat$dt_alive <- with(dat, dt_death-1)
a.dat$dt_alive[is.na(a.dat$dt_death)] <- ymd("2012-04-03")

a.dat$t1 <- as.numeric(a.dat$dt_alive - a.dat$dt_inc)
a.dat$t2 <- as.numeric(a.dat$dt_alive - a.dat$last_time)
a.dat$t3 <- as.numeric(a.dat$last_time - a.dat$penul_time)
# I can also set t3 to be the time between any two HADS observations. How do I do this?

weird <- a.dat %>% filter(t2<0)
a.dat <- anti_join(a.dat, weird, by=c("pid", "dt"))

write.csv(a.dat, "./output_data/ctrldat.csv")
write.csv(d.dat, "./output_data/casedat.csv")

rm(weird)

# Vis
summary(a.dat$t1)
ggplot(a.dat, aes(x=vstat, y=t2)) + geom_boxplot()
ggplot(a.dat, aes(x=vstat, y=t1)) + geom_boxplot()
ggplot(a.dat, aes(x=vstat, y=t3)) + geom_boxplot()

# Times
dat1$t1 <- as.numeric(dat1$dt_death - dat1$dt_inc)
dat1$t2 <- as.numeric(dat1$dt_death - dat1$last_time)
dat1$t3 <- as.numeric(dat1$last_time - dat1$penul_time)
a.dat$t1 <- as.numeric(a.dat$dt_alive - a.dat$dt_inc)
a.dat$t2 <- as.numeric(a.dat$dt_alive - a.dat$last_time)
a.dat$t3 <- as.numeric(a.dat$last_time - a.dat$penul_time)
 
# Finding group nos for each MDT
n <- ctrldat %>% filter(mdt=="Breast") %>% count()
n <- ctrldat %>% filter(mdt=="Genito-urinary") %>% count()
n <- ctrldat %>% filter(mdt=="Gynaecological") %>% count()
n <- ctrldat %>% filter(mdt=="Lower GI") %>% count()
n <- ctrldat %>% filter(mdt=="Lung") %>% count()
rm(n)