# Subset by cancer type, then reiterate
# Breast Genito-urinary Gynaecological Lower GI Lung 

# Breast ------
tabdat <- dat.id %>% filter(mdt=="Breast")

# Create variables and tables
tabsex <- tabdat %>% group_by(vstat) %>% count(sex)
tabmarital <- tabdat %>% group_by(vstat) %>% count(marital)
tabethnic <- tabdat %>% group_by(vstat) %>% count(ethnic)
tabobj <- tabdat %>% group_by(vstat) %>% count(obj)
taburbrur <- tabdat %>% group_by(vstat) %>% count(urbrur)

age.mean <- tabdat %>% group_by(vstat) %>% summarise(age.mean=mean(age))
age.sd <- tabdat %>% group_by(vstat) %>% summarise(age.sd=sd(age))
age.min <- tabdat %>% group_by(vstat) %>% summarise(age.min=min(age))
age.max <- tabdat %>% group_by(vstat) %>% summarise(age.max=max(age))

tabage <- left_join(age.mean, age.sd, by="vstat")
tabage <- left_join(tabage, age.min, by="vstat")
tabage <- left_join(tabage, age.max, by="vstat")

# Save as CSV
write.csv(tabsex, "./tab/breast/tabsex.csv")
write.csv(tabmarital, "./tab/breast/tabmarital.csv")
write.csv(tabethnic, "./tab/breast/tabethnic.csv")
write.csv(tabobj, "./tab/breast/tabobj.csv")
write.csv(taburbrur, "./tab/breast/taburbrur.csv")
write.csv(tabage, "./tab/breast/tabage")

# Genito-urinary ------
tabdat <- dat.id %>% filter(mdt=="Genito-urinary")

# Create variables and tables
tabsex <- tabdat %>% group_by(vstat) %>% count(sex)
tabmarital <- tabdat %>% group_by(vstat) %>% count(marital)
tabethnic <- tabdat %>% group_by(vstat) %>% count(ethnic)
tabobj <- tabdat %>% group_by(vstat) %>% count(obj)
taburbrur <- tabdat %>% group_by(vstat) %>% count(urbrur)

age.mean <- tabdat %>% group_by(vstat) %>% summarise(age.mean=mean(age))
age.sd <- tabdat %>% group_by(vstat) %>% summarise(age.sd=sd(age))
age.min <- tabdat %>% group_by(vstat) %>% summarise(age.min=min(age))
age.max <- tabdat %>% group_by(vstat) %>% summarise(age.max=max(age))

tabage <- left_join(age.mean, age.sd, by="vstat")
tabage <- left_join(tabage, age.min, by="vstat")
tabage <- left_join(tabage, age.max, by="vstat")

# Save as CSV
write.csv(tabsex, "./tab/genitourinary/tabsex.csv")
write.csv(tabmarital, "./tab/genitourinary/tabmarital.csv")
write.csv(tabethnic, "./tab/genitourinary/tabethnic.csv")
write.csv(tabobj, "./tab/genitourinary/tabobj.csv")
write.csv(taburbrur, "./tab/genitourinary/taburbrur.csv")
write.csv(tabage, "./tab/genitourinary/tabage")

# Gynaecological -----
tabdat <- dat.id %>% filter(mdt=="Gynaecological")

# Create variables and tables
tabsex <- tabdat %>% group_by(vstat) %>% count(sex)
tabmarital <- tabdat %>% group_by(vstat) %>% count(marital)
tabethnic <- tabdat %>% group_by(vstat) %>% count(ethnic)
tabobj <- tabdat %>% group_by(vstat) %>% count(obj)
taburbrur <- tabdat %>% group_by(vstat) %>% count(urbrur)

age.mean <- tabdat %>% group_by(vstat) %>% summarise(age.mean=mean(age))
age.sd <- tabdat %>% group_by(vstat) %>% summarise(age.sd=sd(age))
age.min <- tabdat %>% group_by(vstat) %>% summarise(age.min=min(age))
age.max <- tabdat %>% group_by(vstat) %>% summarise(age.max=max(age))

tabage <- left_join(age.mean, age.sd, by="vstat")
tabage <- left_join(tabage, age.min, by="vstat")
tabage <- left_join(tabage, age.max, by="vstat")

# Save as CSV
write.csv(tabsex, "./tab/gynae/tabsex.csv")
write.csv(tabmarital, "./tab/gynae/tabmarital.csv")
write.csv(tabethnic, "./tab/gynae/tabethnic.csv")
write.csv(tabobj, "./tab/gynae/tabobj.csv")
write.csv(taburbrur, "./tab/gynae/taburbrur.csv")
write.csv(tabage, "./tab/gynae/tabage")

# Lower GI -----
tabdat <- dat.id %>% filter(mdt=="Lower GI")

# Create variables and tables
tabsex <- tabdat %>% group_by(vstat) %>% count(sex)
tabmarital <- tabdat %>% group_by(vstat) %>% count(marital)
tabethnic <- tabdat %>% group_by(vstat) %>% count(ethnic)
tabobj <- tabdat %>% group_by(vstat) %>% count(obj)
taburbrur <- tabdat %>% group_by(vstat) %>% count(urbrur)

age.mean <- tabdat %>% group_by(vstat) %>% summarise(age.mean=mean(age))
age.sd <- tabdat %>% group_by(vstat) %>% summarise(age.sd=sd(age))
age.min <- tabdat %>% group_by(vstat) %>% summarise(age.min=min(age))
age.max <- tabdat %>% group_by(vstat) %>% summarise(age.max=max(age))

tabage <- left_join(age.mean, age.sd, by="vstat")
tabage <- left_join(tabage, age.min, by="vstat")
tabage <- left_join(tabage, age.max, by="vstat")

# Save as CSV
write.csv(tabsex, "./tab/lowergi/tabsex.csv")
write.csv(tabmarital, "./tab/lowergi/tabmarital.csv")
write.csv(tabethnic, "./tab/lowergi/tabethnic.csv")
write.csv(tabobj, "./tab/lowergi/tabobj.csv")
write.csv(taburbrur, "./tab/lowergi/taburbrur.csv")
write.csv(tabage, "./tab/lowergi/tabage")

# Lung -----
tabdat <- dat.id %>% filter(mdt=="Lung")

# Create variables and tables
tabsex <- tabdat %>% group_by(vstat) %>% count(sex)
tabmarital <- tabdat %>% group_by(vstat) %>% count(marital)
tabethnic <- tabdat %>% group_by(vstat) %>% count(ethnic)
tabobj <- tabdat %>% group_by(vstat) %>% count(obj)
taburbrur <- tabdat %>% group_by(vstat) %>% count(urbrur)

age.mean <- tabdat %>% group_by(vstat) %>% summarise(age.mean=mean(age))
age.sd <- tabdat %>% group_by(vstat) %>% summarise(age.sd=sd(age))
age.min <- tabdat %>% group_by(vstat) %>% summarise(age.min=min(age))
age.max <- tabdat %>% group_by(vstat) %>% summarise(age.max=max(age))

tabage <- left_join(age.mean, age.sd, by="vstat")
tabage <- left_join(tabage, age.min, by="vstat")
tabage <- left_join(tabage, age.max, by="vstat")

# Save as CSV
write.csv(tabsex, "./tab/lung/tabsex.csv")
write.csv(tabmarital, "./tab/lung/tabmarital.csv")
write.csv(tabethnic, "./tab/lung/tabethnic.csv")
write.csv(tabobj, "./tab/lung/tabobj.csv")
write.csv(taburbrur, "./tab/lung/taburbrur.csv")
write.csv(tabage, "./tab/lung/tabage")

# Clean -----
rm(list=ls(pattern = "tab"))
rm(list=ls(pattern = "age."))
