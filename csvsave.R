# Saving #####

write.csv(tab.dem.ethnic, file="tab_ethnic.csv")
write.csv(tab.mdt.all, file="tab_mdtall.csv")
write.csv(tab.dem.marital, file="tab_marital.csv")
write.csv(tab.dem.obj, file="tab_obj.csv")
write.csv(tab.dem.sex, file="tab_sex.csv")
write.csv(tab.dem.urbrur, file="tab_urbrur.csv")
write.csv(tab.dem.vstat, file="tab_vstat.csv")
write.csv(tabm.hads, file="tab_mhads.csv")

save("0813analysis.rda")
