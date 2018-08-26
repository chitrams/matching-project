### Working with Stata's output ####

matchedGU <- read.csv("./output_data/matchedGU.csv")
m1.dat <- matchedGU %>% select(starts_with("ctrl_"))
m2.dat <- matchedGU %>% select(no:case_numid)
colnames(m1.dat) <- sub("ctrl_", "", colnames(m1.dat))
colnames(m2.dat) <- sub("case_", "", colnames(m2.dat))
m.dat <- bind_rows(m1.dat, m2.dat)

# Change to dates
dat1 <- m.dat
for (i in c("dt_death", "dt", "dt_appt", "dt_inc", "dt_alive")) {
  dat1[[i]] <- as.Date(dat1[[i]], "%Y-%m-%d")
}

# Change to factors
for (i in c("sex", "vstat", "obj", "mdt", "scid_stat", "scidprogress"
            # , "urbrur"
            # , "married"
            )) {
  dat1[[i]] <- as.factor(dat1[[i]])
}

rm(i)
m.dat <- dat1
