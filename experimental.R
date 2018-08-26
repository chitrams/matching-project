# Two-way table for depression vs vital status
with(dat, table(dep, vstat, useNA = "ifany"))

# Exploring depression scores
dat.dep <- dat %>% filter(dep==2)
dat.dep <- dat.dep %>% select(pid, hads, scidprogress, dep)
dat.dep$scidprogress <- factor(dat.dep$scidprogress)
table(dat.dep$scidprogress)

dat.dep <- dat %>% filter(dep==1)
dat.dep <- dat.dep %>% select(pid, hads, scidprogress, dep)
dat.dep$scidprogress <- factor(dat.dep$scidprogress)
table(dat.dep$scidprogress)

dat.dep <- dat %>% filter(dep==0)
dat.dep <- dat.dep %>% select(pid, hads, scidprogress, dep)
dat.dep$scidprogress <- factor(dat.dep$scidprogress)
table(dat.dep$scidprogress)

# SRA: suicide risk assessment
# MDE: Major depressive episode

# Load packages for matching

install.packages("SqlRender")
drat::addRepo("OHDSI")
install.packages("CaseControl", dependencies = F)

require(CaseControl)
library(CaseControl)

library(drat)
