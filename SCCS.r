# This script provides an introduction to Self Controled Case Study analysis with the SCCS package and the SelfControlledCaseSeries package.


path <- "C:\\Users\\Acer\\OneDrive\\Documents\\GitHub\\SCCS"
setwd(path)


# install.packages("SCCS")
library(SCCS)


#load some sample data. The SCCS package includes MANY sample datasets.
itpdat<-(SCCS::itpdat)
# The data comprise ages in days at measles, mumps and rubella (MMR) vaccination and hospital admission for idiopathic thrombocytopaenic purpura (ITP)

#Model building, simple to complex. 

#this model is a simple clogit, where the event is a function of exposure, stratified within individuals
itp.mod1 <- standardsccs(event~mmr, indiv=case, astart=sta, aend=end,
                         aevent=itp, adrug=mmr, aedrug=mmr+42, data=itpdat)
itp.mod1
# adding age groups (temporal confounder)
# likelihood is calculated piecewise for each age category
itp.mod2 <- standardsccs(event~mmr+age, indiv=case, astart=sta, aend=end,
                         aevent=itp, adrug=mmr, aedrug=mmr+42,
                         agegrp=c(427,488,549,610,671), data=itpdat)

#compare models
lrtsccs(itp.mod1,itp.mod2)


itp.mod3 <- standardsccs(event~mmr+age, indiv=case, astart=sta, aend=end,
                         aevent=itp, adrug=mmr, aedrug=mmr+42, expogrp=c(0,15,29),
                         agegrp=c(427,488,549,610,671), data=itpdat)

#
lrtsccs(itp.mod2,itp.mod3)


# The data comprise ages in days at measles mumps rubella (MMR) vaccination and autism diagnosis.
autdat<-(SCCS::autdat)


#this model demonstrates that there is no association between MMR and autism diagnosis.
mmr.mod1<-standardsccs(formula=event~mmr, indiv=case, astart=sta, aend=end, aevent=diag, adrug=mmr, aedrug=mmr+42, data=autdat)
mmr.mod1

#######################
#SCCS model extensions#
#######################

# eventdepenexp SCCS with event-dependent exposure
# This modified method assumes that no exposure is possible following a unique event (i.e. contraindication)

rotdat<-SCCS::rotdat

# Analysis of rotavirus vaccination and intussusception data
# Model 1: Three doses of the same vaccine exposure OPV (OPV, OPV2 and opv3),
# only one risk period [adrug, aedrug]
rot.mod1 <- eventdepenexp(indiv=case, astart=sta, aend=end,
                          aevent=intus, adrug=cbind(rv,rvd2),
                          expogrp=1,aedrug=cbind(rv+21,rvd2+21),
                          agegrp=seq(56,168,14), dataformat="multi", data=rotdat)
rot.mod1
# Model 2: Two doses with two riks periods using expogrp, 1-7 and 8-21
rot.mod2 <- eventdepenexp(indiv=case, astart=sta, aend=end,
                          aevent=intus, adrug=cbind(rv,rvd2),
                          aedrug=cbind(rv+21,rvd2+21), expogrp=c(1,8),
                          agegrp=seq(56,168,14), dataformat="multi",
                          data=rotdat)
rot.mod2


# eventdepenobs SCCS with event-dependent observation periods
# i.e. death cuts observation short, Analysis with censoring

# Nicotine replacement therapy and myocardial infarction (MI)--> increased risk of death
# With no age effect included

nrtdat<-SCCS::nrtdat

nrt.mod <- eventdepenobs(event~nrt, indiv=case, astart=nrt,
                         aend=act, aevent=mi, adrug=nrt, aedrug=nrt+28,
                         censor=cen, expogrp=c(0,8,15,22), agegrp=NULL,
                         data=nrtdat)

nrt.mod

midat<-SCCS::midat
# Respiratory tract infections and MI
# Age effect included
# intial values provided and there are two risk periods
table(duplicated(midat$case))
uni <- (1-duplicated(midat$case))
ageq <- floor(quantile(midat$mi[uni==1], seq(0.1,0.9,0.1), names=FALSE))
# age groups
mi.mod <- eventdepenobs(event~rti+age, indiv=case, astart=sta,
                        aend=end, aevent=mi, adrug=rti, aedrug=rti+14,
                        expogrp=c(0,8), agegrp=ageq, censor=cen, data=midat,
                        initval=rep(1.1,4))
mi.mod

#nonparasccs
# Fits a spline-based non parametric SCCS model where both the exposure related relative incidence and age related relative incidence functions are represented by spline functions; that is, linear combinations of M-spline

#sp : smoothing parameter (1=age, 2=exposure)
#kn : knots for splines (1=age, 2=exposure) must be integer>=5
itp.mod.np1 <- nonparasccs(indiv=case, astart=sta, aend=end,
                          aevent=itp, adrug=mmr, aedrug=mmr+42, sp1=28000, sp2=1200,
                          data=itpdat)

summary(itp.mod.np1)
plot(itp.mod.np1)

itp.mod.np2 <- nonparasccs(indiv=case, astart=sta, aend=end,
                           aevent=itp, adrug=mmr, aedrug=mmr+42, kn1=6, kn2=6, sp1=28000, sp2=1200,
                           data=itpdat)

summary(itp.mod.np2)
plot(itp.mod.np2)


plot(itp.mod.np1, main= "nonparametric w/o knots")
plot(itp.mod.np2, main= "nonparametric with knots")



#SelfControlledCaseSeries is a very new package, still in beta development, so you cannot yet access via install.packages.
#the code below will install the current version of the package

# install.packages("SelfControlledCaseSeries")
# Warning in install.packages : package 'SelfControlledCaseSeries' is not available (for R version 3.6.1)

# install.packages("remotes")
library(remotes)

install_github("ohdsi/SelfControlledCaseSeries")
 # > install_github("ohdsi/SelfControlledCaseSeries")
# Downloading GitHub repo ohdsi/SelfControlledCaseSeries@HEAD
# trying URL 'https://cran.rstudio.com/bin/windows/Rtools/Rtools35.exe'
# Content type 'application/x-msdownload' length 108622512 bytes (103.6 MB)
# downloaded 103.6 MB
# 
# Error: Failed to install 'SelfControlledCaseSeries' from GitHub:
#   Could not find tools necessary to compile a package
# Call `pkgbuild::check_build_tools(debug = TRUE)` to diagnose the problem.

# pkgbuild::check_build_tools(debug = TRUE)


#CRAHSES R#


# build model using SelfControlledCaseSeries

mysettings<-createSccsSimulationSettings(
  meanPatientTime = 4 * 365,
  sdPatientTime = 2 * 365,
  minAge = 18 * 365,
  maxAge = 65 * 365,
  minBaselineRate = 0.001,
  maxBaselineRate = 0.01,
  eraIds = c(1, 2),
  patientUsages = c(0.2, 0.1),
  usageRate = c(0.01, 0.01),
  meanPrescriptionDurations = c(14, 30),
  sdPrescriptionDurations = c(7, 14),
  simulationRiskWindows = list(createSimulationRiskWindow(relativeRisks = 1),
                               createSimulationRiskWindow(relativeRisks = 1.5)),
  includeAgeEffect = TRUE,
  ageKnots = 5,
  includeSeasonality = TRUE,
  seasonKnots = 5,
  outcomeId = 10
)


simulateSccsData(nCases=500, settings=mysettings)




