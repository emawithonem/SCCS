install.packages("SCCS")
# install.packages("SelfControlledCaseSeries")
# Warning in install.packages :package 'SelfControlledCaseSeries' is not available (for R version 3.6.1)
# setRepositories()
# ap <- available.packages()
"SelfControlledCaseSeries" %in% rownames(ap)
# false

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
pkgbuild::check_build_tools(debug = TRUE)

library(SCCS)
# load(SCCS)--> packages tab, check the tick box for SCCS

# library(SelfControlledCaseSeries)


autdat<-(SCCS::autdat)
itpdat<-(SCCS::itpdat)
itp.mod1 <- standardsccs(event~mmr, indiv=case, astart=sta, aend=end,
                         aevent=itp, adrug=mmr, aedrug=mmr+42, data=itpdat)

itp.mod2 <- standardsccs(event~mmr+age, indiv=case, astart=sta, aend=end,
                         aevent=itp, adrug=mmr, aedrug=mmr+42,
                         agegrp=c(427,488,549,610,671), data=itpdat)

lrtsccs(itp.mod1,itp.mod2)

itp.mod3 <- standardsccs(event~mmr+age, indiv=case, astart=sta, aend=end,
                         aevent=itp, adrug=mmr, aedrug=mmr+42, expogrp=c(0,15,29),
                         agegrp=c(427,488,549,610,671), data=itpdat)

lrtsccs(itp.mod2,itp.mod3)

itp.mod <- nonparasccs(indiv=case, astart=sta, aend=end,
                       aevent=itp, adrug=mmr, aedrug=mmr+42, sp1=28000, sp2=1200,
                       data=itpdat)


plot(itp.mod)

mmr.mod1<-standardsccs(formula=event~mmr, indiv=case, astart=sta, aend=end, aevent=diag, adrug=mmr, aedrug=mmr+42, data=autdat)

#get sample data from SCCS


#build model using SCCS

# build model using SelfControlledCaseSeries

# try different models

# generate plots from package


