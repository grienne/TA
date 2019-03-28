#specify the packages of interest
packages = c("epiR", "tidyverse", "survival")

#use this function to check if each package is on the local machine
#if a package is installed, it will be loaded
#if any are not, the missing package(s) will be installed and loaded
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

#verify they are loaded
search()

dat1 <- birthwt; head(dat1)


## Generate a table of cell frequencies. 

#-----------------------------------------------------------------------------------------

## First set the levels of the outcome
## and the exposure so the frequencies in the 2 by 2 table come out in the
## conventional format:


#Turn the variables for 2x2 contingency tables into factors
#Note: the levels statement uses the variables in the column, so 1, 0, etc
dat1$low <- as.factor(dat1$low)
dat1$smoke <- as.factor(dat1$smoke)
dat1$race <- as.factor(dat1$race)


## Generate the 2 by 2 table. Exposure (rows) = smoke. Outcome (columns) = low. 
## Note order of variables; dnn command places titles (it does more, but ignore the rest for now)
tab1 <- table(dat1$smoke, dat1$low, dnn = c("Smoke", "Low BW"))
print(tab1)



## Compute the incidence risk ratio and other measures of association:
epi.2by2(dat = tab1, method = "cohort.count",
         conf.level = 0.95, units = 100, homogeneity = "breslow.day",
         outcome = "as.columns")


## Odds ratio:
## The odds of having a low birth weight child for smokers is 2.02
## (95% CI 1.08 to 3.78) times greater than the odds of having
## a low birth weight child for non-smokers.


## Now stratify by race: Note added a 3rd variable. Remember, order matters
tab2 <- table(dat1$smoke, dat1$low, dat1$race,
              dnn = c("Smoke", "Low BW", "Race"))
print(tab2)



## Compute the crude odds ratio, the Mantel-Haenszel adjusted odds ratio
## and other measures of association:
rval <- epi.2by2(dat = tab2, method = "cohort.count",
                 conf.level = 0.95, units = 100, homogeneity = "breslow.day",
                 outcome = "as.columns")
print(rval)





## After accounting for the confounding effect of race, the odds of
## having a low birth weight child for smokers is 3.09 (95% CI 1.49 to 6.39)
## times that of non-smokers.


## Compare the Greenland-Robins confidence intervals for the Mantel-Haenszel
## adjusted attributable risk with the Wald confidence intervals for the
## Mantel-Haenszel adjusted attributable risk:
rval$massoc$ARisk.mh.green
rval$massoc$ARisk.mh.wald



#----------------------------------------------------------------

dat <- as.table(matrix(c(136,22050,1709,127650), nrow = 2, byrow = TRUE))

#note structure: first column is cases, second is person time
print(dat)

rval <- epi.2by2(dat = dat, method = "cohort.time", conf.level = 0.95,
                 units = 1000, homogeneity = "breslow.day", outcome = "as.columns")

rval

summary(rval)$ARate.strata.wald


## The incidence rate of cancer was 7.22 cases per 1000 person-years less in the
## blind, compared with those who were not blind but had severe visual impairment
## (90% CI 6.00 to 8.43 cases per 1000 person-years).
round(summary(rval)$IRR.strata.wald, digits = 2)

