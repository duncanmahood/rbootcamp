## rbootcamp final ##
## Duncan Mahood   ##

## Set the working directory to the folder where the datasets are saved ##
getwd()
setwd("/Users/duncanmahood/OneDrive - Emory University/_Fall 2018/R Bootcamp/NSFG Data/")
dir()

## install rio package for data import and export functions  ##
install.packages("rio")
install_formats()
library(rio)

## import data ##
dir()
female <- import("nsfg_female.csv")
male <- import("nsfg_male.csv")
religion <- import("religion.txt")

## Recode 8 and 9 as NA ##
summary(female)   # female has no 8s or 9s to recode
summary(male)     # male has 8s in variables: SEDBC, SEDCOND, SEDSTD
summary(religion) # religion has 9s in variable: ATTND14

  # Recode Male
  male$SEDBC[male$SEDBC==8] <- NA
  summary(male$SEDBC)
  
  male$SEDCOND[male$SEDCOND==8] <- NA
  summary(male$SEDCOND)
  
  male$SEDSTD[male$SEDSTD==8] <- NA
  summary(male$SEDSTD)

  #Recode Religion
  religion$ATTND14[religion$ATTND14==8] <- NA
  religion$ATTND14[religion$ATTND14==9] <- NA
  summary(religion$ATTND14)
  
## Rename any inconsistent variables:
  
  names(female)
  names(male)
  names(religion)
  
  ## Change Male$VRY1STAG to Age_sex1
  
  male$Age_sex1 <- male$VRY1STAG
  male$VRY1STAG <- NULL
  summary(male)
  
## Create a gender variable
  ## Female: 0
  ## Male: 1
  
  female$GENDER <- 0
  male$GENDER   <- 1
  
  summary(female$GENDER)
  summary(male$GENDER)

## Merge the male and female datasets
  ## First add USEFSTSX to the male dataset as NA
  
  male$USEFSTSX <- NA
  summary(male$USEFSTSX)
  
  ## Next, Merge the male and female datasets by caseid
  
  mf <- rbind(female, male)  
  summary(mf)  
  
  ###head(mf)
  ###dups <- !duplicated(mf$Caseid)
  ###dups
  
## Merge mf and religion
  
  mfr <- merge(mf, religion, by.x="CASEID", by.y="CASEID")
  summary(mfr)

## Create a dichotomous religious variable using ifelse
  
  # 1:3 = 1  More than once per month
  # 4:7 = 0  Once per month or less
  
  mfr$RELIGIOUS <- ifelse(mfr$ATTND14 < 4, 1, 0)
  summary(mfr$RELIGIOUS)
  
## Create a dichotomous variable for comprehensive sexual education
  
  mfr$SECOMP <- ifelse(mfr$SEDNO + mfr$SEDBC + mfr$SEDCOND + mfr$SEDSTD == 4, 1, 0)
  summary(mfr$SECOMP)

## Create a Table 1 with descriptive statistics for age, age at first sex, gender, 
##  race, religiosity, used contraception at first sex and comprehensive sex ed
  
#  AGE        Mean, SD, Missing
  age <- data.frame(Variable = as.character("Age"), Mean = as.numeric(mean(mfr$AGE_R)), SD = as.numeric(sd(mfr$AGE_R)), Missing = as.integer(summary(mfr$AGE_R)[7]))
  age
  
#  AGE_SEX1   Mean, SD, Missing
  age_sex1 <- data.frame(Variable = as.character("Age at First Sex"), Mean = as.numeric(summary(mfr$Age_sex1)[4]), SD = as.numeric(sd(mfr[!is.na(mfr$Age_sex1), 9])), Missing = as.integer(summary(mfr$Age_sex1)[7]))
  age_sex1
  
#  GENDER     Categories, Missing
  prop.table(table(mfr$GENDER, useNA = 'always'))
  
#  RACE       Categories, Missing
  prop.table(table(mfr$RACE, useNA = 'always'))

#  RELIGIOSITY
  prop.table(table(mfr$RELIGIOUS, useNA = 'always'))
  
#  USED CONTRACEPTION AT FIRST SEX
  prop.table(table(mfr$USEFSTSX, useNA = 'always'))
  
#  COMPREHENSIVE SEX
  prop.table(table(mfr$SECOMP, useNA = 'always'))
  
### Plug values into Table1 in Word  ##
  
  
##  Conduct an analysis to determine the association between        ##
##  comprehensive sex education and age at first sexual encounter   ##

    boxplot(mfr$Age_sex1~mfr$SECOMP)
      
    mytest <- lm(mfr$Age_sex1~mfr$SECOMP)
    summary(mytest)
    anova(mytest) 
    #pvalue: .20 not significantly different 
    

    
