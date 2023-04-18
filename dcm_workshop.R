library(CDM)
library(GDINA)
library(mirt)
library(tidyverse)

## import and clean datasets =======
###read and view cdm reading datafile

#read in the performance data and clean the column names for easier use
ecpe_df <- readr::read_csv(file = "raw_data/ecpe_grammar.csv") %>%  #read in data with readr's read_csv function
  janitor::clean_names() #use janitor's clean_names() function to make column names easier to work with

#read in the q-matrix and clean the column names
ecpe_q <- readr::read_csv(file = "raw_data/ecpe_qmatrix.csv") %>% 
  janitor::clean_names() #make attribute letters lowercase

# q-matrix names (see Landshaw, 2017 within Rupp and Leighton 2017)
attributes <- c("morphosyntactic", "cohesive", "lexical") 

#save the attributes as column names (overwriting the default "a1")
names(ecpe_q) <- attributes

#print to show that worked
ecpe_q

#Get a sense of the data
str(ecpe_df)
summary(ecpe_df)
head(ecpe_df)
psych::describe(ecpe_df)

psych::alpha(ecpe_df)$item.stats



#convert from tibble (a special type of dataframe) to R's basic dataframe so works w/ DCM packages.
ecpe_df <- as.data.frame(ecpe_df)
ecpe_q <- as.data.frame(ecpe_q)

#View(cdm_read)
str(cdm_read)
summary(cdm_read)
head(cdm_read)
psych::describe(cdm_read)

###save analysis-specific info
#big sister to bats (bsbt)
cdm_read_bsbt <- cdm_read %>% dplyr::select(id, RT1_MC1:RT3_MC9, -RT2_MC6, -RT3_MC9) %>% dplyr::mutate(cdm_bsbt = 1) %>% #dplyr::filter(grade_fct != 7 & grade_fct != 8) %>% 
  drop_na() #remove 2 poorly performing items
#bats to marilyn bell (btmb)
cdm_read_btmb <- cdm_read %>% dplyr::select(id, RT3_MC1:RT5_MC9, -RT3_MC9, -RT4_MC7) %>% dplyr::mutate(cdm_btmb = 1) %>% drop_na() #remove 2 poorly performing itmes.

###read and view q matrix
q_read <- readxl::read_excel("../imports/bai_read_cdm.xlsx", sheet = "WithNames_noSA") #using 2nd sheet so can select based on item

q_read_bsbt <- q_read[1:27, c("item", "exp", "inf", "glo")] %>% dplyr::filter(item != "RT2_MC6" & item != "RT3_MC9") %>% dplyr::select(-"item") #select items for the first three booklets, dropping Qmatrix of poorly performing items that dropped in dataset.

q_read_btmb <- q_read[19:45,c("item", "exp", "inf", "glo")] %>% dplyr::filter(item != "RT3_MC9" & item != "RT4_MC7") %>% dplyr::select(-"item") #select items for the first three booklets, dropping Qmatrix of poorly performing items that dropped in dataset.

#save as dataframe so works w/ packages.
q_read_bsbt <- as.data.frame(q_read_bsbt)
q_read_btmb <- as.data.frame(q_read_btmb)

#View(q_read)
str(q_read_bsbt)
str(q_read_btmb)

###remove NAs
id_bsbt <- cdm_read_bsbt
id_btmb <- cdm_read_btmb

cdm_read_bsbt <- cdm_read_bsbt %>% dplyr::select(-id, -cdm_bsbt)
cdm_read_btmb <- cdm_read_btmb %>% dplyr::select(-id, -cdm_btmb)


#old data.
#cdm_read <- drop_na(cdm_read)
#cdm_read3 <- drop_na(cdm_read3)
#cdm_read4 <- drop_na(cdm_read4)
# need to change NAs to 0
#cdm_read[is.na(cdm_read)]=0
#q_read[is.na(q_read)]=0

###convert from tibble to dataframe
cdm_read_bsbt <- as.data.frame(cdm_read_bsbt)
cdm_read_btmb <- as.data.frame(cdm_read_btmb)

## Descriptive Statistics ======
#continuous descriptives
psych::describe(cdm_read_bsbt)
psych::describe(cdm_read_btmb)

##checking characteristics of students in big sister, eagle owl, and bats group
if (!exists("bai_tib")) load("../exports/bai_desc.Rds")

desc_bsbt <- bai_tib %>% dplyr::filter(is.na(bai_tib$RT1_MC1) == FALSE & is.na(bai_tib$RT2_MC1) == FALSE & is.na(bai_tib$RT3_MC1) == FALSE) %>% dplyr::select(gender_fct, grade_fct, age1, ell_fct_student, multi_final, readingSelfEfficacy, writingSelfEfficacy, selfRegulatedLearning, grit, overall_tas) %>% dplyr::mutate(multi_final = factor(multi_final)) %>% dplyr::rename(writing = overall_tas, age = age1) %>% summary()

#checking characteristics of students in bat, family farm, marilyn bell group
desc_btmb <- bai_tib %>% dplyr::filter(is.na(RT3_MC1 == FALSE) & is.na(RT4_MC1 == FALSE) & is.na(RT5_MC1 == FALSE)) %>% dplyr::select(gender_fct, grade_fct, age1, ell_fct_student, multi_final, readingSelfEfficacy, writingSelfEfficacy, selfRegulatedLearning, grit, overall_tas) %>% dplyr::mutate(multi_final = factor(multi_final)) %>% dplyr::rename(writing = overall_tas, age = age1)%>% stats4::summary()

# CDM RESULTS WITH G-DINA R PACKAGE ##########
#https://cran.r-project.org/web/packages/GDINA/vignettes/GDINA.html
#https://wenchao-ma.github.io/GDINA/articles/OnlineExercises/CDMAnalysis_example.html
#(Shi et al., 2021 https://www.mdpi.com/2624-8611/3/4/52)

## Two different Sets Done Together =======
#bsbt = big sister, eagle owls, and bats.
#btmb = bats, family farm, and marilyn bell.

### Model Estimation ----
#for info about monotonicity constraints, see:
#https://journals.sagepub.com/doi/10.1177/0146621620977681 and 
#https://www.mdpi.com/2624-8611/3/4/52  

bsbt_gdina1 <- GDINA::GDINA(dat = cdm_read_bsbt, Q = q_read_bsbt, mono.constraint = TRUE) #estimate the model with monotonicity constraints
btmb_gdina1 <- GDINA::GDINA(dat = cdm_read_btmb, Q = q_read_btmb, mono.contraint = TRUE)  #estimate the model with monotonicity constraints

### Q Matrix Validation -----
bsbt_gdina1_q1 <- Qval(bsbt_gdina1)
bsbt_gdina1_q2 <- Qval(bsbt_gdina1, method = "Wald")
plot(bsbt_gdina1_q2, item = c(1:25))

btmb_gdina1_q1 <- Qval(btmb_gdina1)
btmb_gdina1_q2 <- Qval(btmb_gdina1, method = "Wald")
plot(btmb_gdina1_q2, item = c(1:25))

### Item-level Model Selection
bsbt_gdina1_imodel <- modelcomp(bsbt_gdina1)
btmb_gdina1_imodel <- modelcomp(btmb_gdina1)

### Respecify Model w/ New GDINA model
bsbt_gdina2 <- GDINA::GDINA(dat = cdm_read_bsbt, Q = q_read_bsbt, mono.constraint = TRUE, model = bsbt_gdina1_imodel$selected.model$models) #using separate cdm models for each item based on GDINA package's default selection for 'best' model.

btmb_gdina2 <- GDINA::GDINA(dat = cdm_read_btmb, Q = q_read_btmb, mono.constraint = TRUE, model = btmb_gdina1_imodel$selected.model$models) #using separate cdm models for each item based on GDINA package's default selection for 'best' model.

### Model-Data Fit -----
bsbt_gdina1_tfit <- GDINA::modelfit(bsbt_gdina1)
bsbt_gdina1_ifit <- GDINA::itemfit(bsbt_gdina1)
bsbt_gdina2_tfit <- GDINA::modelfit(bsbt_gdina2)
bsbt_gdina2_ifit <- GDINA::itemfit(bsbt_gdina2)


#summary(bsbt_gdina1_ifit)
anova(bsbt_gdina1, bsbt_gdina2)
plot(bsbt_gdina1_ifit)
plot(bsbt_gdina2_ifit)

btmb_gdina1_tfit <- GDINA::modelfit(btmb_gdina1)
btmb_gdina1_ifit <- GDINA::itemfit(btmb_gdina1)
btmb_gdina2_tfit <- GDINA::modelfit(btmb_gdina2)
btmb_gdina2_ifit <- GDINA::itemfit(btmb_gdina2)

anova(btmb_gdina1, btmb_gdina2)
plot(btmb_gdina1_ifit)
plot(btmb_gdina2_ifit)

### Item Diagnostics ------
#plot(bsbt_gdina2, item = 1:25)
#plot(btmb_gdina2, item = 1:25)
bsbt_gdina2_idiag <- GDINA::extract(bsbt_gdina2, what = "discrim")
btmb_gdina2_idiag <- GDINA::extract(btmb_gdina2, what = "discrim")

### Classification Accuracy -----
bsbt_gdina2_ca <- GDINA::CA(bsbt_gdina2)
bsbt_gdina2_ca
btmb_gdina2_ca <- GDINA::CA(btmb_gdina2)
btmb_gdina2_ca 

## Substantive Results for both CDMs =====
### Person parameters ----
GDINA::personparm(bsbt_gdina2) #person-level attribute mastery profiles predicted (discrete)
GDINA::personparm(bsbt_gdina2, what = "mp") #predicted mastery probaiblities

GDINA::personparm(btmb_gdina2)
GDINA::personparm(btmb_gdina2, what = "mp") #predicted mastery probaiblities

### Attribute probabilities----

#attribute prevelance 
GDINA::extract(bsbt_gdina2, what = "prevalence")
GDINA::extract(btmb_gdina2, what = "prevalence")

#attribute posterior proabilities (estimated latent class size)
GDINA::extract(bsbt_gdina2, what = "posterior.prob")
GDINA::extract(btmb_gdina2, what = "posterior.prob")

#tetrachoric correlation between attributes
#bsbt
psych::tetrachoric(x = GDINA::extract(bsbt_gdina2, what = "attributepattern"), 
                   weight = GDINA::extract(bsbt_gdina2, what = "posterior.prob"))

#btmb
psych::tetrachoric(x = GDINA::extract(btmb_gdina2, what = "attributepattern"), 
                   weight = GDINA::extract(btmb_gdina2, what = "posterior.prob"))

### model coefficients ----
# item probabilities of success for each reduced latent class
coef(bsbt_gdina2, withSE = TRUE)
coef(btmb_gdina2, withSE = TRUE)

#DELTA CDM PARAMETERS. show the intercept and main/interaction effects
coef(bsbt_gdina2, what = "delta", withSE = TRUE)
coef(btmb_gdina2, what = "delta", withSE = TRUE)
# #see "The following code gives delta parameters with standard errors." from https://wenchao-ma.github.io/GDINA/articles/OnlineExercises/GDINA_example.html

#coef(btmb_gdina2, c("catprob", "delta", "gs", "itemprob", "LCprob", "lambda"))

#guessing and slip parameters
coef(bsbt_gdina2, "gs", withSE = TRUE)
coef(btmb_gdina2, "gs", withSE = TRUE)

#item success probabilities for each latent class
coef(bsbt_gdina2, "LCprob")
coef(btmb_gdina2, "LCprob")

# Saving Results ####
save.image(file = "../exports/cdm/cdm.Rdata")

save(cdm_read_bsbt, file = "../exports/cdm/cdm_read_bsbt.Rds")
save(cdm_read_btmb, file = "../exports/cdm/cdm_read_btmb.Rds")




