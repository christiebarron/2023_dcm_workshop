
#a shortcut for loading and/or installing all required packages
if (!require(pacman)) install.packages("pacman")
pacman::p_load(janitor, tidyverse, GDINA, psych, psychometric)



#read in the performance data and clean the column names for easier use with three steps
# 1) read in data with readr's read_csv function
# 2) use janitor's clean_names() function to make column names easier to work
# 3) convert from tidyverse's default tibble to R's basic data.frame object for GDINA

ecpe_df <- readr::read_csv(file = "raw_data/ecpe_grammar.csv") %>%  
  janitor::clean_names() %>%   
  as.data.frame() 

#save data with id column
ecpe_id <- ecpe_df

#remove id from data frame for analyses using dplyr::select() function
ecpe_df <- dplyr::select(ecpe_df, -id)


#read in the q-matrix and clean the column names
ecpe_q <- readr::read_csv(file = "raw_data/ecpe_qmatrix.csv") %>% 
  janitor::clean_names() 

# create a vector of q-matrix names (see Landshaw, 2017 within Rupp and Leighton 2017 or george et al., 2015)
attributes <- c("morphosyntactic", "cohesive", "lexical") 

#save the attributes as column names (overwriting the default "a1") using the vector of q-matrix names
names(ecpe_q) <- attributes

#create vector of item names for future use
item_vec <- colnames(ecpe_df)



#print first 5 rows to show that it worked
head(ecpe_q)





#getting more information about the dplyr::select() function as an example
help(select, package = "dplyr")
?dplyr::select()



#get the structure of the data with str()
str(ecpe_df)

#get summary statistics of the data with summary(). assumes continuous variables
summary(ecpe_df)

#view the first 5 rows of the data with head().
head(ecpe_df)

#get descriptive statistics (that assume continuous variables) using psych package's describe() function. For brevity, only keep the last 5 rows using tail()
tail(psych::describe(ecpe_df))




#use the psychometric package to run CTT as prelim exploration of the data. 
psychometric::item.exam(ecpe_df, discrim=TRUE)[,c(1, 3, 4, 5, 8)] #item-total, item discrim, item difficulty




#run the model by giving GDINA() the dataframe, q-matrix, and item-level model to use
ecpe_est <- GDINA::GDINA(dat = ecpe_df, Q = ecpe_q, model = "GDINA") #, mono.constraint = TRUE) 




#print summary information about the model
print(ecpe_est)

#get summary information about the model
summary(ecpe_est)



#adding a monotonicity constraint
ecpe_est <- GDINA::GDINA(dat = ecpe_df, Q = ecpe_q, model = "GDINA", mono.constraint = TRUE) 




#get high-level info about gdina list object
summary(ecpe_est)

#get more fine-grained information about the list. change max.level argument for even more information.
str(ecpe_est, max.level = 1)




#default is de la Torre and Chiu’s (2016) algorithm for q-matrix validation.
ecpe_qval <- Qval(ecpe_est, method = "PVAF")
ecpe_qval
#PVAF stands for "the proportion of variance accounted for"

#use an iterative procedure (test-attribute level) described by Najera et al., (2019, 2020)
ecpe_qval <- Qval(ecpe_est, method = "PVAF", eps = -1,
              iter = "test.att", iter.args = list(verbose = 1))
ecpe_qval

#method = "Wald" uses a stepwise method (Ma & de la Torre, 2019).
ecpe_qval <- Qval(ecpe_est, method = "Wald")
ecpe_qval



#Create a MESA plot

#note: can replace c(1,9) with c(1:nrow(ecpe_q)) as a shorthand for all items
plot(ecpe_qval, item = c(1,9)) 




#extract and save the suggested Q-matrix using the extract function
ecpe_q_empirical <- GDINA::extract(ecpe_qval, what = "sug.Q")

#alternatively, can extract and save the suggested Q-matrix from the list directly
ecpe_q_empirical <- ecpe_qval[["sug.Q"]]

#If there were some suggestions we want to keep, and some we want to discard, we can manually make that change instead:
#ecpe_q_empirical <- ecpe_q
#ecpe_q_empirical[3,] <- as.list(c(1, 0, 0)) #extract the 3rd row of q-matrix and replace it with our revised q.

#print the revised q
ecpe_q_empirical



#run a model with the suggested q-matrix
ecpe_est2 <- GDINA::GDINA(ecpe_df, ecpe_q_empirical, mono.constraint = TRUE)



#compare the fit of the model with original q-matrix (est) and revised q-matrix (est2)
anova(ecpe_est2,ecpe_est)



#first, informally inspect whether any items may not require all interaction effects (based on patterns of probabilities) 
GDINA::extract(ecpe_est2, what = "catprob.parm")

#informally inspect plot
plot(ecpe_est2, withSE = TRUE)




#conduct the model comparison. By default uses the wald test
ecpe_model_compare <- GDINA::modelcomp(ecpe_est2)
ecpe_model_compare



#extract the recommended models from the model using the extract function
ecpe_model_items <- GDINA::extract(ecpe_model_compare, what = "selected.model")[["models"]]

#alternatively, manually extract the models from the list
ecpe_model_items <- ecpe_model_compare$selected.model$models



#run the new DCM model with more parsimonious item-specific DCM measurement model
ecpe_est3 <- GDINA::GDINA(dat = ecpe_df, Q = ecpe_q_empirical, model = ecpe_model_items, mono.constraint = TRUE)

#compare the item-level model (est3) with fully saturated model (est2)
anova(ecpe_est3, ecpe_est2)




#extract the test-level model data fit using the modelfit function, then print results:
ecpe_tfit3 <- GDINA::modelfit(ecpe_est3)
ecpe_tfit3 

#extract aggregated item fit as a measure of test-level fit using itemfit() function
ecpe_ifit3 <- itemfit(ecpe_est3)
ecpe_ifit3



#relative fit statistics are printed in the first part of this output
print(ecpe_tfit3)

#Compare all models thus far. Note the p-values compare all models to the original ecpe_est model (might not be helpful)
ecpe_tfit_relative <- stats::anova(ecpe_est, ecpe_est2, ecpe_est3)



#Extract the item pair fisherz and log odds
head(ecpe_ifit3$r)
head(ecpe_ifit3$logOR)

#note: used head() just to keep the output succinct.




#note this heatmap plot is of the p-values for the MaxAD.r and MaxAD.LOR.
plot(ecpe_ifit3)



plot(ecpe_est3, withSE = TRUE)



#extract discrimination metrics
ecpe_discrim3 <- GDINA::extract(ecpe_est3, what = "discrim")

#print the results, arranged lowest (worst) to highest (best)
dplyr::arrange(ecpe_discrim3, `P(1)-P(0)`)



#get test-level, pattern-level, and attribute-level accuracy
ecpe_acc3 <- GDINA::CA(ecpe_est3, what = "MAP")

#print all
ecpe_acc3 

#save test-level classification accuracy (Iaconangelo, 2017) into R object
ecpe_acc_test3 <- ecpe_acc3$tau

#save pattern-level classification accuracy (Iaconangelo, 2017)
ecpe_acc_pat3 <- ecpe_acc3$tau_l

#save attribute level classification accuracy (Wang et al., 2015)
ecpe_acc_att3 <- ecpe_acc3$tau_k

#conditional classification matrix (Iaconangelo, 2017)
ecpe_acc_ccm3 <- ecpe_acc3$CCM





## ecpe_est_boot3 <- GDINA::bootSE(ecpe_est3,bootsample = 5000,randomseed=123)



#item probabilities of each reduced latent class with standard errors.
stats::coef(ecpe_est3, withSE = TRUE) # item probabilities of success & standard errors



#extract items with log or logit link to aid delta interpretation. item_vec is vector of item names. cbind() combines two vectors by columns. as_tibble() turns into tibble. filter() selects models without an identity link.
cbind(item_vec, GDINA::extract(ecpe_est3, what = "linkfunc")) %>% 
  as_tibble() %>% 
  dplyr::filter(V2 != "identity") %>% 
  print()

#Delta parameters with standard errors
print(stats::coef(ecpe_est3, what = "delta", withSE = TRUE))

#alternatively:
  #GDINA::extract(ecpe_est3, what = "delta.parm")
  #GDINA::extract(ecpe_est3, what = "delta.se")





ecpe_est3_lcprob <- stats::coef(ecpe_est3, "LCprob")
colnames(ecpe_est3_lcprob) <- paste0("lcprob_", colnames(ecpe_est3_lcprob))
head(ecpe_est3_lcprob)



#Guessing and slip parameters with standard errors
head(stats::coef(ecpe_est3, what = "gs", withSE = TRUE))




#Item is the item(s) you want to plot
plot(ecpe_est3, item = 10, withSE = TRUE)



#select an item to show
item_to_plot = 1

#create a base R barplot, subsetting the data to just include the relevant item
barplot(ecpe_est3$LC.prob[item_to_plot,],
        ylab = "Probability of Correct Response",
        xlab = "Latent Class Membership")



# EAP estimates of attribute profiles are default. using head() to only view first 5
ecpe_est3_eap <- GDINA::personparm(ecpe_est3)
colnames(ecpe_est3_eap) <- paste0(colnames(ecpe_est3_eap), "_eap")
head(ecpe_est3_eap)

# extract MAP estimates of attribute profiles
head(GDINA::personparm(ecpe_est3, what = "MAP")) 

# MLE estimates of attribute profiles
head(GDINA::personparm(ecpe_est3, what = "MLE")) 




#mp stands for mastery probabilities
ecpe_est3_mp <- GDINA::personparm(ecpe_est3, what = "mp")
colnames(ecpe_est3_mp) <- paste0(colnames(ecpe_est3_mp), "_mp")
head(ecpe_est3_mp)



##plot mastery probability for individual 8 
plot(ecpe_est3, what = "mp", person = 8) 

#plot the marginal mastery probability for individuals 1, 8, 28, and 31
plot(ecpe_est3, what = "mp", person = c(1, 8, 28, 31))



plot(ecpe_est3, what = "posterior.prob", person = 8)




#Estimated proportions of the latent classes: attribute posterior probabilities
ecpe_est3_postprob <- GDINA::extract(ecpe_est3, what = "posterior.prob")
ecpe_est3_postprob

#prevalance of non-proficiency and proficiency in each attribute
ecpe_est3_attprev <- GDINA::extract(ecpe_est3, what = "prevalence")
ecpe_est3_attprev
#another way of getting estimated proportions of students in each latent class
  #stats::coef(ecpe_est3,"lambda")




plot(ecpe_est3, what = "posterior.prob")



ecpe_est3_tetcor <- psych::tetrachoric(x = GDINA::extract(ecpe_est3,"attributepattern"),
                   weight = GDINA::extract(ecpe_est3,"posterior.prob"))
ecpe_est3_tetcor



save.image(file = "output/ecpe_dcm.Rdata")



#save the 'final' estimated gdina object
saveRDS(ecpe_est3, file = "output/ecpe_est3.Rds")

#save the cleaned data used in analysis
write_csv(ecpe_df, file = "clean_data/ecpe_data.csv")

#save the original and revised q matrices
write_csv(ecpe_q, file = "clean_data/ecpe_qmatrix_original.csv")
write_csv(ecpe_q_empirical, file = "clean_data/ecpe_qmatrix_revised.csv")

#save the item-specific DCMs used in model calibration
write_csv(ecpe_model_compare$selected.model, file = "output/ecpe_item_dcm.csv")

#save the test-level relative fit indices
write_csv(ecpe_tfit_relative$IC, file = "output/ecpe_tfit_relative.csv")

#save various item statistics
write_csv(cbind(ecpe_q_empirical, ecpe_model_compare$selected.model, ecpe_discrim3, ecpe_est3_lcprob), file = "output/ecpe_itemstats.csv")



#create a new text file named ecpe_modelbuild_modelfit.txt in the "output" folder
sink(file = "output/ecpe_modelbuild_modelfit.txt")

#print the analysis title and the system time analyzed
paste0("ecpe analysis", Sys.time())

#print some details of q-matrix evaluation.
print("##### Empirical Q-Matrix Evaluation #######")
ecpe_qval$method
print("suggested q-matrix (also saved in clean_data)")
print(as.list(cbind(item_vec, ecpe_qval$sug.Q)))

#print suggested model
print("######### Model Calibration #######")
paste0("calibration method = ", ecpe_model_compare$method)

print("######### Model Fit ###########")
print("test-level fit")
ecpe_tfit3
print("aggregated item fit (as source of test fit)")
ecpe_ifit3
sink()

#shouldn't be necessary, but closes all connections again. Use if you have any errors or difficulties.
#closeAllConnections()




#create vector of parameters to loop over
to_extract <-  c("catprob", "delta", "gs", "itemprob", "LCprob")

#use sink() to save printed output to .txt file
sink(file = "output/ecpe_est3_estimates.txt")

#print the analysis title and the system time analyzed
paste0("ecpe analysis", Sys.time())

#document the model used
print("model = ecpe_est3")

#print general model information
print(ecpe_est3)
summary(ecpe_est3)

#print classification accuracy
print("Classification Reliability")
print(ecpe_acc3)

print("#######ITEM-LEVEL ESTIMATES###########")
#loop through, printing all estimates.
for (est in to_extract) {
  print(est)
  print(coef(ecpe_est3, what = est, withSE = TRUE))
}

print("item discrimination")
print(ecpe_discrim3)

print("############ NON-ITEM ESTIMATES ###############")

print("Attribute Correlations")
ecpe_est3_tetcor$rho
ecpe_est3_tetcor$tau

print("Attribute Prevalence")
ecpe_est3_attprev$all

print("Latent Class Proportions")
ecpe_est3_postprob
#close the connection and save the results
sink()

#shouldn't be necessary, but closes all connections again. Use if you have any errors or difficulties.
#closeAllConnections()




write_csv(cbind(ecpe_id, ecpe_est3_eap, ecpe_est3_mp), file = "clean_data/ecpe_data_dcm.csv")

