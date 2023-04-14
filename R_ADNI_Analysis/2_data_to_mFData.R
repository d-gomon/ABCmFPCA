rm(list=ls())
load("data_cleaned/ADNI_cleaned.Rdata")


# Baseline covariates (not time-varying in adnimerge)
baseline.covs <- c("AGE", "PTGENDER", "PTEDUCAT", "status.bl", "APOE4") # b5 

#Load survival data
data.surv <- df.surv_preds

#Load long data
data.long <- df.long_censored

#We no longer work with the transformed data - only relevant for mixed modelling
#data.long <- df.long_censored_transformed


vars_manual_remove <- c("TAU", "PTAU", "ABETA")
vars_irrelevant <- c(
  names(data.long)[grepl(".bl", names(data.long))], # Exclude variables with `.bl` suffix including Years.bl and Months.bl
  "id", "RID",
  "time", "event", "status", "DX", # Survival information
  "VISCODE", "EXAMDATE", "Y", "M", "Month", # Time variables
  "AGE", "age.fup", 
  "COLPROT", "ORIGPROT", "PTID", "SITE", # Visit information
  "PTGENDER", "PTEDUCAT", "PTETHCAT", "PTRACCAT", "PTMARRY", "APOE4", # Baseline variables
  "FSVERSION", "IMAGEUID", "FLDSTRENG" # Metadata for image
)
vars_ignore <- c(vars_manual_remove, vars_irrelevant) # Variables that will not be considered as long covariates


vars_long <- names(data.surv)[!(names(data.surv) %in% vars_ignore)]

################################################################################
#Some functions used for data processing
################################################################################

#Converts the long data we have to the mFData format
long_to_mfdat <- function(dat, id_name, var_name, time_var, scale = TRUE){
  df <- dat
  if(isTRUE(scale)){
    df[, var_name] <- scale(df[, var_name])
  }
  X.temp <- as.matrix(reshape(df[, c(id_name, time_var, var_name)],
                              idvar = id_name, timevar = time_var,
                              direction = "wide"))[, -1]
  X.temp <- X.temp[,order(unique(df[, time_var]))]
  rownames(X.temp) <- unique(df[, id_name])
  return(funData(argvals = sort(unique(df[, time_var])), X = X.temp))
}

#Create a spaghetti plot of a variable in the longitudinal data
spaghetti <- function(var_name, dat){
  var <- enquo(var_name)
  p <- ggplot(data = dat, aes(x = times, y = !!var, group = RID))
  p <- p + geom_line()
  p
}

################################################################################
#Create mFData 
################################################################################

library(funData)

mdat <- multiFunData(lapply(1:length(vars_long), FUN = function(x){
  long_to_mfdat(dat = data.long, id_name = "RID", var_name = vars_long[x], time_var = "Y")}))
names(mdat) <- vars_long


#percentage of non-NA values per variable
missingness <- sapply(1:length(mdat), function(x) mean(!is.na(mdat[[x]]@X)))
names(missingness) <- vars_long

#Display percentage of missing observations per variable
print(missingness)

#Summary of resulting data
summary(missingness[which(missingness > 0.1)])


#Percentage of patients with at least a single observation on a variable
miss_single <- sapply(1:length(mdat), function(x) 1- sum(rowSums(!is.na(mdat[[x]]@X)) == 0)/nObs(mdat))
names(miss_single) <- vars_long
print(miss_single)

#Only consider variables with at least 90 percent of subjects having at least 1 observation
vars_long_21 <- vars_long[which(miss_single > 0.9)]

mdat_21 <- multiFunData(lapply(1:length(vars_long_21), FUN = function(x){
  long_to_mfdat(dat = data.long, id_name = "RID", var_name = vars_long_21[x], time_var = "Y")}))
names(mdat_21) <- vars_long_21


################################################################################
#Create baseline and survival data
################################################################################


#Baseline variables
bl_vars <- c("AGE", "PTGENDER", "PTEDUCAT", "status.bl", "APOE4") # b5 
X_baseline <- as.data.frame(data.surv[, bl_vars])
rownames(X_baseline) <- data.surv$RID

#Create Y_surv_train/pred
Y_surv <- as.matrix(data.surv[, c("time", "event")])
colnames(Y_surv) <- c("time", "status")

#Create age for age adjustment.
age <- round(data.surv$AGE)



save(mdat_21, mdat, X_baseline, Y_surv, age, vars_long_21, file = "ADNImFData.Rdata")

