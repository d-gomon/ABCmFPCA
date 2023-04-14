rm(list = ls())
load("ADNImFData.Rdata")



#Discrete variables summary
n_subjects <- nrow(X_baseline)
SumTable_discrete <- matrix(NA, nrow = 9, ncol = 2)
rownames(SumTable_discrete) <- c("GenderF", "GenderM", "blCN", "blMCI", "prot0", "prot1", "prot2", "DiagAD", "DiagCens")
colnames(SumTable_discrete) <- c("Percent", "Count")

SumTable_discrete[, 2] <- c(summary(X_baseline$PTGENDER), summary(X_baseline$status.bl), summary(X_baseline$APOE4), sum(Y_surv[, "status"] == 1),
                            sum(Y_surv[, "status"] == 0))
SumTable_discrete[,1] <- round(SumTable_discrete[,2]/n_subjects * 100, 1)
print(SumTable_discrete)


#Continuous variables summary
SumTable_continuous <- matrix(c(summary(X_baseline$AGE)[c(3, 2, 5, 1, 6)], summary(X_baseline$PTEDUCAT)[c(3, 2, 5, 1, 6)],
                         summary(Y_surv[which(Y_surv[, "status"] == 1), "time"])[c(3, 2, 5, 1, 6)],
                         summary(Y_surv[which(Y_surv[, "status"] == 0), "time"])[c(3, 2, 5, 1, 6)]), byrow = TRUE, nrow = 4, ncol = 5)
SumTable_continuous <- round(SumTable_continuous, 2)
rownames(SumTable_continuous) <- c("Age", "Education", "AD", "Censored")
colnames(SumTable_continuous) <- c("Median", "LQR", "RQR", "RangeL", "RangeR")
print(SumTable_continuous)



#Average percent of possible outcomes recorded per variable
VariableMissingness <- sapply(1:length(mdat_21), function(x) sum(!is.na(mdat_21[[x]]@X))/(ncol(mdat_21[[1]]@X) * nObs(mdat_21)))
names(VariableMissingness) <- vars_long_21
print(VariableMissingness)
print(summary(VariableMissingness))



#Determine amount of measurements for each subject per variable:
#Make matrix with rows subject num and columns variable num. Entries are then num recorded observations for patient on that var
ObsPerSubject <- sapply(1:length(mdat_21), function(x) sapply(1:nObs(mdat_21), function(y) sum(!is.na(mdat_21[[x]]@X[y, ]))))
#Average number of measurements per patient over all variables
rowMeans(ObsPerSubject)
#Summary over all patients
summary(rowMeans(ObsPerSubject))


#Summaries per variable
summary(colMeans(ObsPerSubject))









