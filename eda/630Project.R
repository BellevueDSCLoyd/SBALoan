# Sam Loyd
#
# Final Project
# 
# The analysis of this data set will be performed in R.
# 
# The data set was obtained as a compressed archive from Kaggle.

# Data Understanding

# LoanNr_ChkDgt Text Identifier – Primary key, I don't want this to confuse the model
# Name Text Borrower name - Remove for privacy concerns.
# City Text Borrower city
# State Text Borrower state
# Zip Text Borrower zip code
# Bank Text Bank name
# BankState Text Bank state
# NAICS Text North American industry classification system code
# ApprovalDate Date/Time Date SBA commitment issued
# ApprovalFY Text Fiscal year of commitment
# Term Number Loan term in months 
# NoEmp Number Number of business employees
# NewExist Text 1 D Existing business, 2 D New business
# CreateJob Number Number of jobs created (Target variable)
# RetainedJob Number Number of jobs retained (*)
# FranchiseCode Text Franchise code, (00000 or 00001) D Nofranchise
# UrbanRural Text 1 D Urban, 2 D rural, 0 D undefined
# RevLineCr Text Revolving line of credit: Y D Yes, N D No
# LowDoc Text LowDoc Loan Program: Y D Yes, N D No
# ChgOffDate Date/Time The date when a loan is declared to be in default (*)
# DisbursementDate Date/Time Disbursement date (*)
# DisbursementGross Currency Amount disbursed (*)
# BalanceGross Currency Gross amount outstanding (*)
# MIS_Status Text Loan status charged off D CHGOFF, Paid in full D PIF (Target variable - Convert to logical) 
# ChgOffPrinGr Currency Charged-off amount (*)
# GrAppv Currency Gross amount of loan approved by bank 
# SBA_Appv Currency SBA’s guaranteed amount of approved loan
# * warning - future information - most will need to be removed

# Given the nature of this data set it is important that we not let future data 
# be used for our model. For example, several fields are based on information after the 
# loan was approved. Disbursement is an example.

library(readxl)
library(dplyr)
library(pastecs)
library(psych)
library(ggplot2)
library(corrplot)
library(purrr)
library(tidyr)
library(dummies)
library(mltools)
library(naniar)
library(data.table)
library(binaryLogic)
library(reshape2)
library(binaryLogic)
library(RColorBrewer)
library(superml)


# Turn off scientinfic notation 

options(scipen=999)

sbaloan_data <- read.csv('SBAnational.csv',na.strings=c("NA","NaN", " ","") )

# glimpse(sbaloan_data)
summary(sbaloan_data)

# Clean up any non - unique data - None found
sbaloan_data <- sbaloan_data %>% distinct()
summary(sbaloan_data)


# Get rid of $ sign and commas
sbaloan_data$GrAppv <- gsub('[^a-zA-Z0-9.]', '', sbaloan_data$GrAppv)
sbaloan_data$ApprovalFY <- gsub('[^a-zA-Z0-9.]', '', sbaloan_data$ApprovalFY)


# Do we have job creation data for each Year. 
# unique(sbaloan_data$ApprovalFY)

CheckFY <- filter(sbaloan_data, `CreateJob` > 0)

NoCheckFY <- filter(sbaloan_data, `CreateJob` == 0)

# Commented out for length
# CheckFY %>% count(ApprovalFY)
# NoCheckFY %>% count(ApprovalFY)

# How many unique banks
#unique(sbaloan_data$Bank)


# Convert GrAppv to numeric
sbaloan_data$GrAppv <- as.numeric(as.character(sbaloan_data$GrAppv))
sbaloan_data$SBA_Appv <- gsub('[^a-zA-Z0-9.]', '', sbaloan_data$SBA_Appv)
sbaloan_data$RevLineCr <- gsub('[^a-zA-Z0-9]', 'UNKNOWN', sbaloan_data$RevLineCr)

# sbaloan_data$RevLineCr
# unique(sbaloan_data$RevLineCr)

# Convert GrAppv to numeric
sbaloan_data$SBA_Appv <- as.numeric(as.character(sbaloan_data$SBA_Appv))

# Domain Knowledge 
# Retrieved from https://www.investopedia.com/terms/c/chargeoff.asp
# Quote:
# A charge-off refers to debt that a company believes it will no longer 
# collect as the borrower has become delinquent on payments.
# This would be future knowledge as I am interested in loans that 
# Retrieved from https://www.investopedia.com/terms/d/disbursement.asp
# Quote:
# A student loan disbursement is the paying out of loan proceeds to a borrower
# Rebrieved from https://www.census.gov/eos/www/naics/faqs/faqs.html#q1
# Quote:
# The North American Industry Classification System (NAICS, 
# pronounced Nakes) was developed under the direction 
# and guidance of the Office of Management and Budget (OMB) 
# as the standard for use by Federal statistical agencies
# in classifying business establishments for the collection, 
# tabulation, presentation, and analysis of statistical 
# data describing the U.S. economy. Use of the standard provides 
# uniformity and comparability in the presentation of 
# these statistical data. NAICS is based on a production-oriented 
# concept, meaning that it groups establishments 
# into industries according to similarity in the processes 
# used to produce goods or services. 
# NAICS replaced the Standard Industrial 
# Classification (SIC) system in 1997.

# head(sbaloan_data)
summary(sbaloan_data)

sbaloan_data$Month = substr(sbaloan_data$ApprovalDate,3,6)
sbaloan_data$Month <- gsub('[^a-zA-Z]', '', sbaloan_data$Month)
# sbaloan_data$Month
sbaloan_data$Day = substr(sbaloan_data$ApprovalDate,1,2)
sbaloan_data$Day <- gsub('[^[:alnum:]]', '', sbaloan_data$Day)
# sbaloan_data$Day

# Remove future data by dropping columns.
# Future data: BalanceGross, DisbursmentGross, DisbursementDate
#              ChgOffDate, ChgOffPrinGr, RetainedJob, and BalanceGross
#              ApprovalDate, ApprovalFY, DisbursmentGross, Disbursement Date
# Name - removed for privacy and security
# Approval Fiscal Date and Year removed as future data 
# and to avoid problems with predictions at the beginning of a new year.
# City and state - removed as redundant with zip
# Domain Knowledge
# It is also illegal to use local zip in determining loans - 
# can lead to racial bias
# Bank geographical information as well .... page 81 (Seigel, 2016)

# Column Removal
sbaloan_data <- subset(sbaloan_data, select = -c(`BankState`,`City`,`State`,`ApprovalDate`,`BalanceGross`,`RetainedJob`,`Name`,`DisbursementGross`,`DisbursementDate`,`ChgOffDate`,`ChgOffPrinGr`))


vis_miss(sbaloan_data,warn_large_data=FALSE)  + ggtitle("SMB Missingness Analysis")

# Data preparation

# I am going to keep most of these as categories
# Converting here as its a pain after they are factors 
sbaloan_data[is.na(sbaloan_data)]<-'UNKNOWN'


# sbaloan_data$Bank[is.na(sbaloan_data$Bank)] <- " "

# Convert variable as appropriate for analysis
sbaloan_data$Bank = as.factor(sbaloan_data$Bank)
sbaloan_data$NAICS = as.factor(sbaloan_data$NAICS)
sbaloan_data$FranchiseCode = as.factor(sbaloan_data$FranchiseCode)
sbaloan_data$UrbanRural = as.factor(sbaloan_data$UrbanRural)
sbaloan_data$RevLineCr = as.factor(sbaloan_data$RevLineCr)
sbaloan_data$ApprovalFY = as.factor(sbaloan_data$ApprovalFY)
sbaloan_data$Day = as.factor(sbaloan_data$Day)
sbaloan_data$RevLineCr = as.factor(sbaloan_data$RevLineCr)
sbaloan_data$LowDoc = as.factor(sbaloan_data$LowDoc)
sbaloan_data$MIS_Status = as.factor(sbaloan_data$MIS_Status)
sbaloan_data$NewExist = as.factor(sbaloan_data$NewExist)
sbaloan_data$ApprovalFY = as.integer(sbaloan_data$ApprovalFY)
sbaloan_data$Day = as.integer(sbaloan_data$Day)

# Remove loans with unknown status as this is a target variable
sbaloan_data <- filter(sbaloan_data, `MIS_Status` == "CHGOFF" | `MIS_Status` =="P I F")


# df1$IS_PASS = as.factor(df1$IS_PASS)
#glimpse(sbaloan_data)
summary(sbaloan_data)

# Domain Knowledge 
# from https://www.sba.gov/sites/default/files/SDOLoanFactSheet_Oct_2011.pdf
# $5 million
# Quote:
# The exact percentage of the guaranty depends on a variety of factors such as size of 
# loan and which SBA program is to be used. This will be worked out between the 

# SBA and your bank. Amounts - The maximum loan amount is $5 million. The total 
# SBA guarantee for any one borrower may not exceed $3,750,000.
# The data shows outliers. Given the information above anything with over 3,750,000 and be imputed 
# as likely a data error.
# Correcting for this.
sum(sbaloan_data$SBA_Appv > 3750000)

# Given the large data set removing 57 rows should be acceptable 

sbaloan_data <- filter(sbaloan_data, SBA_Appv < 3750001)

# Further validation
sum(sbaloan_data$GrAppv > 5000000)
# Returned 0 so the above truncate took care of these as well


# Numeric only for statistics 
num_sbaloan_data <- sbaloan_data[,c("Term", "NoEmp", "CreateJob", "SBA_Appv", "GrAppv")]

format(round(stat.desc(num_sbaloan_data,basic = TRUE, norm = FALSE), digits = 3), scientific=FALSE)

sample_num_sbaloan_data <- num_sbaloan_data[sample(1:nrow(num_sbaloan_data), 5000,
                                          replace=FALSE),]

format(round(stat.desc(sample_num_sbaloan_data,basic = FALSE, norm = TRUE), digits = 3), scientific=FALSE)
# Significant skew and kurtosis. Do not use models that assume a normal distribution.


# Histograms of numeric features
hist(sbaloan_data$Term,
                    main="Term",
                    xlab="Months",
                    col="blue")

hist(sbaloan_data$NoEmp,
                   main="Employees",
                   xlab="Count of Employees Histogram",
                   col="blue")

hist(sbaloan_data$CreateJob,
                   main="Jobs Created Histogram",
                   xlab="New Jobs",
                   breaks = 100,
                   col="blue")

# Lets filter out the extremes and look
CreateJobAnalysis <- filter(sbaloan_data, `CreateJob` < 100)

hist(CreateJobAnalysis$CreateJob,
     main="Histograms of Jobs Created Under 100",
     xlab="New Jobs",
     breaks = 100,
     col="blue")

# Look at loans that created at least 1 job.
CreateJobAnalysis <- CreateJobAnalysis %>%
        mutate(CreateJob = ifelse( CreateJob == 0,0,1))

coul <- brewer.pal(5, "Set2")
barplot(table(CreateJobAnalysis$CreateJob),names.arg=c("No Jobs", "At least 1 Job"), col=coul, main="SBA Loan Job Creation", ylab="Count",ylim=c(0,900000))
box()

hist(sbaloan_data$SBA_Appv,
                   main="SBA Approval Histogram",
                   xlab="Dollars",
                   col="blue")

hist(sbaloan_data$GrAppv,
                   main="Granting Bank Approval Histogram",
                   xlab="Dollars",
                   col="blue")

# Skew and kertosis confirmed by visuals



# Box plots of one variable
boxplot(sbaloan_data$Term, 
        col=(c("gold","darkgreen")),
        main="Loan Term Box Plot", xlab="Months")

boxplot(sbaloan_data$NoEmp, 
        col=(c("gold","darkgreen")),
        main="Number of Employees Box Plot", xlab="Employee Count"
        )

boxplot(sbaloan_data$CreateJob, 
        col=(c("gold","darkgreen")),
        main="Jobs Created Box Plot", xlab="Employee Count")


boxplot(sbaloan_data$SBA_Appv, 
        col=(c("gold","darkgreen")),
        main="SBA Approval Amount Box Plot", xlab="US DOllars")

boxplot(sbaloan_data$GrAppv,
        col=(c("gold","darkgreen")),
        main="Granting Bank Approval Amount Box Plot", xlab="US Dollars")


# Data shows significant outliers but domain knowledge does not indicate justification for removal.
# We would not want to not account for some of our largest loans in the model.

# 1 Hot 
encoded_sbaloan_data <- sbaloan_data %>% mutate(value = 1)  %>% spread(NewExist, value,  fill = 0 ) 
names(encoded_sbaloan_data)[names(encoded_sbaloan_data) == '0'] <- 'NewExist_0'
names(encoded_sbaloan_data)[names(encoded_sbaloan_data) == '1'] <- 'NewExist_1'
names(encoded_sbaloan_data)[names(encoded_sbaloan_data) == '2'] <- 'NewExist_2'
names(encoded_sbaloan_data)[names(encoded_sbaloan_data) == 'UNKNOWN'] <- 'NewExist_U'

encoded_sbaloan_data <- encoded_sbaloan_data %>% mutate(value = 1)  %>% spread(UrbanRural, value,  fill = 0 ) 
names(encoded_sbaloan_data)[names(encoded_sbaloan_data) == '0'] <- 'UrbanRural_0'
names(encoded_sbaloan_data)[names(encoded_sbaloan_data) == '1'] <- 'UrbanRural_1'
names(encoded_sbaloan_data)[names(encoded_sbaloan_data) == '2'] <- 'UrbanRural_2'

encoded_sbaloan_data <- encoded_sbaloan_data %>% mutate(value = 1)  %>% spread(Month, value,  fill = 0 ) 
names(encoded_sbaloan_data)[names(encoded_sbaloan_data) == 'Jan'] <- 'Month_Jan'
names(encoded_sbaloan_data)[names(encoded_sbaloan_data) == 'Feb'] <- 'Month_Feb'
names(encoded_sbaloan_data)[names(encoded_sbaloan_data) == 'Mar'] <- 'Month_Mar'
names(encoded_sbaloan_data)[names(encoded_sbaloan_data) == 'Apr'] <- 'Month_Apr'
names(encoded_sbaloan_data)[names(encoded_sbaloan_data) == 'May'] <- 'Month_May'
names(encoded_sbaloan_data)[names(encoded_sbaloan_data) == 'Jun'] <- 'Month_Jun'
names(encoded_sbaloan_data)[names(encoded_sbaloan_data) == 'Jul'] <- 'Month_Jul'
names(encoded_sbaloan_data)[names(encoded_sbaloan_data) == 'Aug'] <- 'Month_Aug'
names(encoded_sbaloan_data)[names(encoded_sbaloan_data) == 'Sep'] <- 'Month_Sep'
names(encoded_sbaloan_data)[names(encoded_sbaloan_data) == 'Oct'] <- 'Month_Oct'
names(encoded_sbaloan_data)[names(encoded_sbaloan_data) == 'Nov'] <- 'Month_Nov'
names(encoded_sbaloan_data)[names(encoded_sbaloan_data) == 'Dec'] <- 'Month_Dec'


# Create a new field that changes these to binary                                                 
encoded_sbaloan_data <- encoded_sbaloan_data %>%
        mutate(MIS_logical = ifelse( MIS_Status == "P I F",1,0))

# Bar Graph of Defaults to Paid

coul <- brewer.pal(5, "Set2")
barplot(table(encoded_sbaloan_data$MIS_logical),names.arg=c("Defaults", "Paid"), col=coul, main="SBA Loan Defaults Versus Paid", ylab="Count",ylim=c(0,900000))
box()
summary(encoded_sbaloan_data$MIS_logical)

# Label Encoding
sbaloan_data$Bank <- as.integer(sbaloan_data$Bank)
# sbaloan_data$Bank


# Binary Encoding when too many values 
# This code segment was taken from https://www.r-bloggers.com/2020/02/a-guide-to-encoding-categorical-features-using-r/
encode_binary <- function(x, order = unique(x), name = "v_") {
        x <- as.numeric(factor(x, levels = order, exclude = NULL))
        x2 <- as.binary(x)
        maxlen <- max(sapply(x2, length))
        x2 <- lapply(x2, function(y) {
                l <- length(y)
                if (l < maxlen) {
                        y <- c(rep(0, (maxlen - l)), y)
                }
                y
        })
        d <- as.data.frame(t(as.data.frame(x2)))
        rownames(d) <- NULL
        colnames(d) <- paste0(name, 1:maxlen)
        d
}

# Binary Encode calling function above
encoded_sbaloan_data <- cbind(encoded_sbaloan_data, encode_binary(encoded_sbaloan_data[["RevLineCr"]], name = "RevLineCr_"))
encoded_sbaloan_data <- cbind(encoded_sbaloan_data, encode_binary(encoded_sbaloan_data[["FranchiseCode"]], name = "FranchiseCode_"))
encoded_sbaloan_data <- cbind(encoded_sbaloan_data, encode_binary(encoded_sbaloan_data[["LowDoc"]], name = "LowDoc_"))
encoded_sbaloan_data <- cbind(encoded_sbaloan_data, encode_binary(encoded_sbaloan_data[["NAICS"]], name = "NAICS_"))
encoded_sbaloan_data <- cbind(encoded_sbaloan_data, encode_binary(encoded_sbaloan_data[["Bank"]], name = "Bank_"))

# Weak correlation and likely illegal for loan consideration so I am removing 
# encoded_sbaloan_data <- cbind(encoded_sbaloan_data, encode_binary(encoded_sbaloan_data[["BankState"]], name = "BankState_"))
# encoded_sbaloan_data <- cbind(encoded_sbaloan_data, encode_binary(encoded_sbaloan_data[["Zip"]], name = "Zip_"))

# Remove columns
encoded_sbaloan_data <- subset(encoded_sbaloan_data, select = -c(`LoanNr_ChkDgt`,`Zip`,`NAICS`,`FranchiseCode`,`RevLineCr`,`LowDoc`))
                

# Remove origial column
encoded_sbaloan_data <- subset(encoded_sbaloan_data, select = -c(`MIS_Status`))

# Remove Bank since it was enocded and Day as it was just a test and didn't help in later processing
encoded_sbaloan_data <- subset(encoded_sbaloan_data, select = -c(`Bank`,`Day`))
# encoded_sbaloan_data <- subset(encoded_sbaloan_data, select = -c(`Month`,`NewExist`))
# glimpse(encoded_sbaloan_data)
summary(encoded_sbaloan_data)

# Plot did not exhibit strong linear correlation
plot(encoded_sbaloan_data$NoEmp, encoded_sbaloan_data$CreateJob, main="Scatterplot Existing & Created Jobs",
     xlab="Existing Jobs", ylab="Created Jobs", pch=19)

# Plot did not exhibit strong linear correlation
plot(encoded_sbaloan_data$SBA_Appv, encoded_sbaloan_data$CreateJob, main="Scatterplot SBA Loan Amount & Created Jobs",
     xlab="SBA Loan Amount US Dollars not adjusted for current value", ylab="Created Jobs", pch=19)


# Spearman was selected for correlation analysis due to non-parametric data.
cor_matrix <- cor(encoded_sbaloan_data,method="spearman")  

# Commented out for length will summarize below
# cor_matrix
# eROUT_CIR <- corrplot(cor_matrix, method = "circle", title = "Circle Correlation Matrix Heat Map", mar=c(0,0,1,0), tl.cex=.75)

# Too much to look at, I want a quick scan of relevant values
# The following code does that 

# Code modified slightly from 
# https://towardsdatascience.com/how-to-create-a-correlation-matrix-with-too-many-variables-309cc0c0a57
# Look at highest values
corr_simple <- function(data=encoded_sbaloan_data,sig=0.25){
        #convert data to numeric in order to run correlations
        #convert to factor first to keep the integrity of the data - each value will 
        #become a number rather than turn into NA
        df_cor <- data %>% mutate_if(is.character, as.factor)
        df_cor <- df_cor %>% mutate_if(is.factor, as.numeric)
        #run a correlation and drop the insignificant ones
        corr <- cor(df_cor)
        #prepare to drop duplicates and correlations of 1     
        corr[lower.tri(corr,diag=TRUE)] <- NA 
        #drop perfect correlations
        corr[corr == 1] <- NA 
        #turn into a 3-column table
        corr <- as.data.frame(as.table(corr))
        #remove the NA values from above 
        corr <- na.omit(corr) 
        #select significant values  
        corr <- subset(corr, abs(Freq) > sig) 
        #sort by highest correlation
        corr <- corr[order(-abs(corr$Freq)),] 
        #print table
        print(corr)
        #turn corr back into matrix in order to plot with corrplot
        mtx_corr <- reshape2::acast(corr, Var1~Var2, value.var="Freq")
        
        #plot correlations visually
        corrplot(mtx_corr, is.corr=FALSE, tl.col="black", na.label=" ")
}
corr_simple()

# Term had the strongest correlation to loan repayment 
# None were noticeable for Jobs Created 
# Nothing significant for either



# Write out for python
write.csv(encoded_sbaloan_data,"sbapython.csv", row.names = FALSE)

summary(sbaloan_data$CreateJob)

