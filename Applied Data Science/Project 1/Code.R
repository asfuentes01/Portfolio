# *****************************************************************************************
# May 2022
# 
# ** PUBLIC-USE LINKED MORTALITY FOLLOW-UP THROUGH DECEMBER 31, 2019 **
#
# The following R code can be used to read the fixed-width format ASCII public-use Linked
# Mortality Files (LMFs) from a stored location into a R data frame.  Basic frequencies
# are also produced.  
# 
# NOTE:   With the exception of linkage eligibility-status (ELIGSTAT), the other discrete
#         variables (e.g., MORTSTAT) are designated as integers. We provide the definitions
#         of the variable values in the comments but leave it up to the user to decide 
#         whether integer or factor variables is/are preferred for their analyses.  
#
# NOTE:   As some variables are survey specific, we have created two versions of the program: 
#         one for NHIS and another for NHANES.
# 
# *****************************************************************************************   
#
# NOTE:   To download and save the public-use LMFs to your hard-drive, follow these steps:
# 
# (1)     Designate a folder on your hard-drive to download the public-use LMF.  In this example,
#         the data will be saved to "C:\PUBLIC USE DATA\".
#
# (2)     To download the public-use LMF, go to the website:  
#         https://ftp.cdc.gov/pub/Health_Statistics/NCHS/datalinkage/linked_mortality/
#
#         Right-click on the desired survey link and select "Save target as...".  A "Save As"
#         screen will appear where you will need to select and input a location where to
#         save the data file on your hard drive.  
#
#         Also note that the "Save as type:" box should read "DAT File (*.dat)".  This will ensure
#         that the data file is saved to your hard drive in the correct format.  
#
#         In this example, the data file is saved in the folder, "C:\PUBLIC USE DATA\", and the 
#         data file is saved as "<SURVEYNAME>_MORT_2019_PUBLIC.DAT". 
#
# *****************************************************************************************   
#
# R NOTES:
# (1)     For convenience, the user can place the name of the public-use LMF they are reading
#         in and creating as a R data frame in just two places: (1) the line beginning with
#         srvyin; and (2) the line beginning with srvyout. The resulting R data frame
#         will have the shorthand name assigned in the srvyout line.   
#
# (2)     Variable labels are not provided. Please see the public-use LMF codebook for 
#         this information.
#
# (3)     Variable value formats are not attached to the variables. The value formats, 
#         however, are included in comment blocks in the Variable Frequencies section.
#           
# *****************************************************************************************


#install.packages("readr")    #if package is not installed already, then uncomment and run this line
#install.packages("dplyr")    #if package is not installed already, then uncomment and run this line
library(readr)
library(dplyr)


# the location where the .DAT file is saved:
setwd("/Users/astrid/Documents/GitHub/DSC680/Project1/data")

# remove all objects from the R environment
rm(list=ls())


##############
#NHIS FILE VERSION#
##############

# 2009

srvyin <- "NHIS_2009_MORT_2019_PUBLIC.dat"  # full .DAT name here
srvyout <- "NHIS_2009" # shorthand dataset name here

# Example syntax:
#srvyin <- paste("NHIS_1986_MORT_2019_PUBLIC.dat")   
#srvyout <- "NHIS_1986"      


# read in the fixed-width format ASCII file
dsn <- read_fwf(file=srvyin,
                col_types = "ciiiiiiidd",
                fwf_cols(publicid = c(1,14),
                         eligstat = c(15,15),
                         mortstat = c(16,16),
                         ucod_leading = c(17,19),
                         diabetes = c(20,20),
                         hyperten = c(21,21),
                         dodqtr = c(22,22),
                         dodyear = c(23,26),
                         wgt_new = c(27,34),
                         sa_wgt_new = c(35,42)
                ),
                na = c("", ".")
)

# NOTE:   PUBLICID is the Unique ID for NHIS.

NHIS_2009['year'] = 2009


# Structure and contents of data
str(dsn)


# Re-name the dataset, DSN, to the short survey name then remove other R objects
assign(paste0(srvyout), dsn)
rm(dsn, srvyin, srvyout)


# Variable frequencies

#ELIGSTAT: Eligibility Status for Mortality Follow-up
table(dsn$eligstat)
#1 = "Eligible"
#2 = "Under age 18, not available for public release"
#3 = "Ineligible"

#MORTSTAT: Final Mortality Status
table(dsn$mortstat, useNA="ifany")
# 0 = Assumed alive
# 1 = Assumed deceased
# <NA> = Ineligible or under age 18

#UCOD_LEADING: Underlying Cause of Death: Recode
table(dsn$ucod_leading, useNA="ifany")
# 1 = Diseases of heart (I00-I09, I11, I13, I20-I51)
# 2 = Malignant neoplasms (C00-C97)
# 3 = Chronic lower respiratory diseases (J40-J47)
# 4 = Accidents (unintentional injuries) (V01-X59, Y85-Y86)
# 5 = Cerebrovascular diseases (I60-I69)
# 6 = Alzheimer's disease (G30)
# 7 = Diabetes mellitus (E10-E14)
# 8 = Influenza and pneumonia (J09-J18)
# 9 = Nephritis, nephrotic syndrome and nephrosis (N00-N07, N17-N19, N25-N27)
# 10 = All other causes (residual)
# <NA> = Ineligible, under age 18, assumed alive, or no cause of death data available

#DIABETES: Diabetes Flag from Multiple Cause of Death (MCOD)
table(dsn$diabetes, useNA="ifany")
# 0 = No - Condition not listed as a multiple cause of death
# 1 = Yes - Condition listed as a multiple cause of death
# <NA> = Assumed alive, under age 18, ineligible for mortality follow-up, or MCOD not available

#HYPERTEN: Hypertension Flag from Multiple Cause of Death (MCOD)
table(dsn$hyperten, useNA="ifany")
# 0 = No - Condition not listed as a multiple cause of death
# 1 = Yes - Condition listed as a multiple cause of death
# <NA> = Assumed alive, under age 18, ineligible for mortality follow-up, or MCOD not available

table(dsn$dodqtr, useNA="ifany")
# 1 = January-March
# 2 = April-June
# 3 = July-September
# 4 = October-December
# <NA> = Ineligible, under age 18, or assumed alive

table(dsn$dodyear, useNA="ifany")
# <NA> = Ineligible, under age 18, or assumed alive

# 2010

srvyin <- "NHIS_2010_MORT_2019_PUBLIC.dat"  # full .DAT name here
srvyout <- "NHIS_2010" # shorthand dataset name here

# Example syntax:
#srvyin <- paste("NHIS_1986_MORT_2019_PUBLIC.dat")   
#srvyout <- "NHIS_1986"      


# read in the fixed-width format ASCII file
dsn <- read_fwf(file=srvyin,
                col_types = "ciiiiiiidd",
                fwf_cols(publicid = c(1,14),
                         eligstat = c(15,15),
                         mortstat = c(16,16),
                         ucod_leading = c(17,19),
                         diabetes = c(20,20),
                         hyperten = c(21,21),
                         dodqtr = c(22,22),
                         dodyear = c(23,26),
                         wgt_new = c(27,34),
                         sa_wgt_new = c(35,42)
                ),
                na = c("", ".")
)

# NOTE:   PUBLICID is the Unique ID for NHIS.




# Structure and contents of data
str(dsn)


# Re-name the dataset, DSN, to the short survey name then remove other R objects
assign(paste0(srvyout), dsn)
rm(dsn, srvyin, srvyout)

NHIS_2010['year'] = 2010



# 2011

srvyin <- "NHIS_2011_MORT_2019_PUBLIC.dat"  # full .DAT name here
srvyout <- "NHIS_2011" # shorthand dataset name here

# Example syntax:
#srvyin <- paste("NHIS_1986_MORT_2019_PUBLIC.dat")   
#srvyout <- "NHIS_1986"      


# read in the fixed-width format ASCII file
dsn <- read_fwf(file=srvyin,
                col_types = "ciiiiiiidd",
                fwf_cols(publicid = c(1,14),
                         eligstat = c(15,15),
                         mortstat = c(16,16),
                         ucod_leading = c(17,19),
                         diabetes = c(20,20),
                         hyperten = c(21,21),
                         dodqtr = c(22,22),
                         dodyear = c(23,26),
                         wgt_new = c(27,34),
                         sa_wgt_new = c(35,42)
                ),
                na = c("", ".")
)

# NOTE:   PUBLICID is the Unique ID for NHIS.




# Structure and contents of data
str(dsn)


# Re-name the dataset, DSN, to the short survey name then remove other R objects
assign(paste0(srvyout), dsn)
rm(dsn, srvyin, srvyout)

NHIS_2011['year'] = 2011



# 2012

srvyin <- "NHIS_2012_MORT_2019_PUBLIC.dat"  # full .DAT name here
srvyout <- "NHIS_2012" # shorthand dataset name here

# Example syntax:
#srvyin <- paste("NHIS_1986_MORT_2019_PUBLIC.dat")   
#srvyout <- "NHIS_1986"      


# read in the fixed-width format ASCII file
dsn <- read_fwf(file=srvyin,
                col_types = "ciiiiiiidd",
                fwf_cols(publicid = c(1,14),
                         eligstat = c(15,15),
                         mortstat = c(16,16),
                         ucod_leading = c(17,19),
                         diabetes = c(20,20),
                         hyperten = c(21,21),
                         dodqtr = c(22,22),
                         dodyear = c(23,26),
                         wgt_new = c(27,34),
                         sa_wgt_new = c(35,42)
                ),
                na = c("", ".")
)

# NOTE:   PUBLICID is the Unique ID for NHIS.




# Structure and contents of data
str(dsn)


# Re-name the dataset, DSN, to the short survey name then remove other R objects
assign(paste0(srvyout), dsn)
rm(dsn, srvyin, srvyout)

NHIS_2012['year'] = 2012


# 2013

srvyin <- "NHIS_2013_MORT_2019_PUBLIC.dat"  # full .DAT name here
srvyout <- "NHIS_2013" # shorthand dataset name here

# Example syntax:
#srvyin <- paste("NHIS_1986_MORT_2019_PUBLIC.dat")   
#srvyout <- "NHIS_1986"      


# read in the fixed-width format ASCII file
dsn <- read_fwf(file=srvyin,
                col_types = "ciiiiiiidd",
                fwf_cols(publicid = c(1,14),
                         eligstat = c(15,15),
                         mortstat = c(16,16),
                         ucod_leading = c(17,19),
                         diabetes = c(20,20),
                         hyperten = c(21,21),
                         dodqtr = c(22,22),
                         dodyear = c(23,26),
                         wgt_new = c(27,34),
                         sa_wgt_new = c(35,42)
                ),
                na = c("", ".")
)

# NOTE:   PUBLICID is the Unique ID for NHIS.




# Structure and contents of data
str(dsn)


# Re-name the dataset, DSN, to the short survey name then remove other R objects
assign(paste0(srvyout), dsn)
rm(dsn, srvyin, srvyout)

NHIS_2013['year'] = 2013



# 2014

srvyin <- "NHIS_2014_MORT_2019_PUBLIC.dat"  # full .DAT name here
srvyout <- "NHIS_2014" # shorthand dataset name here

# Example syntax:
#srvyin <- paste("NHIS_1986_MORT_2019_PUBLIC.dat")   
#srvyout <- "NHIS_1986"      


# read in the fixed-width format ASCII file
dsn <- read_fwf(file=srvyin,
                col_types = "ciiiiiiidd",
                fwf_cols(publicid = c(1,14),
                         eligstat = c(15,15),
                         mortstat = c(16,16),
                         ucod_leading = c(17,19),
                         diabetes = c(20,20),
                         hyperten = c(21,21),
                         dodqtr = c(22,22),
                         dodyear = c(23,26),
                         wgt_new = c(27,34),
                         sa_wgt_new = c(35,42)
                ),
                na = c("", ".")
)

# NOTE:   PUBLICID is the Unique ID for NHIS.




# Structure and contents of data
str(dsn)


# Re-name the dataset, DSN, to the short survey name then remove other R objects
assign(paste0(srvyout), dsn)
rm(dsn, srvyin, srvyout)

NHIS_2014['year'] = 2014




data <- rbind (NHIS_2009, NHIS_2010, NHIS_2011, NHIS_2012, NHIS_2013, NHIS_2014)

head(data)

# full data model

model_1 <- lm(mortstat ~ eligstat + diabetes + hyperten + year, data = data)
summary(model_1)

library(coefplot)
coefplot(model_1)



hist(data$eligstat, main="Histogram of Elgibility Status", xlab = "Eligibility Status")
hist(data$diabetes, main="Histogram of Diabetic", xlab = "Diabetic")
hist(data$hyperten, main="Histogram of Diabetic", xlab = "Hypertense")
hist(data$mortstat, main="Histogram of Mortality", xlab = "Mortality")
hist(data$year, main="Histogram of Year", xlab = "Year")

# Remove non-numeric column so we can calculate correlation
num_data <- subset (data, select = -publicid)
cor(num_data[], method="pearson")

#Kaplan Meier Model

library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)

km <- with(data, Surv(year, mortstat))

km_fit <- survfit(Surv(year, mortstat) ~ 1, data=data)
summary(km_fit, times = c(2009,2010,2011,2012,2013,2014,2015*(1:10)))


# Binary Logistic Regression

str(data)

# Generating the frequency table
table(data$diabetes)
table(data$hyperten)
table(data$mortstat)

library(ggplot2)
ggplot(data, aes(year)) +
  geom_histogram(aes(fill = mortstat), color = "black", binwidth = 2)

ggplot(data, aes(year)) +
  geom_histogram(aes(fill = diabetes), color = "black", binwidth = 2)

ggplot(data, aes(year)) +
  geom_histogram(aes(fill = hyperten), color = "black", binwidth = 2)

#Building the Model

# Splitting the data into train and test
#make this example reproducible
set.seed(1)

#use 70% of dataset as training set and 30% as test set
sample <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.7,0.3))
train  <- data[sample, ]
test   <- data[!sample, ]

# Training the model
logistic_model <- glm(mortstat ~ ., family = binomial(), train)

data['mortstat'] = data['mortstat']-1
mylogit <- glm(mortstat ~ hyperten , data = train, family = "binomial")


# Checking the model
summary(logistic_model)
