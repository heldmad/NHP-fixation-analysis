# Insert DV200 Analysis scripts here!

# Install Required Packages
install.packages(c("readxl", 
                   "agricolae", 
                   "car", 
                   "tidyverse", 
                   "lme4", 
                   "AICcmodavg", 
                   "sjPlot", 
                   "jtools", 
                   "betareg"))

# Load Required Packages
library(readxl)
library(agricolae)
library(car)
library(tidyverse)
library(lme4)
library(AICcmodavg)
library(sjPlot)
library(jtools)
library(betareg)

# Load in Sample Data and create/format groups for short-, long-term storage, and combined 
All_Samples_FixTests <- read_excel("/Users/heldm/Library/CloudStorage/OneDrive-SharedLibraries-OregonHealth&ScienceUniversity/PC.Conrad Lab - wet lab/Experiment Results/Conrad Bio Archive - CBA/CBA29 - MK27 Processing and NHP Fixative Test 2/All_Samples_FixTests.xlsx")

allsamples <-data.frame(All_Samples_FixTests)
allsamples$Date_Extracted = as.Date(allsamples$Date_Extracted)
allsamples$Hours_Fixed = as.character(allsamples$Hours_Fixed)
allsamples_short <- subset(allsamples, Date_Extracted == '2024-09-11' | Date_Extracted == '2024-10-01' | Date_Extracted == '2025-05-30' | Date_Extracted == '2025-07-31')
short <- allsamples_short
short <- short[short$Sample_ID != "B5" | short$Date_Extracted != "2024-10-01",]
short <- short[short$Sample_ID != "B6" | short$Date_Extracted != "2024-10-01",]
short <- short[short$Sample_ID != "B15" | short$Date_Extracted != "2024-10-01",]
short <- short[short$Sample_ID != "B16" | short$Date_Extracted != "2024-10-01",]
short$DV200_prop = short$DV200/100
short$Hours_Fixed_numeric <- as.numeric(short$Hours_Fixed)
short$Fixative=factor(short$Fixative,levels=c("PFA","mDF"))
short$Hours_Fixed = factor(short$Hours_Fixed, levels = c("2", "6", "12", "24", "48"))

long <- subset(allsamples, Storage_Time == "Long")
long$DV200_prop = long$DV200/100
long$Hours_Fixed_numeric <- as.numeric(long$Hours_Fixed)
long$Fixative = factor(long$Fixative, levels = c("PFA", "mDF"))

combined <- rbind(short, long)
combined$Storage_Time = factor(combined$Storage_Time, levels = c("Short", "Long"))
combined$Hours_Fixed = factor(combined$Hours_Fixed, levels = c("2", "6", "12", "24", "48"))

### Beta Regression Analysis for All Groups

## Short-term Storage
short %>% group_by(Fixative, Hours_Fixed_numeric) %>% summarise(mean=mean(DV200_prop)) %>% arrange(Fixative,Hours_Fixed_numeric)
betareg_short_m0 <- betareg(DV200_prop ~ 1, data = short)
betareg_short_m1 <- betareg(DV200_prop ~ Fixative, data = short)
betareg_short_m2 <- betareg(DV200_prop ~ Hours_Fixed_numeric, data = short)
betareg_short_m3 <- betareg(DV200_prop ~ Fixative * Hours_Fixed_numeric, data = short)
summary(betareg_short_m0)
summary(betareg_short_m1)
summary(betareg_short_m2)
summary(betareg_short_m3)
AIC(betareg_short_m0, betareg_short_m1, betareg_short_m2, betareg_short_m3)

# Create Lines of Best Fit for each fixative type
short_pfa <- short %>%
  filter(short$Fixative == "PFA")
short_mdf <- short %>%
  filter(short$Fixative == "mDF")
line_betareg_short_m3 <- predict(betareg_short_m3, short)
line_betareg_short_m3_pfa <- predict(betareg_short_m3, short_pfa)
line_betareg_short_m3_mDF <- predict(betareg_short_m3, short_mdf)

# Model Tabel (with p-values) for short-term beta regression
tab_model(betareg_short_m3, p.style = "scientific")
