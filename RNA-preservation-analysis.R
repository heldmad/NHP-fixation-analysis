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

# Load in Sample Data
All_Samples_FixTests <- read_excel("/Users/heldm/Library/CloudStorage/OneDrive-SharedLibraries-OregonHealth&ScienceUniversity/PC.Conrad Lab - wet lab/Experiment Results/Conrad Bio Archive - CBA/CBA29 - MK27 Processing and NHP Fixative Test 2/All_Samples_FixTests.xlsx")
