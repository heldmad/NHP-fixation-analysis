# Install Required Packages
install.packages(c("tidyverse,
                   readxl"))
# Load Required Packages
library(tidyverse)
library(readxl)

# Read in Data
hemaxl <- read_excel("/Users/heldm/Library/CloudStorage/OneDrive-SharedLibraries-OregonHealth&ScienceUniversity/PC.Conrad Lab - wet lab/Experiment Results/Histology Optimization & Troubleshooting - HOT/MMH NHP Fixation Project/NHP-fixative-text_PAS-H-pixelclassification.xlsx")
hemaxl <- as.data.frame(hemaxl)
hemaxl$hem_pos <- as.numeric(hemaxl$hem_pos)
hemaxl$hem_neg <- as.numeric(hemaxl$hem_neg)
