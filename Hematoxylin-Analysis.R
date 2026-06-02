# Install Required Packages
install.packages(c("tidyverse,
                   readxl",
                   "car",
                   "agricolae",
                   "lme4",
                   "AICcmodavg",
                   "sjPlot",
                   "jtools",
                   "betareg"))
# Load Required Packages
library(tidyverse)
library(readxl)
library(car)
library(agricolae)
library(lme4)
library(AICcmodavg)
library(sjPlot)
library(jtools)
library(betareg)


# Read in Data
hemaxl <- read_excel("/Users/heldm/Library/CloudStorage/OneDrive-SharedLibraries-OregonHealth&ScienceUniversity/PC.Conrad Lab - wet lab/Experiment Results/Histology Optimization & Troubleshooting - HOT/MMH NHP Fixation Project/NHP-fixative-text_PAS-H-pixelclassification.xlsx")
hemaxl <- as.data.frame(hemaxl)
hemaxl$hem_pos <- as.numeric(hemaxl$hem_pos)
hemaxl$hem_neg <- as.numeric(hemaxl$hem_neg)
hemaxl$Hours_Fixed <- as.numeric(hemaxl$Hours_Fixed)
hemaxl <- mutate(hemaxl, hem_pos_int = hem_pos/100, hem_neg_int = hem_neg/100)
hemaxl_avg <- hemaxl[hemaxl$Square_Number == "avg",]

## Assess Normality
# Create Test Condition Groups
mdf2 <- hemaxl[hemaxl$Fixative == "mDF" & hemaxl$Hours_Fixed == 2 & hemaxl$Square_Number != "avg",]
mdf6 <- hemaxl[hemaxl$Fixative == "mDF" & hemaxl$Hours_Fixed == 6 & hemaxl$Square_Number != "avg",]
mdf12 <- hemaxl[hemaxl$Fixative == "mDF" & hemaxl$Hours_Fixed == 12 & hemaxl$Square_Number != "avg",]
mdf24 <- hemaxl[hemaxl$Fixative == "mDF" & hemaxl$Hours_Fixed == 24 & hemaxl$Square_Number != "avg",]
mdf48 <- hemaxl[hemaxl$Fixative == "mDF" & hemaxl$Hours_Fixed == 48 & hemaxl$Square_Number != "avg",]
pfa2 <- hemaxl[hemaxl$Fixative == "PFA" & hemaxl$Hours_Fixed == 2 & hemaxl$Square_Number != "avg",]
pfa6 <- hemaxl[hemaxl$Fixative == "PFA" & hemaxl$Hours_Fixed == 6 & hemaxl$Square_Number != "avg",]
pfa12 <- hemaxl[hemaxl$Fixative == "PFA" & hemaxl$Hours_Fixed == 12 & hemaxl$Square_Number != "avg",]
pfa24 <- hemaxl[hemaxl$Fixative == "PFA" & hemaxl$Hours_Fixed == 24 & hemaxl$Square_Number != "avg",]
pfa48 <- hemaxl[hemaxl$Fixative == "PFA" & hemaxl$Hours_Fixed == 48 & hemaxl$Square_Number != "avg",]

# Shapiro Wilk Test to Assess Normality
mdf2pos <- shapiro.test(mdf2$hem_pos)
mdf2neg <- shapiro.test(mdf2$hem_neg)
c(mdf2pos, mdf2neg)

mdf6pos <- shapiro.test(mdf6$hem_pos)
mdf6neg <- shapiro.test(mdf6$hem_neg)
c(mdf6pos, mdf6neg)

mdf12pos <- shapiro.test(mdf12$hem_pos)
mdf12neg <- shapiro.test(mdf12$hem_neg)
c(mdf12pos, mdf12neg)

mdf24pos <- shapiro.test(mdf24$hem_pos)
mdf24neg <- shapiro.test(mdf24$hem_neg)
c(mdf24pos, mdf24neg)

mdf48pos <- shapiro.test(mdf48$hem_pos)
mdf48neg <- shapiro.test(mdf48$hem_neg)
c(mdf48pos, mdf48neg)

pfa2pos <- shapiro.test(pfa2$hem_pos)
pfa2neg <- shapiro.test(pfa2$hem_neg)
c(pfa2pos, pfa2neg)

pfa6pos <- shapiro.test(pfa6$hem_pos)
pfa6neg <- shapiro.test(pfa6$hem_neg)
c(pfa6pos, pfa6neg)

pfa12pos <- shapiro.test(pfa12$hem_pos)
pfa12neg <- shapiro.test(pfa12$hem_neg)
c(pfa12pos, pfa12neg)

pfa24pos <- shapiro.test(pfa24$hem_pos)
pfa24neg <- shapiro.test(pfa24$hem_neg)
c(pfa24pos, pfa24neg)

pfa48pos <- shapiro.test(pfa48$hem_pos)
pfa48neg <- shapiro.test(pfa48$hem_neg)
c(pfa48pos, pfa48neg)

# Levene Test to assess equality of variances - Not equally distributed; moving forward with beta regression
hemaxl <- mutate(hemaxl, Hours_Fixed_factor = as.factor(hemaxl$Hours_Fixed))
pos <- leveneTest(hem_pos ~ Hours_Fixed_factor * Fixative, data = hemaxl)
neg <- leveneTest(hem_neg ~ Hours_Fixed_factor * Fixative, data = hemaxl)
c(pos, neg)


# Run beta regression - positive hematoxylin stain
hemaxl_avg %>% 
  na.omit() %>% 
  group_by(Fixative, Hours_Fixed) %>% 
  summarise(mean=mean(hem_pos_int)) %>% 
  arrange(Fixative,Hours_Fixed)

hemaxl_br0 <- betareg(hem_pos_int ~ 1, data = hemaxl_avg)
summary(hemaxl_br0)

hemaxl_br1 <- betareg(hem_pos_int ~ Fixative, data = hemaxl_avg)
summary(hemaxl_br1)

hemaxl_br2 <- betareg(hem_pos_int ~ Hours_Fixed, data = hemaxl_avg)
summary(hemaxl_br2)

hemaxl_br3 <- betareg(hem_pos_int ~ Fixative * Hours_Fixed, data = hemaxl_avg)
summary(hemaxl_br3)

hemaxl_br4 <- betareg(hem_pos_int ~ Fixative + Hours_Fixed, data = hemaxl_avg)
summary(hemaxl_br4)

AIC(hemaxl_br0,
    hemaxl_br1,
    hemaxl_br2,
    hemaxl_br3,
    hemaxl_br4)

plot(hemaxl_br2)
plot(hemaxl_br4)

# Make Plot
pred <- predict(hemaxl_br4, hemaxl_avg)
View(pred)

ggplot(hemaxl_avg, 
       aes(x = Hours_Fixed, 
           y = hem_pos_int, 
           color = Fixative, 
           shape = Animal_Number)) + geom_point() + geom_line(data = data.frame(Fixative = c(hemaxl_avg$Fixative), 
                                                                                Hours_Fixed = c(hemaxl_avg$Hours_Fixed), 
                                                                                hem_pos_int = c(predict(hemaxl_br4, hemaxl_avg)), 
                                                                                Animal_Number = c(hemaxl_avg$Animal_Number), 
                                                                                linewidth = 5)) + ylim(0.5, 1) + theme_bw() + scale_color_brewer(palette = "Dark2") + labs(x = "Number of Hours Fixed", y = "Proportion of Hematoxylin-Positive Pixels per Sample", shape = "Animal Replicate ID", color = "Fixative Used")

                                                                                              