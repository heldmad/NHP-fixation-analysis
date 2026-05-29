# Install Required Packages
install.packages(c("tidyverse,
                   readxl",
                   "car",
                   "modelbased",
                   "ggsignif",
                   "agricolae",
                   "lme4",
                   "AICcmodavg",
                   "sjPlot",
                   "jtools",
                   "betareg",
                   "ggeffects"))
# Load Required Packages
library(tidyverse)
library(readxl)
library(car)
library(modelbased)
library(ggsignif)
library(agricolae)
library(lme4)
library(AICcmodavg)
library(sjPlot)
library(jtools)
library(betareg)
library(ggeffects)

# Read in Data
hemaxl <- read_excel("/Users/heldm/Library/CloudStorage/OneDrive-SharedLibraries-OregonHealth&ScienceUniversity/PC.Conrad Lab - wet lab/Experiment Results/Histology Optimization & Troubleshooting - HOT/MMH NHP Fixation Project/NHP-fixative-text_PAS-H-pixelclassification.xlsx")
hemaxl <- as.data.frame(hemaxl)
hemaxl$hem_pos <- as.numeric(hemaxl$hem_pos)
hemaxl$hem_neg <- as.numeric(hemaxl$hem_neg)
hemaxl$Hours_Fixed <- as.numeric(hemaxl$Hours_Fixed)
hemaxl <- mutate(hemaxl, hem_pos_int = hem_pos/100, hem_neg_int = hem_neg/100)

## Assess Normality
# Create Test Condition Groups
mdf2 <- hemaxl[hemaxl$Fixative == "mDF" & hemaxl$Hours_Fixed == 2,]
mdf6 <- hemaxl[hemaxl$Fixative == "mDF" & hemaxl$Hours_Fixed == 6,]
mdf12 <- hemaxl[hemaxl$Fixative == "mDF" & hemaxl$Hours_Fixed == 12,]
mdf24 <- hemaxl[hemaxl$Fixative == "mDF" & hemaxl$Hours_Fixed == 24,]
mdf48 <- hemaxl[hemaxl$Fixative == "mDF" & hemaxl$Hours_Fixed == 48,]
pfa2 <- hemaxl[hemaxl$Fixative == "PFA" & hemaxl$Hours_Fixed == 2,]
pfa6 <- hemaxl[hemaxl$Fixative == "PFA" & hemaxl$Hours_Fixed == 6,]
pfa12 <- hemaxl[hemaxl$Fixative == "PFA" & hemaxl$Hours_Fixed == 12,]
pfa24 <- hemaxl[hemaxl$Fixative == "PFA" & hemaxl$Hours_Fixed == 24,]
pfa48 <- hemaxl[hemaxl$Fixative == "PFA" & hemaxl$Hours_Fixed == 48,]

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

# Levene Test to assess equality of variances
hemaxl <- mutate(hemaxl, Hours_Fixed_factor = as.factor(hemaxl$Hours_Fixed))
pos <- leveneTest(hem_pos ~ Hours_Fixed_factor * Fixative, data = hemaxl)
neg <- leveneTest(hem_neg ~ Hours_Fixed_factor * Fixative, data = hemaxl)

# Run 2-way ANOVA - positive hematoxylin stain
pos_result <- aov(hem_pos ~ Hours_Fixed * Fixative, data = hemaxl)
summary(pos_result)
marginal_means_fix <- estimate_means(pos_result, by = "Fixative")
marginal_means_fix
marginal_means_hours <- estimate_means(pos_result, by = "Hours_Fixed")
marginal_means_hours

# Run beta regression - positive hematoxylin stain
hemaxl %>% na.omit() %>% group_by(Fixative, Hours_Fixed) %>% summarise(mean=mean(hem_pos_int)) %>% arrange(Fixative,Hours_Fixed)
hemaxl_br0 <- betareg(hem_pos_int ~ 1, data = hemaxl)
summary(hemaxl_br0)

hemaxl_br1 <- betareg(hem_pos_int ~ Fixative, data = hemaxl)
summary(hemaxl_br1)

hemaxl_br2 <- betareg(hem_pos_int ~ Hours_Fixed, data = hemaxl)
summary(hemaxl_br2)

hemaxl_br3 <- betareg(hem_pos_int ~ Fixative * Hours_Fixed, data = hemaxl)
summary(hemaxl_br3)

hemaxl_br4 <- betareg(hem_pos_int ~ Fixative + Hours_Fixed, data = hemaxl)
summary(hemaxl_br4)

AIC(hemaxl_br0,
    hemaxl_br1,
    hemaxl_br2,
    hemaxl_br3,
    hemaxl_br4)

plot(hemaxl_br2)
plot(hemaxl_br4)

# Make Plot
pred <- ggpredict(hemaxl_br4, terms = c("Fixative", "Hours_Fixed"))
plot(pred)

ggplot(hemaxl, aes(x = Hours_Fixed, y = hem_pos_int, fill = Fixative)) + geom_violin(aes(group = Hours_Fixed)) + geom_smooth(data = data.frame(Fixative = c(hemaxl$Fixative), 
                                                                                                 Hours_Fixed = c(hemaxl$Hours_Fixed), 
                                                                                                 hem_pos_int = c(predict(hemaxl_br4, hemaxl)))) + facet_wrap(facets = vars(Fixative))
                                                                            
                                                                                              