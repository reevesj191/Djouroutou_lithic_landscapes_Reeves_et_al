################################################################################
# Title: Density GLM                                                           #  
# Subtitle: Reeves et. al. (2024), In search of the earliest archaeological... #
# Author: Jonathan S. Reeves                                                   #  
# Last Updated: 22.05.24                                                       #  
# Summary: This code runs the GLMM and associated diagnostics for the density  #
# model. Unfortunately, you will not be able run some some of the diagnostics  #
# because I use code provided to me by Roger Mundry and I do not have the right#
# to publish it or make it public. If you wish to use these functions please   #
# write directly to Roger.                                                     #
#                                                                              #  
# As discussed in the paper each model revealed influential cases that we kept #
# in the model. This decision was based on the fact that their removal had no  #
# influence on the outcome of the model. The code on line 26 that is commented #
# will remove those cases from the dataset should you wish to replicate this   #
# result.                                                                      #
################################################################################
library(car)
library(MASS)
library(performance)

# Load data and functions 

source("../../Reeves_et_al_Djouroutou_Landscape/3. Analysis/Scripts/diagnostic_fcns.r")
load("Djouroutou_lithic_landscapes_Reeves_et_al/Data/DJR_Workspace.Rdata")
#xdata <- xdata[!(xdata$Square %in% c("PPD-49", "CDN-24", "CDD-45")),]


# Intial definition of poisson model
full_model <- glm(formula = n_stone_no_anvil~SPECIES + primary_rm + stone_anvil_present,
                  family = "poisson", data = xdata)

# Check for over dispersion
performance::check_overdispersion(full_model)

# Over dispersion is detect

full_model <- glm.nb(n_stone_no_anvil~SPECIES + primary_rm + stone_anvil_present,data = xdata)

performance::check_overdispersion(full_model)

library(car)

xx <- lm(n_stone_no_anvil~SPECIES + primary_rm + stone_anvil_present, data = xdata)

class(xdata)
vif(xx)

xdata$dense_influence <- influence(full_model)$hat
max(as.vector(influence(full_model)$hat))

lev.thresh(full_model)

cbind(coefficients(full_model), coefficients(full_model)+
        t(apply(X=dfbeta(full_model), MARGIN=2, FUN=range)))

null <- glm.nb(formula = n_stone_no_anvil~1, data = xdata,)

fn_res <- anova(null, full_model, test = "Chisq")
fn_res

xres <- summary(full_model)


drop1(full_model, test = "Chisq")


write.csv(coef(xres), file = "../6. Submission/Data/coef_N_stones.csv")
