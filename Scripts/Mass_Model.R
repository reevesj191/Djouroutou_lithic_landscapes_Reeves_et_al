################################################################################
# Title: Max Weight GLM                                                        #  
# Subtitle: Reeves et. al. (2024), In search of the earliest archaeological... #
# Author: Jonathan S. Reeves                                                   #  
# Last Updated: 22.05.24                                                       #  
# Summary: This code runs the GLMM and associated diagnostics for the weight   #
# model. Unfortunately, you will not be able run some some of the diagnostics  #
# because I use code provided to me by Roger Mundry and I do not have the right#
# to publish it or make it public. If you wish to use these functions please   #
# write directly to Roger.                                                     #
#                                                                              #  
# As discussed in the paper each model revealed influential cases that we kept #
# in the model. This decision was based on the fact that their removal had no  #
# influence on the outcome of the model. The code on line 24 that is commented #
# will remove those cases from the data set should you wish to replicate this  #
# result.                                                                      #
################################################################################


source("3. Analysis/Scripts/diagnostic_fcns.r")
load("Djouroutou_lithic_landscapes_Reeves_et_al/Data/DJR_Workspace.Rdata")
xdata <- xdata[!(xdata$Square %in% c("PrQL-42")),] # This site is excluded as it is only represented by a single anvil

#xdata <- xdata[!(xdata$Square %in% c( "PPD-49", "CDN-24", "CDD-45")),] 

full_model <- glm(formula= max_weight~SPECIES + primary_rm + stone_anvil_present, 
                  family = "gaussian", data = xdata)


xx <- lm(max_weight~SPECIES + primary_rm + stone_anvil_present, data = xdata)
vif(xx)



### Model Stability: influence and leverage
xdata$dense_influence <- influence(full_model)$hat
max(as.vector(influence(full_model)$hat))

lev.thresh(full_model)

### Check DFBeta

cbind(coefficients(full_model), coefficients(full_model)+
        t(apply(X=dfbeta(full_model), MARGIN=2, FUN=range)))



### Full-Null model comparison


null <- glm(formula = max_weight~1, family = "gaussian", data = xdata[xdata$Square != "PrQL-42",])

fn_res <- anova(null, full_model, test = "Chisq")
fn_res

### Summary of Model


xres <- summary(full_model)
write.csv(coef(xres), file = "../6. Submission/Data/coef_Stone_size.csv")


### Significance of individual variables

drop1(full_model, test = "Chisq")

