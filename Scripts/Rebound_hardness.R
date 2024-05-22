################################################################################
# Title: Statistical analyses of Rebound hardness                              #  
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


load("../../../0_GIT_HUB/Djouroutou_lithic_landscapes_Reeves_et_al/Data/DJR_Workspace.Rdata")

## Rebound Hardness

# Test for normality 
rb_shap <- shapiro.test(djr_rbh$Hardness_Value) # Not Normal

# Kruskal Wallis
rb_kw_res <- kruskal.test(djr_rbh$Hardness_Value,djr_rbh$Material)

# Post Hoc Test

rb_ph_res <- FSA::dunnTest(djr_rbh$Hardness_Value,djr_rbh$Material)

