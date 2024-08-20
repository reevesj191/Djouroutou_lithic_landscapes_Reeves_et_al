################################################################################
# Title: Statistical analyses of Rebound hardness                              #
# Subtitle: Reeves et. al. (2024), In search of the earliest archaeological... #
# Author: Jonathan S. Reeves                                                   #  
# Last Updated: 22.05.24                                                       #  
# Summary: This code runs the statistical analyses associated with the rebound #
# hardness data reported in the paper.                                         #
#                                                                              #                                                                   #
################################################################################


load("../../../0_GIT_HUB/Djouroutou_lithic_landscapes_Reeves_et_al/Data/DJR_Workspace.Rdata")

## Rebound Hardness

# Test for normality 
rb_shap <- shapiro.test(djr_rbh$Hardness_Value) # Not Normal

# Kruskal Wallis
rb_kw_res <- kruskal.test(djr_rbh$Hardness_Value,djr_rbh$Material)

# Post Hoc Test

rb_ph_res <- FSA::dunnTest(djr_rbh$Hardness_Value,djr_rbh$Material)

