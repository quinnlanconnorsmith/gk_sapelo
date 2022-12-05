library(lme4)
library(nlme)


mod1 <- lme(shannon_diversity~dist.to.strand..m. + specific_conductance + soil.moisture.pct + pct.OM, random = ~1 | Site, method = 'REML', data=model_df)
summary(mod1)
plot(mod1)


ctrl <- lmeControl(opt='optim')
mod2 <- lme(shannon_diversity~dist.to.strand..m. + specific_conductance + soil.moisture.pct + pct.OM, random = (~1+dist.to.strand..m. | Site), control=ctrl, method = 'ML', data=model_df)


gls_mod1 <- gls(shannon_diversity~dist.to.strand..m. + specific_conductance + soil.moisture.pct + pct.OM, data=model_df)
anova(gls_mod1, mod1)


#no do this 
mod3 <- lm(shannon_diversity~dist.to.strand..m. + specific_conductance + soil.moisture.pct + pct.OM + n_s,  data=model_df)


summary(mod3)
anova(mod1, mod2)          
