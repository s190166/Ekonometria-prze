# 
library(plm)
panel <- pdata.frame(eu27_merged, index = c("ID", "rok"))

# Fixed Effects
fe_model <- plm(PPP ~ Electricity.prices + log(GDP) + Stopa.bezrobocia +
                  log(Średnie.zarobki) + Ślad.zużycia + Czas.w.unii.europejskiej,
                data = panel, model = "within")

summary(fe_model)

# Random Effects
re_model <- plm(PPP ~ Electricity.prices + log(GDP) + Stopa.bezrobocia +
                  log(Średnie.zarobki) + Ślad.zużycia + Czas.w.unii.europejskiej,
                data = panel, model = "random")

summary(re_model)

# Reszty z modelu FE
fe_resid <- residuals(fe_model)
moran.test(fe_resid, weights_list, zero.policy = TRUE)

# Reszty z modelu RE
re_resid <- residuals(re_model)
moran.test(re_resid, weights_list, zero.policy = TRUE)

# test 
phtest(fe_model, re_model)
