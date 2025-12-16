##################################################################
#####The False Promise Of Political Culture models and graphs#####
##################################################################

# Load libraries and data from the other script/alternatively download and load the final datasets from github

#######################
##H1: GENERAL CONTEXT##
#######################

# Bivariate OLS regressions, clustered by ISO3

# Main outcome: V-Dem democratic mobilization
H1_SupDem_bivariate1 <- feols(v2cademmob ~ SupDem_trim,
                             cluster = ~ ISO3,
                             data = panel_macro_autocratization)
summary(H1_SupDem_bivariate1)

# Main outcome: NAVCO any_nonviolent
H1_SupDem_bivariate2 <- feols(any_nonviolent ~ SupDem_trim,
                              cluster = ~ ISO3,
                              data = panel_macro_autocratization)
summary(H1_SupDem_bivariate2)

# Main outcome: V-Dem democratic mobilization
H1_EV_bivariate1 <- feols(v2cademmob ~ Emancipative_valuesWIK2015,
                              cluster = ~ ISO3,
                              data = panel_macro_autocratization)
summary(H1_EV_bivariate1)

# Main outcome: NAVCO any_nonviolent
H1_EV_bivariate2 <- feols(any_nonviolent ~ Emancipative_valuesWIK2015,
                              cluster = ~ ISO3,
                              data = panel_macro_autocratization)
summary(H1_EV_bivariate2)

# One-way Fixed effects on ISO3, bivariate, clustered by ISO3

# Main outcome: V-Dem democratic mobilization
H1_SupDem_FE1 <- feols(v2cademmob ~ SupDem_trim | ISO3,
  cluster = ~ ISO3,
  data = panel_macro_autocratization)

summary(H1_SupDem_FE1)

# Main outcome: NAVCO any_nonviolent
H1_SupDem_FE2 <- feols(any_nonviolent ~ SupDem_trim | ISO3,
  cluster = ~ ISO3,
  data = panel_macro_autocratization)

summary(H1_SupDem_FE2)

# Main outcome: V-Dem democratic mobilization
H1_EV_FE1 <- feols(v2cademmob ~ Emancipative_valuesWIK2015 | ISO3,
  cluster = ~ ISO3,
  data = panel_macro_autocratization)

summary(H1_EV_FE1)

# Main outcome: NAVCO any_nonviolent
H1_EV_FE2 <- feols(any_nonviolent ~ Emancipative_valuesWIK2015 | ISO3,
  cluster = ~ ISO3,
  data = panel_macro_autocratization)

summary(H1_EV_FE2)

# Two-way FE: ISO3 + Year, bivariate, clustered by ISO3

# Main outcome: V-Dem democratic mobilization 
H1_SupDem_FE3 <- feols(v2cademmob ~ SupDem_trim | ISO3 + Year,
  cluster = ~ ISO3,
  data = panel_macro_autocratization)

summary(H1_SupDem_FE3)

# Main outcome: NAVCO any_nonviolent 
H1_SupDem_FE4 <- feols(any_nonviolent ~ SupDem_trim | ISO3 + Year,
  cluster = ~ ISO3,
  data = panel_macro_autocratization)

summary(H1_SupDem_FE4)

# Main outcome: V-Dem democratic mobilization
H1_EV_FE3 <- feols(v2cademmob ~ Emancipative_valuesWIK2015 | ISO3 + Year,
  cluster = ~ ISO3,
  data = panel_macro_autocratization)

summary(H1_EV_FE3)

# Main outcome: NAVCO any_nonviolent 
H1_EV_FE4 <- feols(any_nonviolent ~ Emancipative_valuesWIK2015 | ISO3 + Year,
  cluster = ~ ISO3,
  data = panel_macro_autocratization)

summary(H1_EV_FE4)

# Full model specifications, two-way FE: ISO3 + Year, clustered by ISO3

# Main outcome: V-Dem democratic mobilization
H1_SupDem_full1 <- feols(v2cademmob ~ SupDem_trim + log_gdpcapcon2015 + e_peaveduc + wdi_popurb + v2x_polyarchy |
              ISO3 + Year, cluster = ~ ISO3, data = panel_macro_autocratization)

summary(H1_SupDem_full1)

# NAVCO any_nonviolent
H1_SupDem_full2 <- feols(any_nonviolent ~ SupDem_trim + log_gdpcapcon2015 + e_peaveduc + wdi_popurb + v2x_polyarchy |
              ISO3 + Year, cluster = ~ ISO3, data = panel_macro_autocratization)

summary(H1_SupDem_full2)

# Main outcome: V-Dem democratic mobilization
H1_EV_full1 <- feols(v2cademmob ~ Emancipative_valuesWIK2015 + log_gdpcapcon2015 + e_peaveduc + wdi_popurb + v2x_polyarchy |
          ISO3 + Year, cluster = ~ ISO3, data = panel_macro_autocratization)

summary(H1_EV_full1)

# NAVCO any_nonviolent
H1_EV_full2 <- feols(any_nonviolent ~ Emancipative_valuesWIK2015 + log_gdpcapcon2015 + e_peaveduc + wdi_popurb + v2x_polyarchy |
          ISO3 + Year, cluster = ~ ISO3, data = panel_macro_autocratization)

summary(H1_EV_full2)

# Non-linear models

# Main outcome: V-Dem democratic mobilization
H1_SupDem_nonlinear1 <- feols(v2cademmob ~ SupDem_trim + I(SupDem_trim^2) +
    log_gdpcapcon2015 + e_peaveduc + wdi_popurb + v2x_polyarchy |
    ISO3 + Year,
  cluster = ~ISO3,
  data = panel_macro_autocratization)

summary(H1_SupDem_nonlinear1)

# NAVCO any_nonviolent
H1_SupDem_nonlinear2 <- feols(any_nonviolent ~ SupDem_trim + I(SupDem_trim^2) +
    log_gdpcapcon2015 + e_peaveduc + wdi_popurb + v2x_polyarchy |
    ISO3 + Year,
  cluster = ~ISO3,
  data = panel_macro_autocratization)

summary(H1_SupDem_nonlinear2)

# Main outcome: V-Dem democratic mobilization
H1_EV_nonlinear1 <- feols(v2cademmob ~ Emancipative_valuesWIK2015 + I(Emancipative_valuesWIK2015^2) +
    log_gdpcapcon2015 + e_peaveduc + wdi_popurb + v2x_polyarchy |
    ISO3 + Year,
  cluster = ~ISO3,
  data = panel_macro_autocratization)

summary(H1_EV_nonlinear1)

# NAVCO any_nonviolent
H1_EV_nonlinear2 <- feols(any_nonviolent ~ Emancipative_valuesWIK2015 + I(Emancipative_valuesWIK2015^2) +
    log_gdpcapcon2015 + e_peaveduc + wdi_popurb + v2x_polyarchy |
    ISO3 + Year,
  cluster = ~ISO3,
  data = panel_macro_autocratization)

summary(H1_EV_nonlinear2)

##############################
##H2: AUTOCRATIZING EPISODES##
##############################

# Support for democracy

# AUTOCRATIZATION SUBSETS (EDI) Pelke & Croissant 2021

# Main outcome: V-Dem democratic mobilization

# 0.05 subset
H2_m1 <- feols(v2cademmob ~ SupDem_trim + log_gdpcapcon2015 + e_peaveduc + wdi_popurb |
              ISO3 + Year, cluster = ~ ISO3, data = panel_autocratization_005)

# 0.10 subset
H2_m2 <- feols(v2cademmob ~ SupDem_trim + log_gdpcapcon2015 + e_peaveduc + wdi_popurb |
              ISO3 + Year, cluster = ~ ISO3, data = panel_autocratization_01)

# 0.10 + CI subset
H2_m3 <- feols(v2cademmob ~ SupDem_trim + log_gdpcapcon2015 + e_peaveduc + wdi_popurb |
              ISO3 + Year, cluster = ~ ISO3, data = panel_autocratization_01ci)

# Main outcome: NAVCO any_nonviolent

# 0.05 subset
H2_m4 <- feols(any_nonviolent ~ SupDem_trim + log_gdpcapcon2015 + e_peaveduc + wdi_popurb |
                 ISO3 + Year, cluster = ~ ISO3, data = panel_autocratization_005)

# 0.10 subset
H2_m5 <- feols(any_nonviolent ~ SupDem_trim + log_gdpcapcon2015 + e_peaveduc + wdi_popurb |
                 ISO3 + Year, cluster = ~ ISO3, data = panel_autocratization_01)

# 0.10 + CI subset
H2_m6 <- feols(any_nonviolent ~ SupDem_trim + log_gdpcapcon2015 + e_peaveduc + wdi_popurb |
                 ISO3 + Year, cluster = ~ ISO3, data = panel_autocratization_01ci)

summary(H2_m1)
summary(H2_m2)
summary(H2_m3)
summary(H2_m4)
summary(H2_m5)
summary(H2_m6)

# INTERACTIONS (EDI THRESHOLDS) Pelke & Croissant 2021

# Main outcome: V-Dem democratic mobilization
H2_m7 <- feols(v2cademmob ~ SupDem_trim * auto_period01 + log_gdpcapcon2015 + e_peaveduc + wdi_popurb |
                ISO3 + Year, cluster = ~ ISO3, data = panel_macro_autocratization)

H2_m8 <- feols(v2cademmob ~ SupDem_trim * auto_period005 + log_gdpcapcon2015 + e_peaveduc + wdi_popurb |
               ISO3 + Year, cluster = ~ ISO3, data = panel_macro_autocratization)

H2_m9 <- feols(v2cademmob ~ SupDem_trim * auto_period01ci + log_gdpcapcon2015 + e_peaveduc + wdi_popurb |
              ISO3 + Year, cluster = ~ ISO3, data = panel_macro_autocratization)

# Main outcome: NAVCO any_nonviolent
H2_m10 <- feols(any_nonviolent ~ SupDem_trim * auto_period01 + log_gdpcapcon2015 + e_peaveduc + wdi_popurb |
               ISO3 + Year, cluster = ~ ISO3, data = panel_macro_autocratization)

H2_m11 <- feols(any_nonviolent ~ SupDem_trim * auto_period005 + log_gdpcapcon2015 + e_peaveduc + wdi_popurb |
               ISO3 + Year, cluster = ~ ISO3, data = panel_macro_autocratization)

H2_m12 <- feols(any_nonviolent ~ SupDem_trim * auto_period01ci + log_gdpcapcon2015 + e_peaveduc + wdi_popurb |
               ISO3 + Year, cluster = ~ ISO3, data = panel_macro_autocratization)

summary(H2_m7)
summary(H2_m8)
summary(H2_m9)
summary(H2_m10)
summary(H2_m11)
summary(H2_m12)

# INTERACTIONS (ERT V-Dem)

# Main outcome: V-Dem democratic mobilization
H2_m13 <- feols(v2cademmob ~ SupDem_trim * ert_aut_ep + log_gdpcapcon2015 + e_peaveduc + wdi_popurb |
               ISO3 + Year, cluster = ~ ISO3, data = panel_macro_autocratization)

# Main outcome: NAVCO any_nonviolent
H2_m14 <- feols(any_nonviolent ~ SupDem_trim * ert_aut_ep + log_gdpcapcon2015 + e_peaveduc + wdi_popurb |
               ISO3 + Year, cluster = ~ ISO3, data = panel_macro_autocratization)

summary(H2_m13)
summary(H2_m14)

# Emancipative values 

# AUTOCRATIZATION SUBSETS (EDI) Pelke & Croissant 2021

# Main outcome: V-Dem democratic mobilization

# 0.05 subset
H2_m1ev <- feols(v2cademmob ~ Emancipative_valuesWIK2015 + log_gdpcapcon2015 + e_peaveduc + wdi_popurb |
                 ISO3 + Year, cluster = ~ ISO3, data = panel_autocratization_005)

# 0.10 subset
H2_m2ev <- feols(v2cademmob ~ Emancipative_valuesWIK2015 + log_gdpcapcon2015 + e_peaveduc + wdi_popurb |
                 ISO3 + Year, cluster = ~ ISO3, data = panel_autocratization_01)

# 0.10 + CI subset
H2_m3ev <- feols(v2cademmob ~ Emancipative_valuesWIK2015 + log_gdpcapcon2015 + e_peaveduc + wdi_popurb |
                 ISO3 + Year, cluster = ~ ISO3, data = panel_autocratization_01ci)

# Main outcome: NAVCO any_nonviolent

# 0.05 subset
H2_m4ev <- feols(any_nonviolent ~ Emancipative_valuesWIK2015 + log_gdpcapcon2015 + e_peaveduc + wdi_popurb |
                 ISO3 + Year, cluster = ~ ISO3, data = panel_autocratization_005)

# 0.10 subset
H2_m5ev <- feols(any_nonviolent ~ Emancipative_valuesWIK2015 + log_gdpcapcon2015 + e_peaveduc + wdi_popurb |
                 ISO3 + Year, cluster = ~ ISO3, data = panel_autocratization_01)

# 0.10 + CI subset
H2_m6ev <- feols(any_nonviolent ~ Emancipative_valuesWIK2015 + log_gdpcapcon2015 + e_peaveduc + wdi_popurb |
                 ISO3 + Year, cluster = ~ ISO3, data = panel_autocratization_01ci)

summary(H2_m1ev)
summary(H2_m2ev)
summary(H2_m3ev)
summary(H2_m4ev)
summary(H2_m5ev)
summary(H2_m6ev)

# INTERACTIONS (EDI THRESHOLDS) Pelke & Croissant 2021

# Main outcome: V-Dem democratic mobilization
H2_m7ev <- feols(v2cademmob ~ Emancipative_valuesWIK2015 * auto_period01 + log_gdpcapcon2015 + e_peaveduc + wdi_popurb |
                   ISO3 + Year, cluster = ~ ISO3, data = panel_macro_autocratization)

H2_m8ev <- feols(v2cademmob ~ Emancipative_valuesWIK2015 * auto_period005 + log_gdpcapcon2015 + e_peaveduc + wdi_popurb |
                  ISO3 + Year, cluster = ~ ISO3, data = panel_macro_autocratization)

H2_m9ev <- feols(v2cademmob ~ Emancipative_valuesWIK2015 * auto_period01ci + log_gdpcapcon2015 + e_peaveduc + wdi_popurb |
                 ISO3 + Year, cluster = ~ ISO3, data = panel_macro_autocratization)

# Main outcome: NAVCO any_nonviolent
H2_m10ev <- feols(any_nonviolent ~ Emancipative_valuesWIK2015 * auto_period01 + log_gdpcapcon2015 + e_peaveduc + wdi_popurb |
                  ISO3 + Year, cluster = ~ ISO3, data = panel_macro_autocratization)

H2_m11ev <- feols(any_nonviolent ~ Emancipative_valuesWIK2015 * auto_period005 + log_gdpcapcon2015 + e_peaveduc + wdi_popurb |
                  ISO3 + Year, cluster = ~ ISO3, data = panel_macro_autocratization)

H2_m12ev <- feols(any_nonviolent ~ Emancipative_valuesWIK2015 * auto_period01ci + log_gdpcapcon2015 + e_peaveduc + wdi_popurb |
                  ISO3 + Year, cluster = ~ ISO3, data = panel_macro_autocratization)

summary(H2_m7ev)
summary(H2_m8ev)
summary(H2_m9ev)
summary(H2_m10ev)
summary(H2_m11ev)
summary(H2_m12ev)

# INTERACTIONS (ERT V-Dem)

# Main outcome: V-Dem democratic mobilization
H2_m13ev <- feols(v2cademmob ~ Emancipative_valuesWIK2015 * ert_aut_ep + log_gdpcapcon2015 + e_peaveduc + wdi_popurb |
                  ISO3 + Year, cluster = ~ ISO3, data = panel_macro_autocratization)

# Main outcome: NAVCO any_nonviolent
H2_m14ev <- feols(any_nonviolent ~ Emancipative_valuesWIK2015 * ert_aut_ep + log_gdpcapcon2015 + e_peaveduc + wdi_popurb |
                  ISO3 + Year, cluster = ~ ISO3, data = panel_macro_autocratization)

summary(H2_m13ev)
summary(H2_m14ev)

##########################
##H3: AUTOCRATIC CONTEXT##
##########################

# Support for democracy

# AUTOCRATIC CONTEXT - INTERACTIONS (EDI and Boix)

# Main outcome: V-Dem democratic mobilization
H3_m15 <- feols(v2cademmob ~ SupDem_trim * edi_autocracy + log_gdpcapcon2015 + e_peaveduc + wdi_popurb |
                 ISO3 + Year, cluster = ~ ISO3, data = panel_macro_autocratization)

H3_m16 <- feols(v2cademmob ~ SupDem_trim * e_boix_regime + log_gdpcapcon2015 + e_peaveduc + wdi_popurb |
                 ISO3 + Year, cluster = ~ ISO3, data = panel_macro_autocratization)

# Main outcome: NAVCO any_nonviolent
H3_m17 <- feols(any_nonviolent ~ SupDem_trim * edi_autocracy + log_gdpcapcon2015 + e_peaveduc + wdi_popurb |
                 ISO3 + Year, cluster = ~ ISO3, data = panel_macro_autocratization)

H3_m18 <- feols(any_nonviolent ~ SupDem_trim * e_boix_regime + log_gdpcapcon2015 + e_peaveduc + wdi_popurb |
                 ISO3 + Year, cluster = ~ ISO3, data = panel_macro_autocratization)

# AUTOCRATIC CONTEXT - SUBSET (Boix)

# Main outcome: V-Dem democratic mobilization
H3_m19 <- feols(v2cademmob ~ SupDem_trim + log_gdpcapcon2015 + e_peaveduc + wdi_popurb |
                 ISO3 + Year, cluster = ~ ISO3, data = panel_autocratic)

# Main outcome: NAVCO any_nonviolent
H3_m20 <- feols(any_nonviolent ~ SupDem_trim + log_gdpcapcon2015 + e_peaveduc + wdi_popurb |
                 ISO3 + Year, cluster = ~ ISO3, data = panel_autocratic)

summary(H3_m15)
summary(H3_m16)
summary(H3_m17)
summary(H3_m18)
summary(H3_m19)
summary(H3_m20)

# Emancipative values

# AUTOCRATIC CONTEXT - INTERACTIONS (EDI and Boix)

# Main outcome: V-Dem democratic mobilization
H3_m15ev <- feols(v2cademmob ~ Emancipative_valuesWIK2015 * edi_autocracy + log_gdpcapcon2015 + e_peaveduc + wdi_popurb |
                   ISO3 + Year, cluster = ~ ISO3, data = panel_macro_autocratization)

H3_m16ev <- feols(v2cademmob ~ Emancipative_valuesWIK2015 * e_boix_regime + log_gdpcapcon2015 + e_peaveduc + wdi_popurb |
                   ISO3 + Year, cluster = ~ ISO3, data = panel_macro_autocratization)

# Main outcome: NAVCO any_nonviolent
H3_m17ev <- feols(any_nonviolent ~ Emancipative_valuesWIK2015 * edi_autocracy + log_gdpcapcon2015 + e_peaveduc + wdi_popurb |
                   ISO3 + Year, cluster = ~ ISO3, data = panel_macro_autocratization)

H3_m18ev <- feols(any_nonviolent ~ Emancipative_valuesWIK2015 * e_boix_regime + log_gdpcapcon2015 + e_peaveduc + wdi_popurb |
                   ISO3 + Year, cluster = ~ ISO3, data = panel_macro_autocratization)

# AUTOCRATIC CONTEXT - SUBSET (Boix)

# Main outcome: V-Dem democratic mobilization
H3_m19ev <- feols(v2cademmob ~ Emancipative_valuesWIK2015 + log_gdpcapcon2015 + e_peaveduc + wdi_popurb |
                   ISO3 + Year, cluster = ~ ISO3, data = panel_autocratic)

# Main outcome: NAVCO any_nonviolent
H3_m20ev <- feols(any_nonviolent ~ Emancipative_valuesWIK2015 + log_gdpcapcon2015 + e_peaveduc + wdi_popurb |
                   ISO3 + Year, cluster = ~ ISO3, data = panel_autocratic)

summary(H3_m15ev)
summary(H3_m16ev) 
summary(H3_m17ev)
summary(H3_m18ev)
summary(H3_m19ev)
summary(H3_m20ev)

# Stricter test for models that yielded expected results
H3_m15ev_more_controls <- feols(v2cademmob ~ Emancipative_valuesWIK2015 * edi_autocracy + log_gdpcapcon2015 + e_peaveduc + wdi_popurb + v2x_civlib + v2x_execorr + v2xeg_eqdr |
                      ISO3 + Year, cluster = ~ ISO3, data = panel_macro_autocratization)

H3_m17ev_more_controls <- feols(any_nonviolent ~ Emancipative_valuesWIK2015 * edi_autocracy + log_gdpcapcon2015 + e_peaveduc + wdi_popurb + v2x_civlib + v2x_execorr + v2xeg_eqdr |
                      ISO3 + Year, cluster = ~ ISO3, data = panel_macro_autocratization)

H3_m18ev_more_controls <- feols(any_nonviolent ~ Emancipative_valuesWIK2015 * e_boix_regime + log_gdpcapcon2015 + e_peaveduc + wdi_popurb + v2x_civlib + v2x_execorr + v2xeg_eqdr  |
                      ISO3 + Year, cluster = ~ ISO3, data = panel_macro_autocratization)

summary(H3_m15ev_more_controls)
summary(H3_m17ev_more_controls)
summary(H3_m18ev_more_controls)

# Outcome: V-Dem mobilization for democracy
ab_model1 <- pgmm(v2cademmob ~ lag(v2cademmob, 1) + Emancipative_valuesWIK2015*edi_autocracy +
    log_gdpcapcon2015 + e_peaveduc + wdi_popurb +
    v2x_civlib + v2x_execorr + v2xeg_eqdr |
    lag(v2cademmob, 2:5),
  data = panel_macro_autocratization,
  effect = "individual",
  model = "twosteps",
  transformation = "d")

summary(ab_model1, robust = TRUE)

ab_model_tight1 <- pgmm(v2cademmob ~ lag(v2cademmob, 1) + Emancipative_valuesWIK2015*edi_autocracy +
    log_gdpcapcon2015 + e_peaveduc + wdi_popurb +
    v2x_civlib + v2x_execorr + v2xeg_eqdr |
    lag(v2cademmob, 2:3),
  data = panel_macro_autocratization,
  effect = "individual",
  model = "twosteps",
  transformation = "d",
  collapse = TRUE)

summary(ab_model_tight1, robust = TRUE)

# Outcome: any_nonviolent
ab_model2 <- pgmm(any_nonviolent ~ lag(any_nonviolent, 1) + Emancipative_valuesWIK2015*edi_autocracy +
    log_gdpcapcon2015 + e_peaveduc + wdi_popurb +
    v2x_civlib + v2x_execorr + v2xeg_eqdr |
    lag(any_nonviolent, 2:5),
  data = panel_macro_autocratization,
  effect = "individual",
  model = "twosteps",
  transformation = "d")

summary(ab_model2, robust = TRUE)

ab_model_tight2 <- pgmm(any_nonviolent ~ lag(any_nonviolent, 1) + Emancipative_valuesWIK2015*edi_autocracy +
    log_gdpcapcon2015 + e_peaveduc + wdi_popurb +
    v2x_civlib + v2x_execorr + v2xeg_eqdr |
    lag(any_nonviolent, 2:3),
  data = panel_macro_autocratization,
  effect = "individual",
  model = "twosteps",
  transformation = "d",
  collapse = TRUE)

summary(ab_model_tight2, robust = TRUE)

# Outcome: any_nonviolent
ab_model3 <- pgmm(any_nonviolent ~ lag(any_nonviolent, 1) + Emancipative_valuesWIK2015*e_boix_regime +
    log_gdpcapcon2015 + e_peaveduc + wdi_popurb +
    v2x_civlib + v2x_execorr + v2xeg_eqdr |
    lag(any_nonviolent, 2:5),
  data = panel_macro_autocratization,
  effect = "individual",
  model = "twosteps",
  transformation = "d")

summary(ab_model3, robust = TRUE)

ab_model_tight3 <- pgmm(any_nonviolent ~ lag(any_nonviolent, 1) + Emancipative_valuesWIK2015*e_boix_regime +
    log_gdpcapcon2015 + e_peaveduc + wdi_popurb +
    v2x_civlib + v2x_execorr + v2xeg_eqdr |
    lag(any_nonviolent, 2:3),
  data = panel_macro_autocratization,
  effect = "individual",
  model = "twosteps",
  transformation = "d",
  collapse = TRUE)

summary(ab_model_tight3, robust = TRUE)

#############
##VIF TESTS##
#############

# VIF versions of the full H1 models
H1_SupDem_full1_vif <- lm(v2cademmob ~ SupDem_trim + log_gdpcapcon2015 + e_peaveduc + wdi_popurb + v2x_polyarchy,
  data = panel_macro_autocratization)

H1_SupDem_full2_vif <- lm(any_nonviolent ~ SupDem_trim + log_gdpcapcon2015 + e_peaveduc + wdi_popurb + v2x_polyarchy,
  data = panel_macro_autocratization)

H1_EV_full1_vif <- lm(v2cademmob ~ Emancipative_valuesWIK2015 + log_gdpcapcon2015 + e_peaveduc + wdi_popurb + v2x_polyarchy,
  data = panel_macro_autocratization)

H1_EV_full2_vif <- lm(any_nonviolent ~ Emancipative_valuesWIK2015 + log_gdpcapcon2015 + e_peaveduc + wdi_popurb + v2x_polyarchy,
  data = panel_macro_autocratization)

# Create a list for table formation
models_vif <- list(H1_SupDem_full1 = H1_SupDem_full1_vif, 
                   H1_SupDem_full2 = H1_SupDem_full2_vif, 
                   H1_EV_full1 = H1_EV_full1_vif, 
                   H1_EV_full2 = H1_EV_full2_vif)

# Make df
vif_df <- imap_dfr(models_vif, ~ tibble(
  Model = .y,
  Variable = names(vif(.x)),
  VIF = as.numeric(vif(.x))))

print(vif_df)

# Tidy workspace
remove(models_vif, vif_df, H1_EV_full1_vif, H1_EV_full2_vif, H1_SupDem_full1_vif, H1_SupDem_full2_vif)

################
##Hausman test##
################
# Model 1: V-Dem democratic mobilization
fe1 <- plm(v2cademmob ~ SupDem_trim + log_gdpcapcon2015 + 
             e_peaveduc + wdi_popurb + v2x_polyarchy,
           data = panel_macro_autocratization, model = "within")

# Model 2: NAVCO any_nonviolent
fe2 <- plm(any_nonviolent ~ SupDem_trim + log_gdpcapcon2015 + 
             e_peaveduc + wdi_popurb + v2x_polyarchy,
           data = panel_macro_autocratization, model = "within")

# Model 1: Random effects
re1 <- plm(v2cademmob ~ SupDem_trim + log_gdpcapcon2015 + 
             e_peaveduc + wdi_popurb + v2x_polyarchy,
           data = panel_macro_autocratization, model = "random")

# Model 2: Random effects
re2 <- plm(any_nonviolent ~ SupDem_trim + log_gdpcapcon2015 + 
             e_peaveduc + wdi_popurb + v2x_polyarchy,
           data = panel_macro_autocratization, model = "random")

# Hausman test for Model 1
phtest(fe1, re1)

# Hausman test for Model 2
phtest(fe2, re2)

# Tidy
remove(re1, re2, fe1, fe2)

#################################################
##Visualizing nonlinear and interaction effects##
#################################################

##H1 nonlinear plots (appendix)##

# Controlling for country-year similar to FE but in another format for visualization purposes
H1_SupDem_modelling1 <- feols(v2cademmob ~ SupDem_trim + I(SupDem_trim^2) +
                                log_gdpcapcon2015 + e_peaveduc + wdi_popurb + v2x_polyarchy +
                                factor(ISO3) + factor(Year),
                              cluster = ~ISO3, data = panel_macro_autocratization)

H1_SupDem_modelling2 <- feols(any_nonviolent ~ SupDem_trim + I(SupDem_trim^2) +
                                log_gdpcapcon2015 + e_peaveduc + wdi_popurb + v2x_polyarchy +
                                factor(ISO3) + factor(Year),
                              cluster = ~ISO3, data = panel_macro_autocratization)

H1_Emancipative_modelling1 <- feols(v2cademmob ~ Emancipative_valuesWIK2015 + I(Emancipative_valuesWIK2015^2) +
                                      log_gdpcapcon2015 + e_peaveduc + wdi_popurb + v2x_polyarchy +
                                      factor(ISO3) + factor(Year),
                                    cluster = ~ISO3, data = panel_macro_autocratization)

H1_Emancipative_modelling2 <- feols(any_nonviolent ~ Emancipative_valuesWIK2015 + I(Emancipative_valuesWIK2015^2) +
                                      log_gdpcapcon2015 + e_peaveduc + wdi_popurb + v2x_polyarchy +
                                      factor(ISO3) + factor(Year),
                                    cluster = ~ISO3, data = panel_macro_autocratization)

Nonlinear_plot <- function(model, xvar, xlab, ylab) {
  pred <- plot_predictions(
    model,
    condition = xvar,
    grid = 100,
    interval = TRUE,
    draw = FALSE) 
  
  ggplot(pred, aes_string(x = xvar, y = "estimate")) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.18) +
    geom_line(linewidth = 0.7) +
    labs(x = xlab, y = ylab) +
    theme_bw(base_size = 9) +
    theme(
      panel.grid.major = element_line(linetype = "dotted", linewidth = 0.25, colour = "grey80"),
      panel.grid.minor = element_blank())}

plot_SupDem_modelling1 <- Nonlinear_plot(
  H1_SupDem_modelling1,
  "SupDem_trim",
  "Support for democracy",
  "Mobilization for democracy")

plot_SupDem_modelling2 <- Nonlinear_plot(
  H1_SupDem_modelling2,
  "SupDem_trim",
  "Support for democracy",
  "Nonviolent campaign")

plot_Emancipative_modelling1 <- Nonlinear_plot(
  H1_Emancipative_modelling1,
  "Emancipative_valuesWIK2015",
  "Emancipative values",
  "Mobilization for democracy")

plot_Emancipative_modelling2 <- Nonlinear_plot(
  H1_Emancipative_modelling2,
  "Emancipative_valuesWIK2015",
  "Emancipative values",
  "Nonviolent campaign")

plot_SupDem_modelling1 + plot_SupDem_modelling2 + plot_Emancipative_modelling1 + plot_Emancipative_modelling2

# Tidy workspace
remove(H1_SupDem_modelling1, H1_SupDem_modelling2,
       H1_Emancipative_modelling1, H1_Emancipative_modelling2,
       plot_SupDem_modelling1, plot_SupDem_modelling2,
       plot_Emancipative_modelling1, plot_Emancipative_modelling2, Nonlinear_plot)

##Interaction plots for autocratizing episodes and autocratic rule (H2-H3)##

##H2##

# Four models from H2, using the 01 measure of autocratizing episodes

# Make data frame to reduce errors and allow for plotting more consistently
plotting_df <- as.data.frame(panel_macro_autocratization)

# Model 1
H2_m7_plot <- feols(
  v2cademmob ~ SupDem_trim * auto_period01 +
    log_gdpcapcon2015 + e_peaveduc + wdi_popurb +
    factor(ISO3) + factor(Year),
  cluster = ~ ISO3,
  data = plotting_df)

# Marginal effects
me_df_H2_m7 <- slopes(
  H2_m7_plot,
  variables = "SupDem_trim",
  newdata = datagrid(auto_period01 = c(0, 1)))

me_plot_H2_m7 <- ggplot(
  me_df_H2_m7,
  aes(x = factor(auto_period01),
      y = estimate,
      color = factor(auto_period01))) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1) +
  scale_color_manual(
    values = c("0" = "steelblue", "1" = "red"),
    labels = c("0" = "Not autocratizing episode", "1" = "Autocratizing episode"),
    name = "Episode type") +
  labs(
    x = "Autocratizing episode",
    y = "Marginal Effect of Public support for democracy") +
  theme_bw(base_size = 8) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom")

# Predicted values
pred_df_H2_m7 <- predictions(
  H2_m7_plot,
  newdata = datagrid(
    SupDem_trim = seq(
      min(plotting_df$SupDem_trim, na.rm = TRUE),
      max(plotting_df$SupDem_trim, na.rm = TRUE),
      length.out = 100),
    auto_period01 = c(0, 1)))

pred_plot_H2_m7 <- ggplot(
  pred_df_H2_m7,
  aes(x = SupDem_trim,
      color = as.factor(auto_period01),
      fill  = as.factor(auto_period01))) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.1, color = NA) +
  geom_line(aes(y = estimate), linewidth = 0.8) +
  scale_color_manual(
    values = c("0" = "steelblue", "1" = "red"),
    labels = c("Not autocratizing episode", "Autocratizing episode"),
    name  = NULL) +
  scale_fill_manual(
    values = c("0" = "steelblue", "1" = "red"),
    labels = c("Not autocratizing episode", "Autocratizing episode"),
    guide = "none") +
  labs(
    x = "Public support for democracy",
    y = "Predicted mobilization for democracy") +
  theme_bw(base_size = 8) +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank())

# Model 2
H2_m10_plot <- feols(
  any_nonviolent ~ SupDem_trim * auto_period01 +
    log_gdpcapcon2015 + e_peaveduc + wdi_popurb +
    factor(ISO3) + factor(Year),
  cluster = ~ ISO3,
  data = plotting_df)

# Marginal effects
me_df_H2_m10 <- slopes(
  H2_m10_plot,
  variables = "SupDem_trim",
  newdata = datagrid(auto_period01 = c(0, 1)))

me_plot_H2_m10 <- ggplot(
  me_df_H2_m10,
  aes(x = factor(auto_period01),
      y = estimate,
      color = factor(auto_period01))) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1) +
  scale_color_manual(
    values = c("0" = "steelblue", "1" = "red"),
    labels = c("0" = "Not autocratizing episode", "1" = "Autocratizing episode"),
    name = "Episode type") +
  labs(
    x = "Autocratizing episode",
    y = "Marginal Effect of Public support for democracy") +
  theme_bw(base_size = 8) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom")

# Predicted values
pred_df_H2_m10 <- predictions(
  H2_m10_plot,
  newdata = datagrid(
    SupDem_trim = seq(
      min(plotting_df$SupDem_trim, na.rm = TRUE),
      max(plotting_df$SupDem_trim, na.rm = TRUE),
      length.out = 100), 
    auto_period01 = c(0, 1)))

pred_plot_H2_m10 <- ggplot(
  pred_df_H2_m10,
  aes(x = SupDem_trim,
      color = as.factor(auto_period01),
      fill  = as.factor(auto_period01))) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.1, color = NA) +
  geom_line(aes(y = estimate), linewidth = 0.8) +
  scale_color_manual(
    values = c("0" = "steelblue", "1" = "red"),
    labels = c("Not autocratizing episode", "Autocratizing episode"),
    name = NULL) +
  scale_fill_manual(
    values = c("0" = "steelblue", "1" = "red"),
    labels = c("Not autocratizing episode", "Autocratizing episode"),
    guide = "none") +
  labs(
    x = "Public support for democracy",
    y = "Predicted probability of nonviolent campaign") +
  theme_bw(base_size = 8) +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank())

# Model 3
H2_m7ev_plot <- feols(
  v2cademmob ~ Emancipative_valuesWIK2015 * auto_period01 +
    log_gdpcapcon2015 + e_peaveduc + wdi_popurb +
    factor(ISO3) + factor(Year),
  cluster = ~ ISO3,
  data = plotting_df)

# Marginal effects
me_df_H2_m7ev <- slopes(
  H2_m7ev_plot,
  variables = "Emancipative_valuesWIK2015",
  newdata = datagrid(auto_period01 = c(0, 1)))

me_plot_H2_m7ev <- ggplot(
  me_df_H2_m7ev,
  aes(x = factor(auto_period01),
      y = estimate,
      color = factor(auto_period01))) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1) +
  scale_color_manual(
    values = c("0" = "steelblue", "1" = "red"),
    labels = c("0" = "Not autocratizing episode", "1" = "Autocratizing episode"),
    name = "Episode type") +
  labs(
    x = "Autocratizing episode",
    y = "Marginal Effect of Emancipative values") +
  theme_bw(base_size = 8) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom")

# Predicted values
pred_df_H2_m7ev <- predictions(
  H2_m7ev_plot,
  newdata = datagrid(
    Emancipative_valuesWIK2015 = seq(
      min(plotting_df$Emancipative_valuesWIK2015, na.rm = TRUE),
      max(plotting_df$Emancipative_valuesWIK2015, na.rm = TRUE),
      length.out = 100),
    auto_period01 = c(0, 1)))

pred_plot_H2_m7ev <- ggplot(
  pred_df_H2_m7ev,
  aes(x = Emancipative_valuesWIK2015,
      color = as.factor(auto_period01),
      fill  = as.factor(auto_period01))) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.1, color = NA) +
  geom_line(aes(y = estimate), linewidth = 0.8) +
  scale_color_manual(
    values = c("0" = "steelblue", "1" = "red"),
    labels = c("Not autocratizing episode", "Autocratizing episode"),
    name = NULL) +
  scale_fill_manual(
    values = c("0" = "steelblue", "1" = "red"),
    labels = c("Not autocratizing episode", "Autocratizing episode"),
    guide = "none") +
  labs(
    x = "Emancipative values",
    y = "Predicted mobilization for democracy") +
  theme_bw(base_size = 8) +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank())

# Model 4
H2_m10ev_plot <- feols(
  any_nonviolent ~ Emancipative_valuesWIK2015 * auto_period01 +
    log_gdpcapcon2015 + e_peaveduc + wdi_popurb +
    factor(ISO3) + factor(Year),
  cluster = ~ ISO3,
  data = plotting_df)

# Marginal effects
me_df_H2_m10ev <- slopes(
  H2_m10ev_plot,
  variables = "Emancipative_valuesWIK2015",
  newdata = datagrid(auto_period01 = c(0, 1)))

me_plot_H2_m10ev <- ggplot(
  me_df_H2_m10ev,
  aes(x = factor(auto_period01),
      y = estimate,
      color = factor(auto_period01))) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1) +
  scale_color_manual(
    values = c("0" = "steelblue", "1" = "red"),
    labels = c("0" = "Not autocratizing episode", "1" = "Autocratizing episode"),
    name = "Episode type") +
  labs(
    x = "Autocratizing episode",
    y = "Marginal Effect of Emancipative values") +
  theme_bw(base_size = 8) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom")

# Predicted values
pred_df_H2_m10ev <- predictions(
  H2_m10ev_plot,
  newdata = datagrid(
    Emancipative_valuesWIK2015 = seq(
      min(plotting_df$Emancipative_valuesWIK2015, na.rm = TRUE),
      max(plotting_df$Emancipative_valuesWIK2015, na.rm = TRUE),
      length.out = 100),
    auto_period01 = c(0, 1)))

pred_plot_H2_m10ev <- ggplot(
  pred_df_H2_m10ev,
  aes(x = Emancipative_valuesWIK2015,
      y = estimate,
      color = as.factor(auto_period01),
      fill = as.factor(auto_period01))) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.1, color = NA) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(
    values = c("0" = "steelblue", "1" = "red"),
    labels = c("Not autocratizing episode", "Autocratizing episode"),
    name = NULL) +
  scale_fill_manual(
    values = c("0" = "steelblue", "1" = "red"),
    labels = c("Not autocratizing episode", "Autocratizing episode"),
    guide = "none") +
  labs(
    x = "Emancipative values",
    y = "Predicted probability of nonviolent campaign") +
  theme_bw(base_size = 8) +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank())

# Mobilization for democracy
me_plot_H2_m7 + pred_plot_H2_m7 + me_plot_H2_m7ev + pred_plot_H2_m7ev

# Nonviolent campaign
me_plot_H2_m10 + pred_plot_H2_m10 + me_plot_H2_m10ev + pred_plot_H2_m10ev

# Tidy workspace
remove(plotting_df, H2_m7_plot, me_df_H2_m7, me_plot_H2_m7, pred_df_H2_m7, pred_plot_H2_m7,
  H2_m10_plot, me_df_H2_m10, me_plot_H2_m10, pred_df_H2_m10, pred_plot_H2_m10,
  H2_m7ev_plot, me_df_H2_m7ev, me_plot_H2_m7ev, pred_df_H2_m7ev, pred_plot_H2_m7ev,
  H2_m10ev_plot, me_df_H2_m10ev, me_plot_H2_m10ev, pred_df_H2_m10ev, pred_plot_H2_m10ev)

##H3##

# Make data frame to reduce errors and allow for plotting more consistently
plotting_df <- as.data.frame(panel_macro_autocratization)

# H3_15
H3_m15_plot <- feols(
  v2cademmob ~ SupDem_trim * edi_autocracy +
    log_gdpcapcon2015 + e_peaveduc + wdi_popurb +
    factor(ISO3) + factor(Year),
  cluster = ~ ISO3,
  data = plotting_df)

marginal_effects_15 <- slopes(
  H3_m15_plot,
  variables = "SupDem_trim",
  newdata = datagrid(
    edi_autocracy = seq(
      min(plotting_df$edi_autocracy, na.rm = TRUE),
      max(plotting_df$edi_autocracy, na.rm = TRUE),
      length.out = 100)))

marginal_effects_plot_15 <- ggplot(marginal_effects_15,
                     aes(x = edi_autocracy, y = estimate)) +
  geom_line(color = "black", linewidth = 0.5) +
  geom_line(aes(y = conf.low), linetype = "dashed", color = "black", linewidth = 0.3) +
  geom_line(aes(y = conf.high), linetype = "dashed", color = "black", linewidth = 0.3) +
  geom_rug(data = plotting_df,
           aes(x = edi_autocracy),
           sides = "b",          
           alpha = 0.2,          
           linewidth = 0.1,     
           inherit.aes = FALSE) +
  geom_hline(yintercept = 0, color = "grey20") +
  labs(x = "Polyarchy reversed (autocracy index)",
       y = "Marginal Effect of Public support for democracy on mobilization for democracy") +
  theme_classic(base_size = 9) +
  theme(panel.grid = element_blank())

# H3_16
H3_m16_plot <- feols(
  v2cademmob ~ SupDem_trim * e_boix_regime +
    log_gdpcapcon2015 + e_peaveduc + wdi_popurb +
    factor(ISO3) + factor(Year),
  cluster = ~ ISO3,
  data = plotting_df)

marginal_effects_16 <- slopes(
  H3_m16_plot,
  variables = "SupDem_trim",
  newdata = datagrid(e_boix_regime = c(0, 1)))

marginal_effects_plot_16 <- ggplot(
  marginal_effects_16,
  aes(x = factor(e_boix_regime),
      y = estimate,
      color = factor(e_boix_regime))) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1) +
  scale_color_manual(
    values = c("0" = "steelblue", "1" = "red"),
    labels = c("Not autocratic", "Autocratic"),
    name = "Regime type") +
  labs(
    x = "Regime Type",
    y = "Marginal Effect of Public support for democracy on mobilization for democracy") +
  theme_bw(base_size = 9) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom")

# H3_17
H3_m17_plot <- feols(
  any_nonviolent ~ SupDem_trim * edi_autocracy +
    log_gdpcapcon2015 + e_peaveduc + wdi_popurb +
    factor(ISO3) + factor(Year),
  cluster = ~ ISO3,
  data = plotting_df)

marginal_effects_17 <- slopes(
  H3_m17_plot,
  variables = "SupDem_trim",
  newdata = datagrid(
    edi_autocracy = seq(
      min(plotting_df$edi_autocracy, na.rm = TRUE),
      max(plotting_df$edi_autocracy, na.rm = TRUE),
      length.out = 100)))

marginal_effects_plot_17 <- ggplot(marginal_effects_17,
                                   aes(x = edi_autocracy, y = estimate)) +
  geom_line(color = "black", linewidth = 0.5) +
  geom_line(aes(y = conf.low), linetype = "dashed", color = "black", linewidth = 0.3) +
  geom_line(aes(y = conf.high), linetype = "dashed", color = "black", linewidth = 0.3) +
  geom_rug(data = plotting_df,
           aes(x = edi_autocracy),
           sides = "b",          
           alpha = 0.2,          
           linewidth = 0.1,     
           inherit.aes = FALSE) +
  geom_hline(yintercept = 0, color = "grey20") +
  labs(x = "Polyarchy reversed (autocracy index)",
       y = "Marginal Effect of Public support for democracy on nonviolent campaign") +
  theme_classic(base_size = 9) +
  theme(panel.grid = element_blank())

# H3_18
H3_m18_plot <- feols(
  any_nonviolent ~ SupDem_trim * e_boix_regime +
    log_gdpcapcon2015 + e_peaveduc + wdi_popurb +
    factor(ISO3) + factor(Year),
  cluster = ~ ISO3,
  data = plotting_df)

marginal_effects_18 <- slopes(
  H3_m18_plot,
  variables = "SupDem_trim",
  newdata = datagrid(e_boix_regime = c(0, 1)))

marginal_effects_plot_18 <- ggplot(
  marginal_effects_18,
  aes(x = factor(e_boix_regime),
      y = estimate,
      color = factor(e_boix_regime))) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1) +
  scale_color_manual(
    values = c("0" = "steelblue", "1" = "red"),
    labels = c("Not autocratic", "Autocratic"),
    name = "Regime type") +
  labs(
    x = "Regime Type",
    y = "Marginal Effect of Public support for democracy on nonviolent campaign") +
  theme_bw(base_size = 9) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom")

# H3_m15ev
H3_m15ev_plot <- feols(
  v2cademmob ~ Emancipative_valuesWIK2015 * edi_autocracy +
    log_gdpcapcon2015 + e_peaveduc + wdi_popurb +
    factor(ISO3) + factor(Year),
  cluster = ~ ISO3,
  data = plotting_df)

marginal_effects_15ev <- slopes(
  H3_m15ev_plot,
  variables = "Emancipative_valuesWIK2015",
  newdata = datagrid(
    edi_autocracy = seq(
      min(plotting_df$edi_autocracy, na.rm = TRUE),
      max(plotting_df$edi_autocracy, na.rm = TRUE),
      length.out = 100)))

marginal_effects_plot_15ev <- ggplot(marginal_effects_15ev,
                                   aes(x = edi_autocracy, y = estimate)) +
  geom_line(color = "black", linewidth = 0.5) +
  geom_line(aes(y = conf.low), linetype = "dashed", color = "black", linewidth = 0.3) +
  geom_line(aes(y = conf.high), linetype = "dashed", color = "black", linewidth = 0.3) +
  geom_rug(data = plotting_df,
           aes(x = edi_autocracy),
           sides = "b",          
           alpha = 0.2,          
           linewidth = 0.1,     
           inherit.aes = FALSE) +
  geom_hline(yintercept = 0, color = "grey20") +
  labs(x = "Polyarchy reversed (autocracy index)",
       y = "Marginal Effect of Emancipative values on mobilization for democracy") +
  theme_classic(base_size = 9) +
  theme(panel.grid = element_blank())

# H3_m16ev
H3_m16ev_plot <- feols(
  v2cademmob ~ Emancipative_valuesWIK2015 * e_boix_regime +
    log_gdpcapcon2015 + e_peaveduc + wdi_popurb +
    factor(ISO3) + factor(Year),
  cluster = ~ ISO3,
  data = plotting_df)

marginal_effects_16ev <- slopes(
  H3_m16ev_plot,
  variables = "Emancipative_valuesWIK2015",
  newdata = datagrid(e_boix_regime = c(0, 1)))

marginal_effects_plot_16ev <- ggplot(
  marginal_effects_16ev,
  aes(x = factor(e_boix_regime),
      y = estimate,
      color = factor(e_boix_regime))) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1) +
  scale_color_manual(
    values = c("0" = "steelblue", "1" = "red"),
    labels = c("Not autocratic", "Autocratic"),
    name = "Regime type") +
  labs(
    x = "Regime Type",
    y = "Marginal Effect of Emancipative values on mobilization for democracy") +
  theme_bw(base_size = 9) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom")

# H3_m17ev
H3_m17ev_plot <- feols(
  any_nonviolent ~ Emancipative_valuesWIK2015 * edi_autocracy +
    log_gdpcapcon2015 + e_peaveduc + wdi_popurb +
    factor(ISO3) + factor(Year),
  cluster = ~ ISO3,
  data = plotting_df)

marginal_effects_17ev <- slopes(
  H3_m17ev_plot,
  variables = "Emancipative_valuesWIK2015",
  newdata = datagrid(
    edi_autocracy = seq(
      min(plotting_df$edi_autocracy, na.rm = TRUE),
      max(plotting_df$edi_autocracy, na.rm = TRUE),
      length.out = 100)))

marginal_effects_plot_17ev <- ggplot(marginal_effects_17ev,
                                   aes(x = edi_autocracy, y = estimate)) +
  geom_line(color = "black", linewidth = 0.5) +
  geom_line(aes(y = conf.low), linetype = "dashed", color = "black", linewidth = 0.3) +
  geom_line(aes(y = conf.high), linetype = "dashed", color = "black", linewidth = 0.3) +
  geom_rug(data = plotting_df,
           aes(x = edi_autocracy),
           sides = "b",          
           alpha = 0.2,          
           linewidth = 0.1,     
           inherit.aes = FALSE) +
  geom_hline(yintercept = 0, color = "grey20") +
  labs(x = "Polyarchy reversed (autocracy index)",
       y = "Marginal Effect of Emancipative values on nonviolent campaign") +
  theme_classic(base_size = 9) +
  theme(panel.grid = element_blank())

# H3_m18ev
H3_m18ev_plot <- feols(
  any_nonviolent ~ Emancipative_valuesWIK2015 * e_boix_regime +
    log_gdpcapcon2015 + e_peaveduc + wdi_popurb +
    factor(ISO3) + factor(Year),
  cluster = ~ ISO3,
  data = plotting_df)

marginal_effects_18ev <- slopes(
  H3_m18ev_plot,
  variables = "Emancipative_valuesWIK2015",
  newdata = datagrid(e_boix_regime = c(0, 1)))

marginal_effects_plot_18ev <- ggplot(
  marginal_effects_18ev,
  aes(x = factor(e_boix_regime),
      y = estimate,
      color = factor(e_boix_regime))) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1) +
  scale_color_manual(
    values = c("0" = "steelblue", "1" = "red"),
    labels = c("Not autocratic", "Autocratic"),
    name = "Regime type") +
  labs(
    x = "Regime Type",
    y = "Marginal Effect of Emancipative values on nonviolent campaign") +
  theme_bw(base_size = 9) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom")

# Print
marginal_effects_plot_15 + marginal_effects_plot_17

marginal_effects_plot_16 + marginal_effects_plot_18

marginal_effects_plot_15ev + marginal_effects_plot_17ev

marginal_effects_plot_16ev + marginal_effects_plot_18ev

remove(H3_m15_plot, H3_m16_plot, H3_m17_plot, H3_m18_plot, H3_m15ev_plot, H3_m16ev_plot, H3_m17ev_plot,
       H3_m18ev_plot, marginal_effects_15, marginal_effects_15ev, marginal_effects_16, marginal_effects_16ev,
       marginal_effects_17, marginal_effects_17ev, marginal_effects_18, marginal_effects_18ev,
       marginal_effects_plot_15, marginal_effects_plot_15ev, marginal_effects_plot_16,
       marginal_effects_plot_16ev, marginal_effects_plot_17, marginal_effects_plot_17ev,
       marginal_effects_plot_18, marginal_effects_plot_18ev, plotting_df)

##Significant models with more controls (graphs not in the thesis)##

# Make data frame to reduce errors and allow for plotting more consistently
plotting_df <- as.data.frame(panel_macro_autocratization)

# Binary model
H3_m18ev_more_controls_plot <- feols(
  any_nonviolent ~ Emancipative_valuesWIK2015 * e_boix_regime +
    log_gdpcapcon2015 + e_peaveduc + wdi_popurb +
    v2x_civlib + v2x_execorr + v2xeg_eqdr +
    factor(ISO3) + factor(Year),
  cluster = ~ ISO3,
  data = plotting_df)

# Marginal effects
me_df_18 <- slopes(
  H3_m18ev_more_controls_plot,
  variables = "Emancipative_valuesWIK2015",
  newdata = datagrid(e_boix_regime = c(0, 1)))

me_plot_18 <- ggplot(
  me_df_18,
  aes(x = factor(e_boix_regime),
      y = estimate,
      color = factor(e_boix_regime))) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1) +
  scale_color_manual(
    values = c("0" = "steelblue", "1" = "red"),
    labels = c("Not autocratic", "Autocratic"),
    name = "Regime type") +
  labs(
    x = "Regime Type",
    y = "Marginal Effect of Emancipative values") +
  theme_bw(base_size = 9) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom")

# Predicted values
pred_df_18 <- predictions(
  H3_m18ev_more_controls_plot,
  newdata = datagrid(
    Emancipative_valuesWIK2015 = seq(
      min(plotting_df$Emancipative_valuesWIK2015, na.rm = TRUE),
      max(plotting_df$Emancipative_valuesWIK2015, na.rm = TRUE),
      length.out = 100),
    e_boix_regime = c(0, 1)))

pred_plot_18 <- ggplot(
  pred_df_18,
  aes(x = Emancipative_valuesWIK2015,
      color = as.factor(e_boix_regime),
      fill  = as.factor(e_boix_regime))) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.1, color = NA) +
  geom_line(aes(y = estimate), linewidth = 0.8) +
  scale_color_manual(
    values = c("0" = "steelblue", "1" = "red"),
    labels = c("Not autocratic", "Autocratic"),
    name = NULL) +
  scale_fill_manual(
    values = c("0" = "steelblue", "1" = "red"),
    guide = "none") +
  labs(
    x = "Emancipative values",
    y = "Predicted probability of nonviolent campaign") +
  theme_bw(base_size = 9) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank())

me_plot_18 + pred_plot_18

# Continuous model 1
H3_m15ev_more_controls_plot <- feols(
  v2cademmob ~ Emancipative_valuesWIK2015 * edi_autocracy +
    log_gdpcapcon2015 + e_peaveduc + wdi_popurb +
    v2x_civlib + v2x_execorr + v2xeg_eqdr +
    factor(ISO3) + factor(Year),
  cluster = ~ ISO3,
  data = plotting_df)

# Marginal effects
me_df_15_cont <- slopes(
  H3_m15ev_more_controls_plot,
  variables = "Emancipative_valuesWIK2015",
  newdata = datagrid(
    edi_autocracy = seq(
      min(plotting_df$edi_autocracy, na.rm = TRUE),
      max(plotting_df$edi_autocracy, na.rm = TRUE),
      length.out = 100)))

me_plot_15 <- ggplot(me_df_15_cont,
                     aes(x = edi_autocracy, y = estimate)) +
  geom_line(color = "black", linewidth = 0.5) +
  geom_line(aes(y = conf.low), linetype = "dashed", color = "black", linewidth = 0.3) +
  geom_line(aes(y = conf.high), linetype = "dashed", color = "black", linewidth = 0.3) +
  geom_hline(yintercept = 0, color = "grey20") +
  labs(x = "Polyarchy reversed (autocracy index)",
       y = "Marginal Effect of Emancipative values") +
  theme_classic(base_size = 9) +
  theme(panel.grid = element_blank())

# Predicted values
pred_df_15 <- predictions(
  H3_m15ev_more_controls_plot,
  newdata = datagrid(
    Emancipative_valuesWIK2015 = seq(
      min(plotting_df$Emancipative_valuesWIK2015, na.rm = TRUE),
      max(plotting_df$Emancipative_valuesWIK2015, na.rm = TRUE),
      length.out = 100),
    edi_autocracy = seq(
      min(plotting_df$edi_autocracy, na.rm = TRUE),
      max(plotting_df$edi_autocracy, na.rm = TRUE),
      length.out = 2   # two representative levels (low and high)
      )))

pred_plot_15 <- ggplot(
  pred_df_15,
  aes(x = Emancipative_valuesWIK2015,
      y = estimate,
      color = factor(round(edi_autocracy, 2)),
      fill  = factor(round(edi_autocracy, 2)))) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.1, color = NA) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(
    values = c("steelblue", "red"),
    name = "Autocracy levels") +
  scale_fill_manual(
    values = c("steelblue", "red"),
    guide = "none") +
  labs(
    x = "Emancipative values",
    y = "Predicted mobilization for democracy") +
  theme_bw(base_size = 9) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank())

me_plot_15 + pred_plot_15

# Continuous model 2
H3_m17ev_more_controls_plot <- feols(
  any_nonviolent ~ Emancipative_valuesWIK2015 * edi_autocracy +
    log_gdpcapcon2015 + e_peaveduc + wdi_popurb +
    v2x_civlib + v2x_execorr + v2xeg_eqdr +
    factor(ISO3) + factor(Year),
  cluster = ~ ISO3,
  data = plotting_df)

# Marginal effects
me_df_17_cont <- slopes(
  H3_m17ev_more_controls_plot,
  variables = "Emancipative_valuesWIK2015",
  newdata = datagrid(
    edi_autocracy = seq(
      min(plotting_df$edi_autocracy, na.rm = TRUE),
      max(plotting_df$edi_autocracy, na.rm = TRUE),
      length.out = 100)))

me_plot_17 <- ggplot(me_df_17_cont,
                     aes(x = edi_autocracy, y = estimate)) +
  geom_line(color = "black", linewidth = 0.5) +
  geom_line(aes(y = conf.low), linetype = "dashed", color = "black", linewidth = 0.3) +
  geom_line(aes(y = conf.high), linetype = "dashed", color = "black", linewidth = 0.3) +
  geom_hline(yintercept = 0, color = "grey20") +
  labs(x = "Polyarchy reversed (autocracy index)",
       y = "Marginal Effect of Emancipative values") +
  theme_classic(base_size = 9) +
  theme(panel.grid = element_blank())

# Predicted values
pred_df_17 <- predictions(
  H3_m17ev_more_controls_plot,
  newdata = datagrid(
    Emancipative_valuesWIK2015 = seq(
      min(plotting_df$Emancipative_valuesWIK2015, na.rm = TRUE),
      max(plotting_df$Emancipative_valuesWIK2015, na.rm = TRUE),
      length.out = 100),
    edi_autocracy = seq(
      min(plotting_df$edi_autocracy, na.rm = TRUE),
      max(plotting_df$edi_autocracy, na.rm = TRUE),
      length.out = 2   # two representative levels
    )))

pred_plot_17 <- ggplot(
  pred_df_17,
  aes(x = Emancipative_valuesWIK2015,
      y = estimate,
      color = factor(round(edi_autocracy, 2)),
      fill  = factor(round(edi_autocracy, 2)))) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.1, color = NA) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(
    values = c("steelblue", "red"),
    name = "Autocracy levels") +
  scale_fill_manual(
    values = c("steelblue", "red"),
    guide = "none") +
  labs(
    x = "Emancipative values",
    y = "Predicted probability of nonviolent campaign") +
  theme_bw(base_size = 9) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank())

me_plot_17 + pred_plot_17

# Tidy workspace
remove(plotting_df, H3_m18ev_more_controls_plot, me_df_18, me_plot_18, pred_df_18, pred_plot_18,
       H3_m15ev_more_controls_plot, me_df_15_cont, me_plot_15, pred_df_15, pred_plot_15,
       H3_m17ev_more_controls_plot, me_df_17_cont, me_plot_17, pred_df_17, pred_plot_17)


##Experimenting with suggested model by Per and a democratization model (not in the thesis)##

# Make data frame to reduce errors and allow for plotting more consistently
Pers_df <- as.data.frame(panel_macro_autocratization)

# Center and square centered polyarchy variable
Pers_df$poly_c <- Pers_df$v2x_polyarchy - mean(Pers_df$v2x_polyarchy, na.rm = TRUE) #mean centered democracy score removing NA values

Pers_df$poly_c2 <- Pers_df$poly_c^2 #squared mean centered democracy score

# Fit model
Pers_model <- feols(any_nonviolent ~ Emancipative_valuesWIK2015 * (poly_c + poly_c2) + poly_c + poly_c2 + log_gdpcapcon2015 + e_peaveduc + wdi_popurb +
    factor(ISO3) + factor(Year),
  cluster = ~ ISO3,
  data = Pers_df)

summary(Pers_model)

# Running the model with SupDem_trim as the independent variable
Pers_model1 <- feols(any_nonviolent ~ SupDem_trim * (poly_c + poly_c2) + poly_c + poly_c2 + log_gdpcapcon2015 + e_peaveduc + wdi_popurb +
    factor(ISO3) + factor(Year),
  cluster = ~ ISO3,
  data = Pers_df)

summary(Pers_model1) #similar patterns

# Tidy workspace
remove(Pers_model1, Pers_model, Pers_df)

# Democratization model
democratization_model <- feols(v2x_polyarchy ~ SupDem_trim * v2cademmob + log_gdpcapcon2015 + e_peaveduc + wdi_popurb |
                                 ISO3 + Year, cluster = ~ ISO3, data = panel_macro_autocratization)
summary(democratization_model) # positive effect but not significant

remove(democratization_model)
