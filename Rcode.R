######Packages needed#########
library(readxl) ## To load excel sheet


library(dplyr) # Data grammar and manipulation
library(rstatix) # Shapiro Wilk and effect size
library(psych) #descriptives
library(kableExtra) #tables
library(lme4) #linear mixed effects models (LMM)
library(lmerTest) #anova like output for LMM
library(ggplot2) #data visualization
library(ggpubr)#data visualization
library(ggprism)##makes plots look like graphad
library(table1) #for descriptives
library(emmeans)
library(ggsignif)


#################Descriptives###########

Descriptives <- read_excel("~/ECC_MX.xlsx",
                           sheet = "max")
View(Descriptives)

### Descriptives Overall
table1(~ Age + VO2max + Height + Weight +  HR + Lactate + RPE + Workload | Condition*Sex,
       total=F,render.categorical="FREQ (PCTnoNA%)", na.rm = TRUE,data=Descriptives,
       render.missing=NULL,topclass = "Rtable1-grid Rtable1-shade Rtable1-times",
       overall=FALSE)

###Descriptives Overall
table1(~ Age + VO2max + Height + Weight +  HR + Lactate + RPE + Workload | Condition,
       total=F,render.categorical="FREQ (PCTnoNA%)", na.rm = TRUE,data=Descriptives,
       render.missing=NULL,topclass = "Rtable1-grid Rtable1-shade Rtable1-times",
       overall=FALSE)


Descriptives$Condition <- as.factor(Descriptives$Condition)

## Order conditions
Descriptives$Condition <- ordered(Descriptives$Condition,
                                  levels = c("CON", "ECC",
                                             "ECC2"))

#####maxnormalities####
##Lactate normality
Descriptives %>% group_by(Condition) %>%
  shapiro_test(Lactate)

Descriptives %>% group_by(Condition) %>%
  shapiro_test(RPE)

##Re normality
Descriptives %>% group_by(Condition) %>%
  shapiro_test(Workload)

Descriptives %>% group_by(Condition) %>%
  shapiro_test(VO2max)

##ESS normality
Descriptives %>% group_by(Condition) %>%
  shapiro_test(HR)


#####maxcomparisons#####
####Lactate 
lmModel = lmer(Lactate ~ Condition + (1|ID), data=Descriptives)
summary(lmModel)

# mixed model
anova(lmModel)
#test of the random effects in the model
rand(lmModel)

pwc2 <- Descriptives %>%
  pairwise_t_test(Lactate ~ Condition, paired = TRUE,
                  p.adjust.method	= "holm")
pwc2%>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# Effect size Cohen's D with Hedge's g correction for small sample size
Descriptives %>% cohens_d(Lactate ~ Condition,
                paired = TRUE, hedges.correction = TRUE)%>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")

#Plots
# Add position for p values in boxplot
pwc2 <- pwc2 %>% add_xy_position(x = "Condition")
# Boxplot of ESS retrograde
Lactate_max <- ggboxplot(Descriptives, x = "Condition", y = "Lactate",
                                 color = "Condition", palette = get_palette("Set1", 4),
                                 ylab = "Lactate (m/mmol)") +
  stat_pvalue_manual(pwc2,size = 4.5,hide.ns = TRUE)+
  theme_prism()## Change PWC

####HR 
lmModel = lmer(HR ~ Condition + (1|ID), data=Descriptives)
summary(lmModel)

# mixed model
anova(lmModel)
#test of the random effects in the model
rand(lmModel)

pwc2 <- Descriptives %>%
  pairwise_t_test(HR ~ Condition, paired = TRUE,
                  p.adjust.method	= "holm")
pwc2%>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# Effect size Cohen's D with Hedge's g correction for small sample size
Descriptives %>% cohens_d(HR ~ Condition,
                          paired = TRUE, hedges.correction = TRUE)%>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")

#Plots
# Add position for p values in boxplot
pwc2 <- pwc2 %>% add_xy_position(x = "Condition")
# Boxplot of ESS retrograde
HR_max <- ggboxplot(Descriptives, x = "Condition", y = "HR",
                         color = "Condition", palette = get_palette("Set1", 4),
                         ylab = "Heart Rate (bpm)") +
  stat_pvalue_manual(pwc2,size = 4.5,hide.ns = TRUE)+
  theme_prism()## Change PWC


####RPE 
lmModel = lmer(RPE ~ Condition + (1|ID), data=Descriptives)
summary(lmModel)

# mixed model
anova(lmModel)
#test of the random effects in the model
rand(lmModel)

pwc2 <- Descriptives %>%
  pairwise_t_test(RPE ~ Condition, paired = TRUE,
                  p.adjust.method	= "holm")
pwc2%>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# Effect size Cohen's D with Hedge's g correction for small sample size
Descriptives %>% cohens_d(RPE ~ Condition,
                          paired = TRUE, hedges.correction = TRUE)%>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")

#Plots
# Add position for p values in boxplot
pwc2 <- pwc2 %>% add_xy_position(x = "Condition")
# Boxplot of ESS retrograde
RPE_max <- ggboxplot(Descriptives, x = "Condition", y = "RPE",
                    color = "Condition", palette = get_palette("Set1", 4),
                    ylab = "Rate of Perceived Exertion") +
  stat_pvalue_manual(pwc2,size = 4.5,hide.ns = TRUE)+
  theme_prism()## Change PWC

####Workload 
lmModel = lmer(Workload ~ Condition + (1|ID), data=Descriptives)
summary(lmModel)

# mixed model
anova(lmModel)
#test of the random effects in the model
rand(lmModel)

pwc2 <- Descriptives %>%
  pairwise_t_test(Workload ~ Condition, paired = TRUE,
                  p.adjust.method	= "holm")
pwc2%>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# Effect size Cohen's D with Hedge's g correction for small sample size
Descriptives %>% cohens_d(Workload ~ Condition,
                          paired = TRUE, hedges.correction = TRUE)%>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")

#Plots
# Add position for p values in boxplot
pwc2 <- pwc2 %>% add_xy_position(x = "Condition")
# Boxplot of ESS retrograde
Workload_max <- ggboxplot(Descriptives, x = "Condition", y = "Workload",
                     color = "Condition", palette = get_palette("Set1", 4),
                     ylab = "Workload (w)") +
  stat_pvalue_manual(pwc2,size = 4.5,hide.ns = TRUE)+
  theme_prism()## Change PWC


####VO2max
lmModel = lmer(VO2max ~ Condition + (1|ID), data=Descriptives)
summary(lmModel)

# mixed model
anova(lmModel)
#test of the random effects in the model
rand(lmModel)

pwc2 <- Descriptives %>%
  pairwise_t_test(VO2max ~ Condition, paired = TRUE,
                  p.adjust.method	= "holm")
pwc2%>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# Effect size Cohen's D with Hedge's g correction for small sample size
Descriptives %>% cohens_d(VO2max ~ Condition,
                          paired = TRUE, hedges.correction = TRUE)%>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")

#Plots
# Add position for p values in boxplot
pwc2 <- pwc2 %>% add_xy_position(x = "Condition")
# Boxplot of ESS retrograde
VO2max_max <- ggboxplot(Descriptives, x = "Condition", y = "VO2max",
                          color = "Condition", palette = get_palette("Set1", 4),
                          ylab = "Oxygen Consumption (ml/kg/min)") +
  stat_pvalue_manual(pwc2,size = 4.5,hide.ns = TRUE)+
  theme_prism()## Change PWC




#####tailored #####

Df <- read_excel("~/ECC_MX.xlsx",
                 sheet = "tailored")
View(Df)

###Trasnsform to condition and modality
Df$Condition <- as.factor(Df$Condition)
Df$Modality <- as.factor(Df$Modality)

## Order conditions
Df$Condition <- ordered(Df$Condition,
                        levels = c("Baseline", "Low", "Moderate", "High"))

Df$Modality <- ordered(Df$Modality, levels = c("CON_CON", "ECC_CON", "ECC_ECC"))

describeBy(Df ~ Condition * Modality, na.rm=T, skew=FALSE, ranges=F)

###tailored overall descriptives
table1(~ Workload + HR + VO2 + Lactate +  RPE + Ve + Tv + VCO2 + VO2L | Condition*Modality,
       total=F,render.categorical="FREQ (PCTnoNA%)", na.rm = TRUE,data=Df,
       render.missing=NULL,topclass = "Rtable1-grid Rtable1-shade Rtable1-times",
       overall=FALSE)

#####Tailored normality####
##Lactate normality
Df %>% group_by(Condition) %>%
  shapiro_test(Lactate)

Df %>% group_by(Condition) %>%
  shapiro_test(RPE)

##Workload normality
Df %>% group_by(Condition) %>%
  shapiro_test(Workload)

Df %>% group_by(Condition) %>%
  shapiro_test(VO2)

##HR normality
Df %>% group_by(Condition) %>%
  shapiro_test(HR)

Df %>% group_by(Condition) %>%
  shapiro_test(VO2L)

Df %>% group_by(Condition) %>%
  shapiro_test(Ve)

Df %>% group_by(Condition) %>%
  shapiro_test(Tv)


Df %>% group_by(Condition) %>%
  shapiro_test(VCO2)



lmModel = lmer(Lactate ~ Condition + Modality + (1|Subject_ID), data=Df)
summary(lmModel)

# mixed model
anova(lmModel)
#test of the random effects in the model
rand(lmModel)

pwc2 <- Df %>%
  pairwise_t_test(Lactate ~ Condition + Modality, paired = TRUE,
                  p.adjust.method	= "holm")
pwc2%>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")
######Lactate#####
# Linear Mixed Model with interaction term
lmModel <- lmer(Lactate ~ Condition * Modality + (1|Subject_ID), data=Df)
summary(lmModel)

# ANOVA for fixed effects and interactions
anova(lmModel)

# Test of the random effects in the model
rand(lmModel)

# Post-hoc pairwise comparisons
# Pairwise comparisons for Condition within each Modality
pwc_condition <- emmeans(lmModel, pairwise ~ Condition | Modality, adjust = "holm")

# Pairwise comparisons for Modality within each Condition
pwc_modality <- emmeans(lmModel, pairwise ~ Modality | Condition, adjust = "holm")

# Output pairwise comparisons for Condition within each Modality
pwc_condition$contrasts %>%
  kbl(caption = "Pairwise Comparisons for Condition within Each Modality") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# Output pairwise comparisons for Modality within each Condition
pwc_modality$contrasts %>%
  kbl(caption = "Pairwise Comparisons for Modality within Each Condition") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# Extracting pairwise comparisons data
condition_modality_df <- as.data.frame(pwc_condition$contrasts)
modality_condition_df <- as.data.frame(pwc_modality$contrasts)

##Plot 1: Lactate Levels Across Conditions for Each Modality
ggplot(Df, aes(x = Condition, y = Lactate, fill = Modality)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge", width = 0.6) +
  geom_errorbar(stat = "summary", fun.data = "mean_se", width = 0.2, position = position_dodge(0.6)) +
  facet_wrap(~ Modality) +
  labs(title = "Lactate Levels Across Conditions within Each Modality",
       x = "Condition", y = "Lactate (m/mmol)") 
I need to add invidual sig!!!
  p + 
  geom_text(data = data.frame(Modality = "ECC:ECC", x = 2, y = 8, label = "*"),
            aes(x = x, y = y, label = label), color = "black") +
  geom_text(data = data.frame(Modality = "ECC:CON", x = 3, y = 8, label = "**"),
            aes(x = x, y = y, label = label), color = "black") +
  geom_text(data = data.frame(Modality = "CON:CON", x = 4, y = 8, label = "***"),
            aes(x = x, y = y, label = label), color = "black")
Modality = "ECC:ECC": Adjust this to the correct modality you’re annotating.
x = 2, 3, 4: These numbers correspond to the conditions (Baseline, Low, Moderate, High). Adjust based on your actual data.
y = 8: Adjust this based on the range of your Lactate values to ensure the annotations are placed correctly above the bars.


# Plot 2: Lactate Levels Across Modalities for Each Condition
ggplot(Df, aes(x = Modality, y = Lactate, fill = Condition)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge", width = 0.6) +
  geom_errorbar(stat = "summary", fun.data = "mean_se", width = 0.2, position = position_dodge(0.6)) +
  facet_wrap(~ Condition) +
  labs(title = "Lactate Levels Across Modalities within Each Condition",
       x = "Modality", y = "Mean Lactate Levels (±SE)") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  geom_signif(comparisons = list(c("ECC:ECC", "ECC:CON"), c("CON:CON", "ECC:CON")),
              map_signif_level = TRUE, step_increase = 0.1, position = position_dodge(0.6))

####HR
lmModel2 <- lmer(HR ~ Condition * Modality + (1|Subject_ID), data=Df)
summary(lmModel2)

# ANOVA for fixed effects and interactions
anova(lmModel2)

# Test of the random effects in the model
rand(lmModel2)

# Post-hoc pairwise comparisons
# Pairwise comparisons for Condition within each Modality
pwc_condition2 <- emmeans(lmModel2, pairwise ~ Condition | Modality, adjust = "holm")

# Pairwise comparisons for Modality within each Condition
pwc_modality2 <- emmeans(lmModel2, pairwise ~ Modality | Condition, adjust = "holm")

# Output pairwise comparisons for Condition within each Modality
pwc_condition2$contrasts %>%
  kbl(caption = "Pairwise Comparisons for Condition within Each Modality") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# Output pairwise comparisons for Modality within each Condition
pwc_modality2$contrasts %>%
  kbl(caption = "Pairwise Comparisons for Modality within Each Condition") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# Extracting pairwise comparisons data
condition_modality_df2 <- as.data.frame(pwc_condition2$contrasts)
modality_condition_df2 <- as.data.frame(pwc_modality2$contrasts)

##Plot 1: HR Levels Across Conditions for Each Modality
ggplot(Df, aes(x = Condition, y = HR, fill = Modality)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge", width = 0.6) +
  geom_errorbar(stat = "summary", fun.data = "mean_sd", width = 0.2, position = position_dodge(0.6)) +
  facet_wrap(~ Modality) +
  labs(title = "Heart Rate Across Conditions within Each Modality",
       x = "Condition", y = "Heart Rate (bpm)") 
I need to add invidual sig!!!
  p + 
  geom_text(data = data.frame(Modality = "ECC:ECC", x = 2, y = 8, label = "*"),
            aes(x = x, y = y, label = label), color = "black") +
  geom_text(data = data.frame(Modality = "ECC:CON", x = 3, y = 8, label = "**"),
            aes(x = x, y = y, label = label), color = "black") +
  geom_text(data = data.frame(Modality = "CON:CON", x = 4, y = 8, label = "***"),
            aes(x = x, y = y, label = label), color = "black")
Modality = "ECC:ECC": Adjust this to the correct modality you’re annotating.
x = 2, 3, 4: These numbers correspond to the conditions (Baseline, Low, Moderate, High). Adjust based on your actual data.
y = 8: Adjust this based on the range of your Lactate values to ensure the annotations are placed correctly above the bars.


# Plot 2: HR Levels Across Modalities for Each Condition
ggplot(Df, aes(x = Modality, y = HR, fill = Condition)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge", width = 0.6) +
  geom_errorbar(stat = "summary", fun.data = "mean_sd", width = 0.2, position = position_dodge(0.6)) +
  facet_wrap(~ Condition) +
  labs(title = "Heart Rate Across Modalities within Each Condition",
       x = "Modality", y = "Heart Rate (bpm)") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  geom_signif(comparisons = list(c("ECC:ECC", "ECC:CON"), c("CON:CON", "ECC:CON")),
              map_signif_level = TRUE, step_increase = 0.1, position = position_dodge(0.6))

#####Workloadmax####

lmModel3 <- lmer(Workload ~ Condition * Modality + (1|Subject_ID), data=Df)
summary(lmModel3)

# ANOVA for fixed effects and interactions
anova(lmModel3)

# Test of the random effects in the model
rand(lmModel3)

# Post-hoc pairwise comparisons
# Pairwise comparisons for Condition within each Modality
pwc_condition3 <- emmeans(lmModel3, pairwise ~ Condition | Modality, adjust = "holm")

# Pairwise comparisons for Modality within each Condition
pwc_modality3 <- emmeans(lmModel3, pairwise ~ Modality | Condition, adjust = "holm")

# Output pairwise comparisons for Condition within each Modality
pwc_condition3$contrasts %>%
  kbl(caption = "Pairwise Comparisons for Condition within Each Modality") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# Output pairwise comparisons for Modality within each Condition
pwc_modality3$contrasts %>%
  kbl(caption = "Pairwise Comparisons for Modality within Each Condition") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# Extracting pairwise comparisons data
condition_modality_df3 <- as.data.frame(pwc_condition3$contrasts)
modality_condition_df3 <- as.data.frame(pwc_modality3$contrasts)

##Plot 1: Workload Levels Across Conditions for Each Modality
ggplot(Df, aes(x = Condition, y = Workload, fill = Modality)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge", width = 0.6) +
  geom_errorbar(stat = "summary", fun.data = "mean_sd", width = 0.2, position = position_dodge(0.6)) +
  facet_wrap(~ Modality) +
  labs(title = "Workload Across Conditions within Each Modality",
       x = "Condition", y = "Workload (w)") 
I need to add invidual sig!!!
  p + 
  geom_text(data = data.frame(Modality = "ECC:ECC", x = 2, y = 8, label = "*"),
            aes(x = x, y = y, label = label), color = "black") +
  geom_text(data = data.frame(Modality = "ECC:CON", x = 3, y = 8, label = "**"),
            aes(x = x, y = y, label = label), color = "black") +
  geom_text(data = data.frame(Modality = "CON:CON", x = 4, y = 8, label = "***"),
            aes(x = x, y = y, label = label), color = "black")
Modality = "ECC:ECC": Adjust this to the correct modality you’re annotating.
x = 2, 3, 4: These numbers correspond to the conditions (Baseline, Low, Moderate, High). Adjust based on your actual data.
y = 8: Adjust this based on the range of your Lactate values to ensure the annotations are placed correctly above the bars.


# Plot 2: Workload Levels Across Modalities for Each Condition
ggplot(Df, aes(x = Modality, y = Workload, fill = Condition)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge", width = 0.6) +
  geom_errorbar(stat = "summary", fun.data = "mean_sd", width = 0.2, position = position_dodge(0.6)) +
  facet_wrap(~ Condition) +
  labs(title = "Workload Across Modalities within Each Condition",
       x = "Modality", y = "Workload (w)") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  geom_signif(comparisons = list(c("ECC:ECC", "ECC:CON"), c("CON:CON", "ECC:CON")),
              map_signif_level = TRUE, step_increase = 0.1, position = position_dodge(0.6))

#####VO2max#####
lmModel4 <- lmer(VO2 ~ Condition * Modality + (1|Subject_ID), data=Df)
summary(lmModel4)

# ANOVA for fixed effects and interactions
anova(lmModel4)

# Test of the random effects in the model
rand(lmModel4)

# Post-hoc pairwise comparisons
# Pairwise comparisons for Condition within each Modality
pwc_condition4 <- emmeans(lmModel4, pairwise ~ Condition | Modality, adjust = "holm")

# Pairwise comparisons for Modality within each Condition
pwc_modality4 <- emmeans(lmModel4, pairwise ~ Modality | Condition, adjust = "holm")

# Output pairwise comparisons for Condition within each Modality
pwc_condition4$contrasts %>%
  kbl(caption = "Pairwise Comparisons for Condition within Each Modality") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# Output pairwise comparisons for Modality within each Condition
pwc_modality4$contrasts %>%
  kbl(caption = "Pairwise Comparisons for Modality within Each Condition") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# Extracting pairwise comparisons data
condition_modality_df4 <- as.data.frame(pwc_condition4$contrasts)
modality_condition_df4 <- as.data.frame(pwc_modality4$contrasts)

##Plot 1: VO2 Levels Across Conditions for Each Modality
ggplot(Df, aes(x = Condition, y = VO2, fill = Modality)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge", width = 0.6) +
  geom_errorbar(stat = "summary", fun.data = "mean_sd", width = 0.2, position = position_dodge(0.6)) +
  facet_wrap(~ Modality) +
  labs(title = "VO2 Across Conditions within Each Modality",
       x = "Condition", y = "VO2 (ml/kg/min)")
I need to add invidual sig!!!
  p + 
  geom_text(data = data.frame(Modality = "ECC:ECC", x = 2, y = 8, label = "*"),
            aes(x = x, y = y, label = label), color = "black") +
  geom_text(data = data.frame(Modality = "ECC:CON", x = 3, y = 8, label = "**"),
            aes(x = x, y = y, label = label), color = "black") +
  geom_text(data = data.frame(Modality = "CON:CON", x = 4, y = 8, label = "***"),
            aes(x = x, y = y, label = label), color = "black")
Modality = "ECC:ECC": Adjust this to the correct modality you’re annotating.
x = 2, 3, 4: These numbers correspond to the conditions (Baseline, Low, Moderate, High). Adjust based on your actual data.
y = 8: Adjust this based on the range of your Lactate values to ensure the annotations are placed correctly above the bars.


# Plot 2: VO2 Levels Across Modalities for Each Condition
ggplot(Df, aes(x = Modality, y = VO2, fill = Condition)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge", width = 0.6) +
  geom_errorbar(stat = "summary", fun.data = "mean_sd", width = 0.2, position = position_dodge(0.6)) +
  facet_wrap(~ Condition) +
  labs(title = "VO2 Across Modalities within Each Condition",
       x = "Modality", y = "VO2 (ml/kg/min)")+
  theme_minimal() +
  theme(legend.position = "bottom") +
  geom_signif(comparisons = list(c("ECC:ECC", "ECC:CON"), c("CON:CON", "ECC:CON")),
              map_signif_level = TRUE, step_increase = 0.1, position = position_dodge(0.6))


#####RPEmax#####
lmModel5 <- lmer(VO2 ~ Condition * Modality + (1|Subject_ID), data=Df)
summary(lmModel5)

# ANOVA for fixed effects and interactions
anova(lmModel5)

# Test of the random effects in the model
rand(lmModel5)

# Post-hoc pairwise comparisons
# Pairwise comparisons for Condition within each Modality
pwc_condition5 <- emmeans(lmModel5, pairwise ~ Condition | Modality, adjust = "holm")

# Pairwise comparisons for Modality within each Condition
pwc_modality5 <- emmeans(lmModel5, pairwise ~ Modality | Condition, adjust = "holm")

# Output pairwise comparisons for Condition within each Modality
pwc_condition5$contrasts %>%
  kbl(caption = "Pairwise Comparisons for Condition within Each Modality") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# Output pairwise comparisons for Modality within each Condition
pwc_modality5$contrasts %>%
  kbl(caption = "Pairwise Comparisons for Modality within Each Condition") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# Extracting pairwise comparisons data
condition_modality_df5 <- as.data.frame(pwc_condition5$contrasts)
modality_condition_df5 <- as.data.frame(pwc_modality5$contrasts)

##Plot 1: RPE Levels Across Conditions for Each Modality
ggplot(Df, aes(x = Condition, y = RPE, fill = Modality)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge", width = 0.6) +
  geom_errorbar(stat = "summary", fun.data = "mean_sd", width = 0.2, position = position_dodge(0.6)) +
  facet_wrap(~ Modality) +
  labs(title = "Rate of Perceived Exertion Across Conditions within Each Modality",
       x = "Condition", y = "Rate of Perceived Exertion")
I need to add invidual sig!!!
  p + 
  geom_text(data = data.frame(Modality = "ECC:ECC", x = 2, y = 8, label = "*"),
            aes(x = x, y = y, label = label), color = "black") +
  geom_text(data = data.frame(Modality = "ECC:CON", x = 3, y = 8, label = "**"),
            aes(x = x, y = y, label = label), color = "black") +
  geom_text(data = data.frame(Modality = "CON:CON", x = 4, y = 8, label = "***"),
            aes(x = x, y = y, label = label), color = "black")
Modality = "ECC:ECC": Adjust this to the correct modality you’re annotating.
x = 2, 3, 4: These numbers correspond to the conditions (Baseline, Low, Moderate, High). Adjust based on your actual data.
y = 8: Adjust this based on the range of your Lactate values to ensure the annotations are placed correctly above the bars.


# Plot 2: RPE Levels Across Modalities for Each Condition
ggplot(Df, aes(x = Modality, y = RPE, fill = Condition)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge", width = 0.6) +
  geom_errorbar(stat = "summary", fun.data = "mean_sd", width = 0.2, position = position_dodge(0.6)) +
  facet_wrap(~ Condition) +
  labs(title = "Rate of Perceived Exertion Across Modalities within Each Condition",
       x = "Modality", y = "Rate of Perceived Exertion")+
  theme_minimal() +
  theme(legend.position = "bottom") +
  geom_signif(comparisons = list(c("ECC:ECC", "ECC:CON"), c("CON:CON", "ECC:CON")),
              map_signif_level = TRUE, step_increase = 0.1, position = position_dodge(0.6))

