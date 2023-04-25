#setting root directory
here::i_am("code/03_regression.R")

#making dataframe from the saved data object
data_clean <- readRDS(
  file = here::here("output/data_clean.rds")
)

#setting libraries
library(car)
library(gtsummary)

#making scatterplot matrix using car package

scatterplotMatrix(~ meanNatWalkInd + Asthma + Arthritis + Cancer + KidneyDisease + Diabetes +
                    CHD + Stroke + HBP + Cholesterol + MentHealth + PhysHealth,
                  span = 0.7,
                  data = data_clean)


# variables look largely normally distributed
# note that NatWalkInd and Asthma have two humps

# linear regression

mod1 <- lm(meanNatWalkInd ~ Asthma + Arthritis + Cancer + KidneyDisease + Diabetes +
             CHD + Stroke + HBP + Cholesterol + MentHealth + PhysHealth,
           data = data_clean)

summary(mod1)


tbl_mod1 <- tbl_regression(mod1)
tbl_mod1

saveRDS(
  tbl_mod1,
  file = here::here("output/tb1_mod1.rds")
)

#Several of these variable do not have significant p-values, let's see what happens when we remove most of them
#lets try the most conservative model first -> least amount of predictors, only those that had p-value < 0.1 in first model 

mod2 <- lm(meanNatWalkInd ~ Asthma + Arthritis + Diabetes + CHD,
           data = data_clean)

summary(mod2)


tbl_mod2 <- tbl_regression(mod2)
tbl_mod2

saveRDS(
  tbl_mod2,
  file = here::here("output/tb1_mod2.rds")
)

#this model now has all very significant variables 

#lets now try a model that is in a middle ground -> lets use those health outcomes that we'd expect to be associated with physical activity

mod3 <- lm(meanNatWalkInd ~ Arthritis + Diabetes + CHD + Stroke + HBP + Cholesterol + MentHealth + PhysHealth,
           data = data_clean)

summary(mod3)

tbl_mod3 <- tbl_regression(mod3)
tbl_mod3

saveRDS(
  tbl_mod3,
  file = here::here("output/tb1_mod3.rds")
)

#looks like cholesterol is still not sig, same with phys health, stroke, HBP, MentHealth, Arthritis, lets try removing these

mod4 <- lm(meanNatWalkInd ~ Diabetes + CHD,
           data = data_clean)

summary(mod4)

tbl_mod4 <- tbl_regression(mod4)
tbl_mod4

saveRDS(
  tbl_mod4,
  file = here::here("output/tb1_mod4.rds")
)

#lets try one more model that keeps mental health in
mod5 <- lm(meanNatWalkInd ~ Asthma + Arthritis + Diabetes +
             CHD + MentHealth,
           data = data_clean)

summary(mod5)


tbl_mod5 <- tbl_regression(mod5)
tbl_mod5

saveRDS(
  tbl_mod5,
  file = here::here("output/tb1_mod5.rds")
)

#now both of these are statistically significant -> p-values of 0.001 or less
#but this model seems a bit too conservative
#lets compare all of these models

tbl_mod12345 <-
  tbl_merge(
    tbls = list(tbl_mod1, tbl_mod2, tbl_mod3, tbl_mod4, tbl_mod5), 
    tab_spanner = c("**Model 1**", "**Model 2**", 
                    "**Model 3**", "**Model 4**",
                    "**Model 5**") 
  )
tbl_mod12345

saveRDS(
  tbl_mod12345,
  file = here::here("output/tb1_mod12345.rds")
)


#when comparing all 5, I think model 5 is the best 
#model 5 provides the most amount sig. variables, without over fitting, and keeping parsimony

#checking residual plots
residualmod5 <- residualPlots(mod5)
residualmod5

saveRDS(
  residualmod5,
  file = here::here("output/residualmod5.rds")
)


#all have similar to zero trends now

#qqplot for model 5
qqPlot(mod5, id.n=3)
#10, 8 have the largest residual in absolute value

#outlier test
outlierTest(mod5)
#no outliers found

#influence test
influenceIndexPlot(mod1, id.n=3)

#influence plot
influenceplot <- influencePlot(mod1, id.n=3)
# 8 and 10 have large influence

saveRDS(
  influenceplot,
  file = here::here("output/influenceplot.rds")
)

#test for heteroskedasticity
ncvTest(mod5)

#INDIVIDUAL MODELS -> how well does WalkInd predict these health outcomes

#Asthma Model
modAsthma <- lm(Asthma ~ meanNatWalkInd,
                data = data_clean)

summary(modAsthma)

tbl_modAsthma <- tbl_regression(modAsthma)
tbl_modAsthma

#Arthritis Model
modArthritis <- lm(Arthritis ~ meanNatWalkInd,
                data = data_clean)

summary(modArthritis)

tbl_modArthritis <- tbl_regression(modArthritis)
tbl_modArthritis

#Cancer Model
modCancer <- lm(Cancer ~ meanNatWalkInd,
                data = data_clean)

summary(modCancer)

tbl_modCancer <- tbl_regression(modCancer)
tbl_modCancer

#Kidney Disease Model
modKidneyDisease <- lm(KidneyDisease ~ meanNatWalkInd,
                data = data_clean)

summary(modKidneyDisease)

tbl_modKidneyDisease <- tbl_regression(modKidneyDisease)
tbl_modKidneyDisease

#Diabetes Model
modDiabetes <- lm(Diabetes ~ meanNatWalkInd,
                       data = data_clean)

summary(modDiabetes)

tbl_modDiabetes <- tbl_regression(modDiabetes)
tbl_modDiabetes

#CHD Model
modCHD <- lm(CHD ~ meanNatWalkInd,
                       data = data_clean)

summary(modCHD)

tbl_modCHD <- tbl_regression(modCHD)
tbl_modCHD

#Stroke Model
modStroke <- lm(Stroke ~ meanNatWalkInd,
                       data = data_clean)

summary(modStroke)

tbl_modStroke <- tbl_regression(modStroke)
tbl_modStroke

#HBP Model
modHBP <- lm(HBP ~ meanNatWalkInd,
                data = data_clean)

summary(modHBP)

tbl_modHBP <- tbl_regression(modHBP)
tbl_modHBP

#Cholesterol Model
modCholesterol <- lm(Cholesterol ~ meanNatWalkInd,
             data = data_clean)

summary(modCholesterol)

tbl_modCholesterol <- tbl_regression(modCholesterol)
tbl_modCholesterol

#MentHealth Model
modMentHealth <- lm(MentHealth ~ meanNatWalkInd,
             data = data_clean)

summary(modMentHealth)

tbl_modMentHealth <- tbl_regression(modMentHealth)
tbl_modMentHealth

#PhysHealth Model
modPhysHealth <- lm(PhysHealth ~ meanNatWalkInd,
                    data = data_clean)

summary(modPhysHealth)

tbl_modPhysHealth <- tbl_regression(modPhysHealth)
tbl_modPhysHealth

#Comparing all models
tbl_modell1 <-
  tbl_merge(
    tbls = list(tbl_modAsthma, tbl_modArthritis, tbl_modCancer), 
    tab_spanner = c("**Asthma Model**", "**Arthritis Model**", 
                    "**Cancer Model**") 
  )
tbl_modell1

tbl_modell2 <-
  tbl_merge(
    tbls = list(tbl_modKidneyDisease, tbl_modDiabetes, tbl_modCHD), 
    tab_spanner = c("**Kidney Disease Model**","**Diabetes Model**", "**CHD Model**") 
  )
tbl_modell2

tbl_modell3 <-
  tbl_merge(
    tbls = list(tbl_modStroke, tbl_modHBP, tbl_modCholesterol), 
    tab_spanner = c("**Stroke Model**", "**HBP Model**","**Cholesterol Model**") 
  )

tbl_modell3

tbl_modell4 <-
  tbl_merge(
    tbls = list(tbl_modMentHealth, tbl_modPhysHealth), 
    tab_spanner = c("**Mental Health Model**",
                    "**Physical Health Model**") 
  )
tbl_modell4

saveRDS(
  tbl_modell1,
  file = here::here("output/tbl_model1.rds")
)

saveRDS(
  tbl_modell2,
  file = here::here("output/tbl_model2.rds")
)

saveRDS(
  tbl_modell3,
  file = here::here("output/tbl_model3.rds")
)

saveRDS(
  tbl_modell4,
  file = here::here("output/tbl_model4.rds")
)


