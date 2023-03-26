#setting root directory
here::i_am("code/02_figures.R")

#making dataframe from the saved data object
data_clean <- readRDS(
  file = here::here("output/data_clean.rds")
)

#loading libraries
library(ggplot2)
library(hrbrthemes)
library(cowplot)

#making figure 1: Walkability by Arthritis
fig1 <- ggplot(data_clean, aes(x=meanNatWalkInd, y=Arthritis)) + 
  geom_point(size=3) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_minimal() +
  labs(
    title = "Walkability Score by Prevalence of Arthritis"
  ) +
  xlab("Walkability Index") +
  ylab("Age-Adjusted Prevalence (%)")

#making figure 2: Walkability by Asthma
fig2 <- ggplot(data_clean, aes(x=meanNatWalkInd, y=Asthma)) + 
  geom_point(size=3) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_minimal() +
  labs(
    title = "Walkability Score by Prevalence of Asthma"
  ) +
  xlab("Walkability Index") +
  ylab("Age-Adjusted Prevalence (%)")

#making figure 3: Walkability by Cancer
fig3 <- ggplot(data_clean, aes(x=meanNatWalkInd, y=Cancer)) + 
  geom_point(size=3) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_minimal() +
  labs(
    title = "Walkability Score by Prevalence of Cancer"
  ) +
  xlab("Walkability Index") +
  ylab("Age-Adjusted Prevalence (%)")

#making figure 4: Walkability by Kidney Disease
fig4 <- ggplot(data_clean, aes(x=meanNatWalkInd, y=KidneyDisease)) + 
  geom_point(size=3) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_minimal() +
  labs(
    title = "Walkability Score by Prevalence of Kidney Disease"
  ) +
  xlab("Walkability Index") +
  ylab("Age-Adjusted Prevalence (%)")

#making figure 5: Walkability by Diabetes
fig5 <- ggplot(data_clean, aes(x=meanNatWalkInd, y=Diabetes)) + 
  geom_point(size=3) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_minimal() +
  labs(
    title = "Walkability Score by Prevalence of Diabetes"
  ) +
  xlab("Walkability Index") +
  ylab("Age-Adjusted Prevalence (%)")

#making figure 6: Walkability by CHD
fig6 <- ggplot(data_clean, aes(x=meanNatWalkInd, y=CHD)) + 
  geom_point(size=3) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_minimal() +
  labs(
    title = "Walkability Score by Prevalence of Coronary Heart Disease"
  ) +
  xlab("Walkability Index") +
  ylab("Age-Adjusted Prevalence (%)")

#making figure 7: Walkability by Stroke
fig7 <- ggplot(data_clean, aes(x=meanNatWalkInd, y=Stroke)) + 
  geom_point(size=3) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_minimal() +
  labs(
    title = "Walkability Score by Prevalence of Stroke"
  ) +
  xlab("Walkability Index") +
  ylab("Age-Adjusted Prevalence (%)")

#making figure 8: Walkability by HBP
fig8 <- ggplot(data_clean, aes(x=meanNatWalkInd, y=HBP)) + 
  geom_point(size=3) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_minimal() +
  labs(
    title = "Walkability Score by Prevalence of High Blood Pressure"
  ) +
  xlab("Walkability Index") +
  ylab("Age-Adjusted Prevalence (%)")

#figure 9: Walkability by Cholesterol
fig9 <- ggplot(data_clean, aes(x=meanNatWalkInd, y=Cholesterol)) + 
  geom_point(size=3) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_minimal() +
  labs(
    title = "Walkability Score by Prevalence of Cholesterol"
  ) +
  xlab("Walkability Index") +
  ylab("Age-Adjusted Prevalence (%)")

#figure 10: Walkability by Mental Health
fig10 <- ggplot(data_clean, aes(x=meanNatWalkInd, y=MentHealth)) + 
  geom_point(size=3) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_minimal() +
  labs(
    title = "Walkability Score by Prevalence of Poor Mental Health"
  ) +
  xlab("Walkability Index") +
  ylab("Age-Adjusted Prevalence (%)")

#figure 11: Walkability by Mental Health
fig11 <- ggplot(data_clean, aes(x=meanNatWalkInd, y=PhysHealth)) + 
  geom_point(size=3) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_minimal() +
  labs(
    title = "Walkability Score by Prevalence of Poor Physical Health"
  ) +
  xlab("Walkability Index") +
  ylab("Age-Adjusted Prevalence (%)")



all <- plot_grid(fig1, fig2, fig3, fig4, fig5, fig6, fig7, fig8, fig9, fig10, fig11, 
          labels = c("Arthritis", "Asthma", "Cancer", "Kidney Disease", "Diabetes",
                     "CHD", "Stroke", "HBP", "Cholesterol", "Poor Mental Health", "Poor Physical Health"),
          ncol = 2, nrow = 6)


saveRDS(
  fig1,
  file = here::here("output/fig1.rds")
)

saveRDS(
  fig2,
  file = here::here("output/fig2.rds")
)

saveRDS(
  fig3,
  file = here::here("output/fig3.rds")
)

saveRDS(
  fig4,
  file = here::here("output/fig4.rds")
)

saveRDS(
  fig5,
  file = here::here("output/fig5.rds")
)

saveRDS(
  fig6,
  file = here::here("output/fig6.rds")
)

saveRDS(
  fig7,
  file = here::here("output/fig7.rds")
)

saveRDS(
  fig8,
  file = here::here("output/fig8.rds")
)

saveRDS(
  fig9,
  file = here::here("output/fig9.rds")
)

saveRDS(
  fig10,
  file = here::here("output/fig10.rds")
)

saveRDS(
  fig11,
  file = here::here("output/fig11.rds")
)
saveRDS(
  all,
  file = here::here("output/scatter_all.rds")
)
