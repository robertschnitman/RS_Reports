##### === BEGIN === #####

#### SET UP ####
library(foreign)
library(tidyverse)

df <- as.data.frame(read.spss('backup.sav'))

df2 <- df %>% filter(HighestEdu != 5) # For some reason, there is a 5 in education.

df2$PatientType <- with(df2, ifelse(PatientType == 'Outpatient', 'Group1', 'Group2'))

specialties <- c('Specialty 1', 'Specialty 2', 'Specialty 3', 'Specialty 4', 'Specialty 5')

df2$Specialty <- sample(specialties, 447, replace = TRUE)

#### PLOTS ####
  ### Severity vs. Specialty ###
fig1 <- ggplot(filter(df2, is.na(Specialty) == FALSE), 
               aes(y = as.numeric(Severity), 
                   x = as.character(Specialty), 
                   fill = as.character(Specialty))) +
  stat_summary(fun.y = 'mean', geom = 'bar', position = 'dodge', show.legend = FALSE) +
  facet_wrap(~ PatientType) +
  labs(title = 'Fig. 1 - Average Severity by Specialty',
       y     = 'Mean Severity Score',
       x     = 'Specialty') +
  expand_limits(y = c(1, 5)) +    # Severity score is from 1 to 5.
  coord_flip() +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank()) + 
  labs(caption = 'Robert Schnitman')


fig1

summary(lm(data = df2, as.numeric(Severity) ~ factor(Specialty) + Age))

##### === END === #####