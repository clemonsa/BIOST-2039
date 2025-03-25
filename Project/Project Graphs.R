source("Project_DataClean.R")

set.seed(12062019)

pacman::p_load(ztable,
               ggthemes,
               ggpubr,
               ggplot2)

# library(tidyverse)
# library(ztable)
# library(ggthemes)
# library(ggpubr)

#Creates a reusable ggplot object *without* data = data.frame()
gg_prop2 <- ggplot(data = data.frame()) +
  geom_bar(stat = 'identity', position = 'dodge', alpha = 2/3) +  
  scale_fill_few('Medium', drop = FALSE) + # keep levels, if data is filtered
  rremove('legend.title')

## using NSE, except for mutate, the last step to create a proportion table to be used for ggplot object
fun_prop <- function (data = data.frame(), dots = NULL, ...) {
  
  dfr_prop <- data %>% 
    group_by(.dots = dots) %>% 
    summarise(n = ~n()) %>% 
    mutate(prop = prop.table(n))  # can this be done by NSE with mutate_() ??
  
  dfr_prop
}  

demo_prop <- fun_prop(data=combo, dots=c('RIAGENDR','RIDRETH1')) # Proportion Table of Gender & Race
demo_prop2 <- fun_prop(data=combo, dots=c('RIAGENDR','RIDAGEYR')) # Proportion Table of Gender and AGe
demo_prop3 <- fun_prop(data=combo, dots=c('RIDAGEYR','RIDRETH1')) # Proportion Table of Race & Age

diet_male <- dietcombo[(dietcombo$RIAGENDR == 'Male'),c('Positive.Test','DBQ700')] #Subset Male Diets
diet_female <- dietcombo[(dietcombo$RIAGENDR == 'Female'),c('Positive.Test','DBQ700')] #Subset Female Diets

diet_maleprop <- fun_prop(data=diet_male, dots = c('Positive.Test','DBQ700')) #Proportion Table of Diet and STI Status for Men
diet_femaleprop <- fun_prop(data=diet_female, dots = c('Positive.Test','DBQ700')) #Proportion Table of Diet and STI Status for Women

phys_male <- physcombo[(physcombo$RIAGENDR == 'Male'),c('Positive.Test','Activity')] #Subset Male Physical Activity
phys_female <- physcombo[(physcombo$RIAGENDR == 'Female'),c('Positive.Test','Activity')] #Subset Female Physical Activity

phys_maleprop <- fun_prop(data=phys_male, dots = c('Positive.Test','Activity')) #Proportion Table of Phys and STI Status for Men
phys_femaleprop <- fun_prop(data=phys_female, dots = c('Positive.Test','Activity')) #Proportion Table of Phys and STI Status for Women

phys_maleprop$Positive.Test[phys_maleprop$Positive.Test == 0] = 'Negative'
phys_maleprop$Positive.Test[phys_maleprop$Positive.Test == 1] = 'Positive'
phys_femaleprop$Positive.Test[phys_femaleprop$Positive.Test == 0] = 'Negative'
phys_femaleprop$Positive.Test[phys_femaleprop$Positive.Test == 1] = 'Positive'


diet_cond <- dietcombo %>%
  fun_prop(c('DBQ700','RIAGENDR','RIDRETH1')) #Proportion Table of Demographic Data w/ Diet Conditional

diet_cond2 <- dietcombo %>%
  fun_prop(c('DBQ700','RIAGENDR','RIDAGEYR')) #Proportion Table of Demographic Data w/ Diet Conditional

phys_cond <- physcombo %>%
  fun_prop(c('Activity','RIAGENDR','RIDRETH1')) #Proportion Table of Demographic Data w/ Physical Conditonal

phys_cond2 <- physcombo %>%
  fun_prop(c('Activity','RIAGENDR','RIDAGEYR')) #Proportion Table of Demographic Data w/ Diet Conditional

#################################Figures and Graphs########################

#Figure1 for Visualizing STI Cases by Demographic
plot1a <-ggplot(data = combo
               , aes(x = RIAGENDR, y=..count.., fill = factor(Positive.Test))) + 
  geom_bar(position = 'dodge', alpha = 2/3) +  
  rremove('legend.title') +
  scale_fill_few('Medium', drop = FALSE) +              # keep levels, if data is filtered
  labs(x = 'Gender', y = 'Number of Cases', fill = 'RIAGENDR'
       , title = 'STI Status by Gender')  # what's the reader looking at?

plot1b <- ggplot (data= combo ,
                 aes(x=RIDRETH1, y=..count.., fill=Positive.Test)) +
  geom_bar(position = 'dodge', alpha=2/3) +
  rremove('legend.title') +
  scale_fill_few('Medium', drop=FALSE) +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(x='Race/Ethnicity',y='Number of Cases', title ='STI Status by Race/Ethnicity')

plot1c <- ggplot (data= combo ,
                 aes(x=RIDAGEYR, y=..count.., fill=Positive.Test)) +
  geom_bar(position = 'dodge', alpha=2/3) +
  rremove('legend.title') +
  scale_y_continuous() +
  scale_x_continuous() +
  scale_fill_few('Medium', drop=FALSE) +
  labs(x='Age in years',y='Number of Cases', title ='STI Status by Age')

figure1 <- ggarrange(plot1a, plot1b, plot1c,
                     labels = c("A","B","C"),
                     ncol = 2, nrow=2)

annotate_figure(figure1,
                top = text_grob('Visualization of Sample Prevalence', face='bold',size=14),
                bottom = text_grob("Data source: NHANES 2015-2016", color = "blue",
                                   hjust = 1, x = 1, face = "italic", size = 10),
                fig.lab = "Figure 1", fig.lab.face = "bold")

#Figure 2 Visualization of Demographic Proportions
Plot2a <- gg_prop2 %+% demo_prop +
  aes(x = RIDRETH1, y= prop, fill = RIAGENDR) +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title= 'Proportion of Race by Gender', x='Race/Ethnicity', y='proportion')

Plot2b <- ggplot(data=combo) +
  aes(x=RIDAGEYR, y=..density.., fill=factor(RIAGENDR)) +
  geom_histogram(bins=10, position='dodge', alpha=2/3) +
  scale_x_continuous() +
  labs(title='Density of Age by Gender', x='Age (18-39 years)') +
  rremove('legend.title') +
  scale_fill_few('Medium', drop = FALSE)  # keep levels, if data is filtered

figure2 <- ggarrange(Plot2a, Plot2b,
                     labels = c("A", "B"),
                     ncol = 2, nrow = 1 )

annotate_figure(figure2,
                top = text_grob('Demographics of Sample', face='bold',size=14),
                bottom = text_grob("Data source: NHANES 2015-2016", color = "blue",
                                   hjust = 1, x = 1, face = "italic", size = 10),
                fig.lab = "Figure 2", fig.lab.face = "bold")

#Figure 3 Visualization of Demographic Proportions Stratified by Conditions
Plot3a <- gg_prop2 %+%  diet_cond +  
  aes(x = RIDRETH1, y= prop, fill = RIAGENDR) +
  facet_grid(~DBQ700)+ theme(axis.text.x = element_text(angle = 90)) + 
  rremove('ylab') +
  labs(x = 'Race/Ethnicity',title = 'Proportions in Race/Ethnicity by Gender, \n Stratified by Diet')

Plot3b <- gg_prop2 %+% phys_cond +
  aes(x = RIDRETH1, y= prop, fill = RIAGENDR) +
  theme(axis.text.x = element_text(angle = 90)) + 
  facet_grid(~Activity) +
  rremove('ylab') +
  labs(x = 'Race/Ethnicity', title = "Proportions in Race/Ethnicity by Gender, \n Stratified Physical Activity")

Plot3c <- ggplot(data=dietcombo) +
  aes(x=RIDAGEYR, y=..density.., fill=factor(RIAGENDR)) +
  geom_histogram(bins=10, position='dodge', alpha=2/3) +
  scale_x_continuous() +
  labs(title='Densities in Age by Gender, \n Stratified by Diet', x='Age (18-39 years)') +
  rremove('legend.title') +
  facet_wrap(~DBQ700) +
  scale_fill_few('Medium', drop = FALSE)  # keep levels, if data is filtered

Plot3d <- ggplot(data=physcombo) +
  aes(x=RIDAGEYR, y=..density.., fill=factor(RIAGENDR)) +
  geom_histogram(bins=10, position='dodge', alpha=2/3) +
  scale_x_continuous() +
  labs(title='Densities in Age by Gender, \n Stratified by Physical Activity', x='Age (18-39 years)') +
  rremove('legend.title') +
  facet_wrap(~Activity) +
  scale_fill_few('Medium', drop = FALSE)  # keep levels, if data is filtered



figure3 <- ggarrange(Plot3a, Plot3b, Plot3c, Plot3d, 
          labels = c("A", "B", "C","D"),
          ncol = 2, nrow = 2 )

annotate_figure(figure3,
                top = text_grob('Demographics of Sample \n Stratified by Exposure', face='bold',size=14),
                bottom = text_grob("Data source: NHANES 2015-2016", color = "blue",
                                   hjust = 1, x = 1, face = "italic", size = 10),
                fig.lab = "Figure 3", fig.lab.face = "bold")

#Visualization of STI Status by Conditions
Plot4a <- gg_prop2 %+% diet_maleprop +
  aes(x = Positive.Test, y= prop, fill = DBQ700) +
  labs(title = 'Male, Diet', x='STI Status', y='Proportion')

Plot4b <- gg_prop2 %+% diet_femaleprop +
  aes(x = Positive.Test, y= prop, fill = DBQ700) +
  labs(title = 'Female, Diet', x='STI Status',y='Proportion')

Plot4c <- gg_prop2 %+% phys_maleprop +
  aes(x = Positive.Test, y= prop, fill = Activity) +
  labs(title = 'Male, Physical Activity', x='STI Status', y='Proportion')

Plot4d <- gg_prop2 %+% phys_femaleprop +
  aes(x = Positive.Test, y= prop, fill = Activity) +
  labs(title = 'Female, Physical Activity', x='STI Status',y='Proportion')

figure4 <- ggarrange(Plot4a, Plot4b, Plot4c, Plot4d,
                     labels = c("A", "B", "C", "D"),
                     ncol = 2, nrow = 2 )

annotate_figure(figure4,
                top = text_grob('STI Status \n Stratified by Exposure', face='bold',size=14),
                bottom = text_grob("Data source: NHANES 2015-2016", color = "blue",
                                   hjust = 1, x = 1, face = "italic", size = 10),
                fig.lab = "Figure 4", fig.lab.face = "bold")

##############################Tables#########################################
demo_perc <- combo %>% 
  count(Positive.Test, RIAGENDR) %>% 
  mutate(perc = prop.table(n)*100) %>%      # mutate count(n) into perc
  select(-n) %>%                            # remove the count...
  spread(RIAGENDR, perc)                        # to spread perc by subgroup

demo_dist <- combo %>% 
  count(Positive.Test) %>%                            # count for parent groups
  mutate(`(\\%)` = prop.table(n)*100) %>%   # & percentage in those groups
  left_join(demo_perc, by = 'Positive.Test')  


# 1 digit to distinguish % from n and caption
ztab1 <- ztable(as.data.frame(demo_dist), digits = 1
               , caption = 'Distribution of Gender by STI Status')

# name and number column groups
ztab1 <- addcgroup(ztab1, cgroup = c('Status', 'Gender (\\%)') 
                  , n.cgroup = c(2, ncol(demo_dist)-2))   # 3 columns & others

######Below Example on How to use 'tidy data' for Counts and Proportions by M. Devlin#########
dfr <- mtcars %>%             
  mutate(cyl = as.factor(cyl)     # numeric values to factor
         , gear = as.ordered(gear))  # numeric to ordered factor (like a grade)

dfr_prop <- dfr %>% 
  count(cyl, gear) %>%            # group_by() & summarise(n = n()) are implicit
  mutate(prop = prop.table(n))    # prop = n/sum(n) works too

# create a reusable ggplot object *without* data = data.frame()

gg_prop <- ggplot(data = data.frame()
                  , aes(x = cyl, y = prop, fill = gear)) + 
  geom_bar(stat = 'identity', position = 'dodge', alpha = 2/3) +  
  #scale_y_continuous(labels = percent) +    
  scale_fill_few('Medium', drop = FALSE) +              # keep levels, if data is filtered
  labs(x = 'Cylinders', y = NULL, fill = 'Gears'
       , title = 'Proportions in Sub-groups by Group')  # what's the reader looking at?

gg_prop %+%   # use %+% to add...
  dfr_prop    # a dataframe

dfr_perc <- dfr %>% 
  count(cyl, gear) %>% 
  mutate(perc = prop.table(n)*100) %>%      # mutate count(n) into perc
  select(-n) %>%                            # remove the count...
  spread(gear, perc)                        # to spread perc by subgroup

dfr_dist <- dfr %>% 
  count(cyl) %>%                            # count for parent groups
  mutate(`(\\%)` = prop.table(n)*100) %>%   # & percentage in those groups
  left_join(dfr_perc, by = 'cyl')           # join to dfr_perc

# 1 digit to distinguish % from n and caption
ztab <- ztable(as.data.frame(dfr_dist), digits = 1
               , caption = 'Distribution of Gears by Cylinders')

# name and number column groups
ztab <- addcgroup(ztab, cgroup = c('Cylinders', 'Gear Distribution (\\%)') 
                  , n.cgroup = c(3, ncol(dfr_dist)-3))   # 3 columns & others

## using NSE, except for mutate, the last step

fun_prop <- function (data = data.frame(), dots = NULL, ...) {
  
  dfr_prop <- data %>% 
    group_by(.dots = dots) %>% 
    summarise(n = ~n()) %>% 
    mutate(prop = prop.table(n))  # can this be done by NSE with mutate_() ??
  
  dfr_prop
}  

dfr <- dfr %>%             
  mutate(vs = as.logical(vs))     # 0 = Vertical, 1 = Straight

dfr_cond <- dfr %>% 
  fun_prop(c('vs','cyl','gear'))  # piping & position take care of arguments

gg_prop %+% 
  dfr_cond +                # add new data into gg_prop
  facet_grid(~vs) +         # conditional variable defines facets, and re-title
  labs(title = 'Proportions in Sub-groups by Group, \n Conditioned on a 3rd Variable') 


