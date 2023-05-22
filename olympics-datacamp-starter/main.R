### load libraries and data ===============================================

# load tidyverse library 
# note: ggplot2 is comprised in tidyverse library
library(tidyverse)
options(scipen=999)

# define filename
fn1 = 'data/athlete_events.csv'
fn2 = 'data/noc_regions.csv'

# read data
da = read_csv(file = fn1) %>%
  left_join(read_csv(file = fn2))

tmp = unique(da) 
### data wrangling ========================================================

da %>% select(Age, Sport) # select Age and Sport
da %>% select(-Age) # suppress Age
da %>% mutate(Age2 = Age^2) # create new variable, Age squared, called Age2
da %>% filter(Age > 40) # filter on rows by Age value

# compute the number of instances of Year (with *count* method)
da %>% count(Year)

# compute the number of instances of Year (without relying on *count* method)
da %>% 
  group_by(Year) %>% 
  summarise(n_participants=n(), 
            n_countries=n_distinct(NOC),
            mean_age = mean(Age, na.rm=TRUE), 
            sd_age = sd(Age, na.rm=TRUE))

# select 10 lines with highest age values, focusing then on variables Sport, Age, Year
da %>% 
  slice_max(Age, n = 4) %>% 
  select(Sport, Name, NOC, Age, Year)

# trying out kmeans ====================================

# drop NA values of interesting variables
da_ <- da %>% drop_na(Age, Weight, Height) 

# prepare matrix for kmeans
da_num <- da_ %>% select(Age, Weight, Height) %>% as.matrix() 

# run kmeans algorithm
km <- kmeans(da_num, centers = 5)

# add to dataframe the cluster index
da_ <- da_ %>% mutate(clusterIndex = km$cluster)

# check distribution of clusters
da_ %>% count(clusterIndex, name = 'n') %>% mutate(prop = n/sum(n))

# check summaries by clusters
da_ %>%
  group_by(clusterIndex) %>%
  summarise(mean(Age), mean(Weight), mean(Height), mean(Sex=='M'))
  
# view sports represented in clusters
da_ %>%
  group_by(clusterIndex, Sport) %>%
  summarise(n = n(), .groups='drop') %>%
  pivot_wider(id_cols = Sport, names_from = clusterIndex, values_from = n)
da_ %>%
  group_by(clusterIndex, Sport) %>%
  summarise(n = n(), .groups='drop') %>%
  group_by(Sport) %>%
  mutate(prop = n/sum(n), .groups='drop') %>% 
  pivot_wider(id_cols = Sport, names_from = clusterIndex, values_from = prop)


round(100 * table(km$cluster) / nrow(da_num))
km$centers

da

he = da %>%
  filter(Sport == "Judo", Sex == "M") %>%
  select(Height, Weight, Age)
ggplot(data=he) + 
  aes(x=Weight, y=Height) +
  geom_point()
he %>%
  mutate(AgeCateg = cut(Age, breaks=seq(16, 43, length.out = 4))) %>%
ggplot() + 
  aes(x=Weight, y=Height, col=AgeCateg) +
  geom_point() +
  geom_smooth(method='lm') +
  scale_color_viridis_d() +
  facet_wrap(~AgeCateg)
