#### Zooplankton EDA

## Set-up
library(tidyverse)


DATA = file.path(getwd(), "Data")

# load data
zoo_counts_raw = read.csv("Data//zoopl_counts.csv", header = T, stringsAsFactors = T)

adults = zoo_counts_raw %>% 
  select(Acartia, Centropages, Paracalanus, Paracalanus_like, Clausocalanus, 
         Temora, Isias_clavipes,Isias_clavipes.like, Corycaeus, Oithona_red_feather,
         Oithona_similis.like, Oithona_nana.like, Oithona_somenthing, Cyclopidae, 
         Harpacticoida, Clytemnestra.like, Microsetella.like, Euterpina) %>% 
  
  mutate(Adults = rowSums(.))

sum_zoo = zoo_counts_raw %>% 
  mutate(Juveniles = rowSums(select(., ends_with("juv")))) %>% 
  select(Day, Mesocosm, Volume, Juveniles, Nauplii, Eggs)
         
sum_zoo$Adults = adults$Adults
rm(adults)

abd_zoo = sum_zoo[2:109,] %>% 
  pivot_longer(cols = c(Juveniles, Nauplii, Eggs, Adults)) %>% 
  mutate(time = str_remove(Day, ".") %>%  # make Day a number - if needed
           as.integer()) %>%
  mutate(Abund = value*1000/Volume) %>% 
  select(-Volume, -value) %>% 
  mutate(Treatment = gsub("[[:digit:]]", "", Mesocosm),
         Treatment = factor(Treatment, levels = c("C", "A", "P", "B"))) %>%    # NOT THE BEST WAY TO DO IT  
  mutate(Replicate = gsub("[^[:digit:]]", "", Mesocosm)) %>% 
  pivot_wider(names_from = name, values_from = Abund, values_fn = list(counts = sum))

g = sprintf("%02d",abd_zoo$time)
abd_zoo$nDay=paste0("T", g)


ggplot(abd_zoo, aes(x = nDay, y = Eggs))+
  geom_boxplot(aes(fill = Treatment))

B_zoo = abd_zoo %>% 
  filter(Treatment %in% c("C", "B"))

AP_zoo = abd_zoo %>% 
  filter(Treatment %in% c("C", "A", "P"))


#Stat
stat_AP = AP_zoo %>% 
  select(-Mesocosm, -Replicate, -Day) %>% 
  rstatix::convert_as_factor(nDay) %>% 
  pivot_longer(cols = c(Cops, Juveniles, Nauplii, Eggs, Adults)) %>% 
  mutate(ID = rownames(.))

stat_B = B_zoo %>% 
  select(-Mesocosm, -Replicate, -Day) %>% 
  rstatix::convert_as_factor(nDay) %>% 
  pivot_longer(cols = c(Cops, Juveniles, Nauplii, Eggs, Adults)) %>% 
  mutate(ID = rownames(.))

res.aov.ap <- anova_test(
  data = stat_AP, dv = value, wid = ID,
  within = c(Treatment, nDay)
)