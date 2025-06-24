library(tidyverse)

setwd("../data")

temps <- read.csv("MN_temps.csv")
temps$yrmo =ym(temps$Year.month)
temps$year = year(temps$yrmo)
temps$month = month(temps$yrmo)

summary(temps)

ggplot(temps, aes(x=yrmo)) +
  geom_line(aes(y=Max), col="red") +
  geom_line(aes(y=Min), col="blue")

temps %>% dplyr::filter(year==1940 | year==2020) %>% 
  ggplot(aes(x=month, linetype=as.factor(year))) +
  geom_line(aes(y=Max), col="red") + 
  geom_line(aes(y=Min), col="blue") 

temps_tosave = temps %>% 
  select(year_month = yrmo, 
         year,
         month, 
         max_temp_f = Max,
         min_temp_f = Min)

write.csv(temps_tosave, "MN_temps_clean.csv")

#########################################################################
#### edit mercurcy fish dfs 
#########################################################################

library(tidyverse)
setwd("GitHub/EJ-DS/Mod1_Fish/")

merc1 = read.csv("fish_1998.csv") %>% mutate(year=1998) %>% select(-X)
merc2 = read.csv("fish_2023.csv") %>% mutate(year=2023) %>% select(-X)

head(merc1)
head(merc2)

all_fish = rbind(merc1, merc2)

# sample plots 
ggplot(all_fish, aes(x=year, y=mercury, group=year)) + 
  geom_boxplot(aes(fill=as.factor(year)))

ggplot(all_fish, aes(x=length, y=mercury)) +
  geom_point(aes(col=as.factor(year)))

write.csv(all_fish, "../../../data/MN/data/fish_samples.csv", row.names = F)

#####################################################################
## water quality time series 
####################################################################

setwd("../Mod2_water/")

streams = read.csv("streams_data.csv") %>% select(-X)
head(streams)

write.csv(streams, "../../../data/MN/data/streams_data.csv", row.names = F)

#####################################################################
## birds of MN 
#####################################################################

setwd("data/MN")

birds = read.csv("Minnesota Birds.csv")

head(birds)

ggplot(birds, aes(y=Category)) + geom_bar()

ggplot(birds, aes(y=Conservation.status)) + geom_bar()

birds %>% filter(Category=="Hummingbirds") %>% 
  ggplot(aes(x=Body.Mass.Min..g., y=Wingspan.Min..cm., col=Category)) +
  geom_point()

ggplot(birds, aes(x=Body.Mass.Min..g., y=Wingspan.Min..cm.)) + geom_point()

#########################################################
# inaturalist fish species threatened 
#########################################################

# inaturalist - query was: 
#   
#   quality_grade=research&
#   identifications=any&
#   iconic_taxa[]=Actinopterygii&
#   place_id=124854&
#   verifiable=true&
#   threatened=true&
#   introduced=false&
#   native=true&
#   popular=true

# https://www.inaturalist.org/pages/search+urls#taxon-status

inat = read.csv("MN_data_prep/")

unique(inat$common_name)

ggplot(inat, aes(y=common_name, fill=place_state_name)) + geom_bar()

inat_tab = inat %>% select(place_state_name, common_name) %>% 
  table() %>% data.frame() %>% 
  filter(place_state_name == "Minnesota" |
           place_state_name == "Wisconsin") %>% 
  rename(count = Freq)

ggplot(inat_tab, aes(x=place_state_name, y=count)) +
  geom_boxplot()

ggplot(inat_tab, aes(x=place_state_name, 
                     y=common_name,
                     fill=count)) +
  geom_raster()

ggplot(inat_tab, aes(y=common_name, 
                     x=count, 
                     fill=place_state_name)) +
  geom_col()