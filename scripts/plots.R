library(sf)
library(tidyverse)
library(maps)
library(magrittr)
library(data.table)
library(countrycode)
library(here)
source("https://raw.githubusercontent.com/skhiggins/R_guide/master/scripts/programs/set_theme.R")


# Read in data
world_map <- map_data("world")
techcredit <- read_csv(here("proc", "techcredit.csv"))

# Join 
world_map %<>%
  mutate(iso3c = countrycode(region, origin = "country.name", destination = "iso3c")) %>%
  left_join(techcredit %>% filter(year == 2013), by = "iso3c")

# Plot
ggplot(world_map, aes(x = long, y = lat, group = group, fill = big_tech_credit_per_capita)) +
  geom_polygon(color = "white") + 
  scale_fill_viridis_c(option = "C") + 
  theme(legend.position = "bottom")


# Figure 1 in the paper

graph_dat <- techcredit  %>%
  filter(country_name == "Uganda") %>%
  #filter(country_name %chin% c("China", "United States", "Japan", "Korea", "United Kingdom", "Indonesia", "Netherlands", "Russia", "Kenya", "Germany")) %>%
  group_by(year) %>%
  summarise(total_fintech = sum(fintech_credit), 
            total_bigtech = sum(big_tech_credit), 
            total_domestic_fin = sum(total_domestic_credit_by_financial_sector)) %>%
  mutate(alt_share = 100*(total_fintech + total_bigtech)/total_domestic_fin) %>%
  mutate(alt_share = ifelse(alt_share > 10000000000, NA, alt_share)) %>%
  select(year, total_fintech, total_bigtech, alt_share) %>% 
  #filter(alt_share < 100) %>%
  pivot_longer(2:3) %>%
  mutate(value = value/1000) %>%
  group_by(year) %>%
  mutate(total_alt = sum(value)) %>%
  mutate(share = "Total Alternative Credit Share") 

scaling_factor = max(graph_dat$total_alt)
ymax = max(graph_dat$alt_share[!is.na(graph_dat$alt_share)])
test <- graph_dat %>%  
  ggplot() + 
  geom_col(aes(x = as.factor(year), y = value, fill = name)) + 
  geom_line(aes(x = as.factor(year), y = alt_share*scaling_factor, color = share, group = 1))   + 
  geom_point(aes(x = as.factor(year), y = alt_share*scaling_factor, color = share, group = 1))   + 
  scale_fill_manual(values = c("cornflowerblue", "bisque3"), 
                    labels = c("Big Tech", "Fintech")) + 
  scale_y_continuous(
    name = "Billions of USD",
    sec.axis = sec_axis(~./scaling_factor, name="Alternative credit percentage of total credit", 
                        breaks = seq(0,ymax,.1), labels = paste0(seq(0,ymax,.1), "%"))
  ) + 
  scale_color_manual(values = "black") + 
  xlab(element_blank()) + 
  ggthemes::theme_economist_white() + 
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        axis.text.y.right = element_text(hjust  = 1.5))
            




ggplotly(graph_dat %>%  
           ggplot() + 
           geom_col(aes(x = as.factor(year), y = value, fill = name))  + 
           theme(legend.position = "bottom", 
                 legend.title = element_blank(),
                 axis.text.y.right = element_text(hjust  = 1.5)) + 
           xlab(element_blank()) )



alt_share  = (total_fintech + total_bigtech)/total_domestic_credit_by_financial_sector) %>%
  pivot_longer(2:length(.)) %>%
  mutate("Lending volume" = name) %>%
  ggplot(aes(x = as.factor(year), y = value, fill = `Lending volume`)) + 
  geom_col(position = "stack") + 
  scale_fill_manual(values = c("cornflowerblue", "bisque3"), 
                    labels = c("Big Tech", "Fintech")) + 
  xlab(element_blank()) + 
  ylab("Billions of USD") + 
  ggthemes::theme_economist_white() + 
  theme(legend.position = "bottom")


techcredit %>% 
  filter(year == "Algeria") %>%
  group_by(year) %>%
  summarise(total_fintech = sum(fintech_credit), 
            total_bigtech = sum(big_tech_credit)) %>%
  pivot_longer(2:length(.)) %>%
  mutate(value = value/1000) %>%
  mutate("Lending volume" = name) %>%
  ggplot(aes(x = as.factor(year), y = value, fill = `Lending volume`)) + 
  geom_col(position = "stack") + 
  scale_fill_manual(values = c("cornflowerblue", "bisque3"), 
                    labels = c("Big Tech", "Fintech")) + 
  xlab(element_blank()) + 
  ylab("Billions of USD") + 
  ggthemes::theme_economist_white() + 
  theme(legend.position = "bottom")

library(directlabels)

techcredit %>%
  group_by(year, country_name) %>%
  filter(country_name %in% c("Israel", "Poland")) %>%
  pivot_longer(4:9)  %>%
  # mutate(percapita = ifelse(name %in% c("big_tech_credit_per_capita", "fintech_credit_per_capita"), "yes", "no")) %>%
  filter(name == "fintech_credit_per_capita")  %>%
  ggplot(aes(x = year, y = value, color = country_name)) + 
  geom_line() + 
  set_theme(legend_position = "bottom")  +
  geom_dl(aes(label=iso3c), method="last.bumpup") 


techcredit %>% 
  ggplot(aes(x = total_alternative_credit, y = total_domestic_credit_by_financial_sector)) + 
  geom_point()


  
