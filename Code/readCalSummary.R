library(tidyverse)
library(googlesheets4)
library(here)
library(fs)

dir_create(here(),c("Figs"))

# Load data from Google Sheets
cal <- read_sheet("https://docs.google.com/spreadsheets/d/1wE-CLEnHa_wNspp9QBUuoT-ItvOiePUIR-9ciDcXANE/edit?gid=487864540#gid=487864540",
                          sheet = "summary-KLS")

cal.setup <- read_sheet("https://docs.google.com/spreadsheets/d/1wE-CLEnHa_wNspp9QBUuoT-ItvOiePUIR-9ciDcXANE/edit?gid=487864540#gid=487864540",
                  sheet = "setup-KLS")

# Create labels for plot panels
cal.setup.summ <- cal.setup %>% 
  group_by(group) %>% 
  summarise(g1 = min(setup), 
            g2 = max(setup)) %>% 
  mutate(label = paste(g1,"vs.",g2))

# Add info to cal data
cal <- cal %>% 
  left_join(select(cal.setup, setup, group, sphere_num)) %>% 
  left_join(select(cal.setup.summ, group, label))

# Set plotting theme
theme_set(theme_bw())

# Plot comparisons
## Gain
cal.gain <- ggplot(cal, aes(factor(frequency), gain, group = setup, colour = sphere_num)) + 
  geom_line(show.legend = TRUE) + geom_point(show.legend = TRUE) +
  facet_wrap(~label) +
  scale_colour_discrete(name = "Sphere") +
  labs(title = "Gain",
       x = "Freqency (kHz)",
       y = "Gain (dB)")

ggsave(cal.gain, 
       filename = here("Figs/cal_comp_gain.png"))

# Gain adjustment
cal.gain.adj <- ggplot(cal, aes(factor(frequency), gain_adj, group = setup, colour = sphere_num)) + 
  geom_line(show.legend = TRUE) + geom_point(show.legend = TRUE) +
  facet_wrap(~label) +
  scale_colour_discrete(name = "Sphere") +
  labs(title = "Gain adjustment",
       x = "Freqency (kHz)",
       y = "Gain (dB)")

ggsave(cal.gain.adj, 
       filename = here("Figs/cal_comp_gain_adj.png"))
