library(tidyverse)
library(googlesheets4)

# Load data from Google Sheets
cal <- read_sheet("https://docs.google.com/spreadsheets/d/1wE-CLEnHa_wNspp9QBUuoT-ItvOiePUIR-9ciDcXANE/edit?gid=487864540#gid=487864540",
                          sheet = "summary-KLS")

cal.setup <- read_sheet("https://docs.google.com/spreadsheets/d/1wE-CLEnHa_wNspp9QBUuoT-ItvOiePUIR-9ciDcXANE/edit?gid=487864540#gid=487864540",
                  sheet = "setup-KLS")

cal.setup.summ <- cal.setup %>% 
  group_by(group) %>% 
  summarise(g1 = min(setup), 
            g2 = max(setup)) %>% 
  mutate(label = paste(g1,"vs.",g2))

cal <- cal %>% 
  left_join(select(cal.setup, setup, group, sphere_num)) %>% 
  left_join(select(cal.setup.summ, group, label))

theme_set(theme_bw())

# Plot gain
cal.gain <- ggplot(cal, aes(factor(frequency), gain, group = setup, colour = sphere_num)) + 
  geom_line(show.legend = TRUE) + geom_point(show.legend = TRUE) +
  facet_wrap(~label) +
  scale_colour_discrete(name = "Sphere") +
  labs(title = "Gain",
       x = "Freqency (kHz)",
       y = "Gain (dB)")

# Plot gain
cal.gain.adj <- ggplot(cal, aes(factor(frequency), gain_adj, group = setup, colour = sphere_num)) + 
  geom_line(show.legend = TRUE) + geom_point(show.legend = TRUE) +
  facet_wrap(~label) +
  scale_colour_discrete(name = "Sphere") +
  labs(title = "Gain adjustment",
       x = "Freqency (kHz)",
       y = "Gain (dB)")
