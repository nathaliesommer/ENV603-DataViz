#### ENV 603 / 11-April-2021 / Aymane Eddahmani
# Dataset 1: Religion by region


library(tidyverse)
library(socviz)
library(RColorBrewer)
library(viridis)

# Create a new table called rel_by_region
rel_by_region <- gss_sm %>%
  group_by(bigregion, religion) %>%
  summarize(N = n()) %>%
  mutate(freq = N / sum(N),
         pct = round((freq*100), 0))



View(gss_sm)
View(rel_by_region)

# Now let's make some plots!



#reordering the levels:
rel_by_region$religion <- factor(rel_by_region$religion, levels = c("Protestant", "Catholic", "None", "Jewish","Other"))

#changed the color scale. grid lines, and general formatting 
p1 <- rel_by_region %>% filter(!is.na(religion))  %>%  ggplot(aes(x = bigregion, y = pct, fill = religion)) + 
  geom_col(position = "dodge2") +
  theme_minimal()+
  labs(x = "Region",y = "Percentage", fill = "Religion", title = "Most Americans in all major US regions affiliate with a religion") +
  theme(legend.position = "right",panel.grid.major.x = element_blank())+
  scale_fill_viridis(discrete= TRUE)
p1
ggsave("plot1.png",
       plot = last_plot(),
       dpi = 300)

p2 <- ggplot(rel_by_region, aes(x = religion, y = pct, fill = religion)) +
  geom_col(position = "dodge2") +
  labs(x = NULL, y = "Percent", fill = "Religion") +
  guides(fill = FALSE) + 
  coord_flip() + 
  facet_grid(~ bigregion)

p2
