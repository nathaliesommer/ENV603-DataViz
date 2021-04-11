#### ENV 603 / 5-April-2021 / N.R. Sommer
# Dataset 1: Religion by region

# If you have not yet installed these libraries, use install.packages("")

library(tidyverse)
library(socviz)

# Create a new table called rel_by_region
# Had to correct a typo 'gss_sum' to gss_sm

rel_by_region <- gss_sm %>%
  group_by(bigregion, religion) %>%
  drop_na(religion) %>%
  summarize(N = n()) %>%
  mutate(freq = N / sum(N),
         pct = round((freq*100), 0))

# See how the pipeline above has taken the gss_sm dataframe and transformed it into a summary table.
# Similar typo correction made in the below code

View(gss_sm)
View(rel_by_region)

# Now let's make some plots!

p1 <- ggplot(rel_by_region, aes(x = bigregion, y = pct, fill = religion)) + 
  geom_col(position = "dodge2") +
  labs(x = "Region",y = "Percent", fill = "Religion") +
  theme(legend.position = "top")

p1

#re-ordering the order of the bars so the largest are on top and Other and None come after specific religions
rel_by_region$religion <- factor(rel_by_region$religion,levels = c( "None", "Other", "Jewish", "Catholic", "Protestant"))

p2 <- ggplot(rel_by_region, aes(x = religion, y = pct, fill = religion)) +
  geom_col(position = "dodge2") +
  labs(x = NULL, y = "Percent", fill = "Religion") +
  guides(fill = FALSE) + 
  coord_flip() + 
  facet_grid(~ bigregion) + 
  
  #summarizing the main takeway from the graph
  ggtitle("Protestants outnumber other religious groups in most US regions") + 
  
  #white background instead of gray
  theme_bw() +
  
  #recoloring the bars to be easier on the eyes
  scale_fill_brewer(palette="Accent") +
  
  #removing gridlines
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

p2

# Make modifications to either plot. Google is your friend here. A few suggestions:
# (1) Add a title
# (2) Remove the gridlines
# (3) Reorder the bars
# (4) Choose a new color scheme

# Once you're happy with your changes, save your plot:
ggsave("NMW HW9 Exercise 1 Plot.png",
  plot = last_plot(),
  dpi = 500)
