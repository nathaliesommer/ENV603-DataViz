#### ENV 603 / 5-April-2021 / N.R. Sommer
# Dataset 1: Religion by region

# If you have not yet installed these libraries, use install.packages("")

#Installed Socviz package

install.packages("socviz")
library(tidyverse)
library(socviz)

# Create a new table called rel_by_region

#Removed extra u in gss_sum

rel_by_region <- gss_sm %>%
  group_by(bigregion, religion) %>%
  summarize(N = n()) %>%
  mutate(freq = N / sum(N),
         pct = round((freq*100), 0))

# See how the pipeline above has taked the gss_sm dataframe and transformed it into a summary table.

View(gss_sm)
View(rel_by_region)

# Now let's make some plots!

#Reordered by big region. Had trouble reordering within each region, but I still like how it looks. 
p1 <- ggplot(rel_by_region, aes(x = reorder(bigregion, -pct), y = pct, fill = religion)) + 
  geom_col(position = "dodge2") +
  
  #Changed Colors
  scale_fill_brewer(palette="Greens") +
  
  # Added a title and fixed y-axis label
  labs(x = "Region",y = "Percent of Region", fill = "Religion", title = "Northeastern US is Only Region Without Plurality of Protestants") +
  theme(legend.position = "top") +
  
  #Removed Grid lines and Grey Background
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

  #Moved Save Options Up

  ggsave("Exercise1Plot1.png",
       plot = last_plot(),
       dpi = 300)

p1

p2 <- ggplot(rel_by_region, aes(x = religion, y = pct, fill = religion)) +
  geom_col(position = "dodge2") +
  labs(x = NULL, y = "Percent", fill = "Religion") +
  guides(fill = FALSE) + 
  coord_flip() + 
  facet_grid(~ bigregion)

p2

# Make modifications to either plot. Google is your friend here. A few suggestions:
# (1) Add a title
# (2) Remove the gridlines
# (3) Reorder the bars
# (4) Choose a new color scheme

# Once you're happy with your changes, save your plot:
ggsave("plot1.png",
  plot = last_plot(),
  dpi = 300)