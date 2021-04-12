#### ENV 603 / 5-April-2021 / N.R. Sommer
# Dataset 1: Religion by region

# If you have not yet installed these libraries, use install.packages("")

### A NEW NOTE### 

library(tidyverse)
library(socviz)

# Create a new table called rel_by_region

rel_by_region <- gss_sm %>%
  group_by(bigregion, religion) %>%
  summarize(N = n()) %>%
  mutate(freq = N / sum(N),
         pct = round((freq*100), 0))

# See how the pipeline above took the gss_sm dataframe and transformed it into a summary table.

View(gss_sm)
View(rel_by_region)

# Now let's make some plots!

p1 <- ggplot(rel_by_region, aes(x = bigregion, y = pct, fill = religion)) + 
  geom_col(position = "dodge2") +
  labs(x = "Region",y = "Percent", fill = "Religion") +
  theme(legend.position = "top")

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

#Here is my modifications for the second plot (p2)
#To order the bars, the first thing I did is to replace the NA field to a category called "NA"
rel_by_region$religion <- as.character(rel_by_region$religion)
rel_by_region$religion[is.na(rel_by_region$religion)] <- "N/A"

#Use RcolorBrewer package to choose a new color scheme
library(RColorBrewer)

p2_b <- ggplot(rel_by_region, aes(x = reorder(religion, pct), y = pct, fill = religion)) +
  geom_col(position = "dodge2") +
  labs(x = NULL, y = "Percentage", fill = "Religion") +
  scale_y_continuous(labels=function(x) format(x, scientific = FALSE), expand = c(0,0), limits = c(0,100)) +
  geom_text(aes(label=pct), hjust=-0.5) +
  guides(fill = FALSE) + 
  ggtitle("Religion Affiliation in U.S. by Regions",subtitle =  "Protestant is the largest religious group across the country ") + 
  theme(plot.subtitle = element_text(colour="gray40"), #change subtitle font 
        axis.text.y = element_text(size = 11, colour="black"),
        axis.ticks.y = element_blank(),
        axis.line.y = element_line(size = 0.55, colour ="darkgray"),
        panel.grid.major = element_blank(), #remove grids
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), #remove background color
        plot.margin = unit(c(1,1,1,1), "cm"),
        panel.spacing.x = unit(2, "lines")) + #add spaces between each facet so we can see the entire axis 
  coord_flip() + 
  facet_grid(~ bigregion)

p2_b + scale_fill_brewer(palette = "Dark2")

# Once you're happy with your changes, save your plot:
ggsave("Exercise1_Plot2_Revised.png",
  plot = last_plot(),
  dpi = 300)
