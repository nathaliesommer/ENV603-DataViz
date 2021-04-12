#### ENV 603 / 5-April-2021 / N.R. Sommer
#### Homework completed by Francis Commercon
# Dataset 1: Religion by region

# If you have not yet installed these libraries, use install.packages("")

### A NEW NOTE### 

library(tidyverse)
library(socviz)
library(RColorBrewer)

# Create a new table called rel_by_region

rel_by_region <- gss_sm %>%
  group_by(bigregion, religion) %>%
  summarize(N = n()) %>%
  mutate(freq = N / sum(N),
         pct = round((freq*100), 0))

# See how the pipeline above took the gss_sm dataframe and transformed it into a summary table.

View(gss_sm)
View(rel_by_region)
display.brewer.all(colorblindFriendly = TRUE)

# Now let's make some plots!
rel_by_region$religion_reordered <- factor(rel_by_region$religion, levels = c("Protestant",
                                                                              "Catholic",
                                                                              "None",
                                                                              "Other",
                                                                              "Jewish",
                                                                              "NA"))

rel_by_region$region_reordered <- factor(rel_by_region$bigregion, levels = c("South",
                                                               "Midwest",
                                                               "West",
                                                               "Northeast"))

p1 <- ggplot(rel_by_region, aes(x = region_reordered, y = pct, fill = religion_reordered)) + 
  geom_col(position = "dodge2") +
  labs(title = "Most graduates are Protestant \nespecially in the South",
       x = "Region", 
       y = "Percent", 
       fill = "Religion",
       legend = "Religion") +
  theme(legend.position = "right",
        panel.grid.major.y = element_line(colour = "gray"),
        panel.grid.major.x = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        axis.ticks = element_blank()) +
  scale_fill_brewer(palette = "Dark2")
p1

#the instructions say to do "either" plot. So you don't need me to do both plots, right?

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
ggsave("Commercon_Assignment_9_1.png",
  plot = last_plot(),
  dpi = 300)
