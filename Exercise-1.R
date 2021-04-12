#### ENV 603 / 5-April-2021 / N.R. Sommer
# Dataset 1: Religion by region

# If you have not yet installed these libraries, use install.packages("")

library(tidyverse)
library(socviz)

# Create a new table called rel_by_region

rel_by_region <- gss_sm %>%
  group_by(bigregion, religion) %>%
  summarize(N = n()) %>%
  mutate(freq = N / sum(N),
         pct = round((freq*100), 0))

# See how the pipeline above has taked the gss_sm dataframe and transformed it into a summary table.

View(gss_sm)
View(rel_by_region)

# Now let's make some plots!

p1 <- ggplot(rel_by_region, aes(x = reorder(bigregion,pct), y = pct, fill = religion)) + #have reordered the data here by pct
  geom_col(position = "dodge2") +
  ggtitle("Religious Affiliation Breakdown in the US by region", subtitle="There are more Jews in the Northeast than anywhere in the US!") +
  labs(x = "Region",y = "Percent", fill = "Religion") +
  scale_y_continuous(breaks=seq(0,70,30)) + #changing the gridlines
  scale_fill_brewer(palette = "Set3") + #changing the colour palette for the bars
  theme(legend.position = "top") +
  theme_light() #changing the chart theme

p1

p2 <- ggplot(rel_by_region, aes(x = reorder(religion,pct), y = pct, fill = religion)) +
  geom_col(position = "dodge2") +
  ggtitle("Religious Affiliation Breakdown in the US by region", subtitle="There are more Protestants in the South than anywhere in the US!") +
  labs(x = NULL, y = "Percent", fill = "Religion") +
  #scale_y_discrete(breaks = seq(0,70,40)) +
  #scale_x_discrete(breaks = seq(0,70,40)) +
  guides(fill = FALSE) + 
  coord_flip() + 
  facet_grid(~ bigregion) +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal()

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
