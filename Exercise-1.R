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

p1 <- ggplot(rel_by_region, aes(x = bigregion, y = pct, fill = religion)) + 
  geom_col(position = "dodge2") +
  scale_fill_manual(values = c("Protestant" = "#999999",
                               "Catholic" = "#E69F00",
                               "Jewish" = "#F0E442",
                               "None" = "#0072B2",
                               "Other" = "#D55E00",
                               "NA" = "#CC79A7")) + #I chose this palette because it's colorblind-friendly
  labs(x = "Region",y = "Percent of Population", fill = "Religion") +
  theme(legend.position = "right", panel.background = element_blank())

p1 <- p1 + labs(title = "West Coasters Least Likely to List Religious Affiliation",
                subtitle = "Regional Variations Across the United States") #this adds a title and subtitle
p1

p1

#Comenting out p2 so I can focus on p1!

# p2 <- ggplot(rel_by_region, aes(x = religion, y = pct, fill = religion)) +
# geom_col(position = "dodge2") +
# labs(x = NULL, y = "Percent", fill = "Religion") +
# guides(fill = FALSE) + 
# coord_flip() + 
# facet_grid(~ bigregion)

# p2

# Make modifications to either plot. Google is your friend here. A few suggestions:
# (1) Add a title
# (2) Remove the gridlines
# (3) Reorder the bars
# (4) Choose a new color scheme

#Notes on changes: 
# I added a title, removed the gridlines, and chose a new color scheme
# In addition, I moved the legend to the right
# I wasn't able to figure out how to reorder the bars while keeping them bucketed--something to keep experimenting with!

# Once you're happy with your changes, save your plot:
ggsave("plot1.png",
       plot = last_plot(),
       dpi = 300)