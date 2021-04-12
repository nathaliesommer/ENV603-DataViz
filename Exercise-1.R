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
  labs(x = "Region",y = "Percent", fill = "Religion", 
       title = "Predominant Religions by Region Across the United States") +
  theme(legend.position = "top")

p1

my_data <- as_tibble(rel_by_region)
my_data %>% arrange(religion)
my_data

my_data %>% arrange(desc(religion))

ggplot(rel_by_region, aes(x = religion, y = pct, fill = religion)) +
  geom_col(position = "dodge2") +
  labs(x = NULL, y = "Percent", fill = "Religion",
       title = "Predominant Religions by Region Across the United States",
       subtitle = "Catholics edge out Protestants in the NE") +
  guides(fill = FALSE) + 
  coord_flip() + 
  theme_bw() + theme(strip.background  = element_blank(),
                      panel.grid.major = element_line(colour = "grey80"),
                      panel.border = element_blank(),
                      panel.grid.minor.x=element_blank(),
                      panel.grid.major.x=element_blank() ) +
  theme(legend.position="bottom") +
  facet_grid(~ bigregion) 


p2

# Make modifications to either plot. Google is your friend here. A few suggestions:
# (1) Add a title
# (2) Remove the gridlines
# (3) Reorder the bars
# (4) Choose a new color scheme

# Once you're happy with your changes, save your plot:
ggsave("Wraithwall_plot1.png",
  plot = last_plot(),
  dpi = 300)
