#### ENV 603 / 5-April-2021 / N.R. Sommer
# Dataset 3: U.S. Presidential Election History

# Sometimes we want to plot labels along points in a scatterplot, or to plot informative labels directly
ggplot(data = by_country,
       mapping = aes(x = roads_mean, y = donors_mean)) + 
  geom_point() + 
  geom_text(mapping = aes(label = country), hjust = 1)
# This looks terrible. Let's adjust the position of the text. Within the geom_text(), add the argument for hjust=1

# This still looks terrible. You could continue to mess around with values for hjust and get there eventually. Instead, let's call up a new package and explore how to add better labels to plots using a new dataset
install.packages("ggrepel")
library(ggrepel)

head(elections_historic)

# We're going to plot the winner's share of the popular vote against the winner's share of the electoral college vote. 

ggplot(elections_historic, aes(x = popular_pct, y = ec_pct,
                               label = winner_label)) + 
  geom_hline(yintercept = 0.5, size = 1.4, color = "gray80") +
  geom_vline(xintercept = 0.5, size = 1.4, color = "gray80") +
  geom_point() +
  geom_text_repel() +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Winner's share of popular vote", 
       y = "Winner's share of electoral college vote", 
       title = "Presidential Elections: Popular & Electoral College Margins", 
       subtitle = "1824-2016")

# Now, uncomment the geom_text_repel() above and replot. See what happens to the labels if you adjust the size of your plot window in RStudio. 
# Labels appear as they have room to based on the window size!

# It's messy, but all points are labeled without tremendous overlaps. For this plot, we might be more interested in how far away each point is from the 50% threshold on both axis, or in the outcome of a particular election. So, it might make more sense to pick out points of interest in the data instead of labeling every single point. And this is where geom_text_repel() is powerful.

ggplot(elections_historic, aes(x = popular_pct, y = ec_pct,
                               label = winner_label)) + 
  geom_hline(yintercept = 0.5, size = 1.4, color = "gray80") +
  geom_vline(xintercept = 0.5, size = 1.4, color = "gray80") +
  geom_point() +
  geom_text_repel(data = subset(elections_historic,
                                ec_pct < 0.7 & popular_pct < 0.4)) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Winner's share of popular vote", 
       y = "Winner's share of electoral college vote", 
       title = "Presidential Elections: Popular & Electoral College Margins", 
       subtitle = "1824-2016")

ggplot(elections_historic, aes(x = popular_pct, y = ec_pct,
                               label = winner_label)) + 
  geom_hline(yintercept = 0.5, size = 1.4, color = "gray80") +
  geom_vline(xintercept = 0.5, size = 1.4, color = "gray80") +
  geom_point() +
  geom_text_repel(data = subset(elections_historic,
                                year %in% "1912")) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Winner's share of popular vote", 
       y = "Winner's share of electoral college vote", 
       title = "Presidential Elections: Popular & Electoral College Margins", 
       subtitle = "1824-2016")

# Sometimes we want to annotate a figure directly. Let's use annotate() to add text.

ggplot(elections_historic, aes(x = popular_pct, y = ec_pct,
                               label = winner_label)) + 
  geom_hline(yintercept = 0.5, size = 1.4, color = "gray80") +
  geom_vline(xintercept = 0.5, size = 1.4, color = "gray80") +
  geom_point() +
  geom_text_repel(data = subset(elections_historic,
                                year %in% "1912")) +
  annotate(geom="text", x = .47, y = .9,
           label = "Wilson won just 41.8% of the popular vote",
           fontface = "bold",
           color = "red",
           hjust=1) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic() + # removes gridlines
  labs(x = "Winner's share of popular vote", 
       y = "Winner's share of electoral college vote", 
       title = "Presidential Elections: Popular & Electoral College Margins", 
       subtitle = "1824-2016",
       caption = "Wilson took advantage of a Republican split, winning 40 states with just 41.8% of the popular vote.") # adds a caption

# annotate() can be used for other geoms too, including rectangles, line segments, and arrows

ggplot(elections_historic, aes(x = popular_pct, y = ec_pct,
                               label = winner_label)) + 
  geom_hline(yintercept = 0.5, size = 1.4, color = "gray80") +
  geom_vline(xintercept = 0.5, size = 1.4, color = "gray80") +
  geom_point() +
  geom_text_repel(data = subset(elections_historic,
                                year %in% "1912" |
                                winner %in% "John Quincy Adams")) +
  annotate(geom="text", x = .3, y = .93,
           label = "Wilson still performed better than \n Adams' 30% share of the popular vote.",
           hjust=0, 
           fontface="italic", 
           color = "red") +
  annotate(geom = "segment", 
           x = .41, xend = .32,
           y = .78, yend = .37, 
           colour = "red",
           alpha = .4,
           arrow =arrow()) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic() + # removes gridlines
  labs(x = "Winner's share of popular vote", 
       y = "Winner's share of electoral college vote", 
       title = "Presidential Elections: Popular & Electoral College Margins", 
       subtitle = "1824-2016",
       caption = "Wilson took advantage of a Republican split, winning 40 states with just 41.8% of the popular vote.") # adds a caption


# Armed with the numerous examples from above, and using the elections_historic dataset, make a new graph that:
# (1) Highlights at least two outliers, either using text or color.
# (2) Includes annotated text
# (3) Includes an annotated shape
# Be sure to update the title and caption accordingly. 

ggplot(elections_historic, aes(x = popular_pct, y = ec_pct,
                               label = winner_label)) + 
  geom_hline(yintercept = 0.5, size = 1.4, color = "gray80") +
  geom_vline(xintercept = 0.5, size = 1.4, color = "gray80") +
  geom_point(aes(color = year)) + # adding the year dimension via color of the dots, just to show there is not a clear historical trend over the years
  geom_text_repel(data = subset(elections_historic,
                                year %in% "1912" | year %in% "2016" | # adding 2016 election annotation for context
                                  winner %in% "John Quincy Adams")) +
  annotate(geom="text", x = .31, y = .97,
           label = "19 presidential elections have been won\nwith less than 50% of the popular vote.",
           hjust=0, 
           fontface="bold", 
           color = "steelblue") + # adding caption for context of shape annotation
  annotate(geom = "rect", 
           xmin = .29, xmax = .5,
           ymin = .29, ymax = 1, 
           colour = "blue",
           alpha = 0) + # adding rectangle around all points to the left of the 50% popular vote line, meaning the election was won without a majority of the popular vote
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic() + # removes gridlines
  labs(x = "Winner's share of popular vote", 
       y = "Winner's share of electoral college vote", 
       title = "Electoral college victories without the popular vote are not a new phenomenon", #my new main point of the graph
       subtitle = "All presidential elections, 1824-2016") + 
  theme(plot.title = element_text(face = "bold"), plot.subtitle = element_text(face = "italic"))
       # caption = "Electoral college victories without the popular vote are not a new phenomenon.")



# Save your plot: 

ggsave("NMW HW9 Exercise 3 Plot.png",
       plot = last_plot(),
       dpi = 500)
