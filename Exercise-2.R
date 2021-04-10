#### ENV 603 / 5-April-2021 / N.R. Sommer
# Dataset 2: Organ transplants for OECD countries

# We'll start by naively graphing some of this data. Take a look at a scatterplot of organ donors vs time. 
ggplot(data = organdata,
       mapping = aes(x = year, y = donors)) + 
  geom_point()

# What does the error message mean here? --> (comment your answer)
#It often means that there are data points outside the range of the x and y axis. Since we haven't specified range, it 
#probably means just what it says, that it rows missing a value for year or donors.

# Now let's use geom_line() to plot each country's time series
ggplot(data = organdata,
       mapping = aes(x = year, y = donors)) + 
  geom_line(aes(group = country)) + 
  facet_wrap(~ country)

# Leaving the timeseries aside, we can also look at the number of donors by country:
ggplot(data = organdata,
       mapping = aes(x = country, y = donors)) + 
  geom_boxplot()+
  coord_flip()

# This doesn't look great... try adding coord_flip() to the code above.

# Better, but not ideal. We probably want our countries listed from high to low avg donation rate, rather than alphabetical order.
ggplot(data = organdata,
       mapping = aes(x = reorder(country, donors, na.rm=TRUE),
                     y = donors)) + 
  geom_boxplot() +
  labs(x=NULL) +
  coord_flip()

# This is a decent plot, but there is more to explore in this dataset.
ggplot(data = organdata,
       mapping = aes(x = reorder(country, donors, na.rm=TRUE),
                     y = donors, fill = world)) + # adding var "world" 
  geom_boxplot() + labs(x=NULL) +
  coord_flip() + theme(legend.position = "top")

# We could also use points instead of a boxplot:
ggplot(data = organdata,
       mapping = aes(x = reorder(country, donors, na.rm=TRUE),
                     y = donors, color = world)) +
  geom_point() + 
  labs(x=NULL) + 
  coord_flip() + 
  theme(legend.position = "top")+
  geom_jitter()

# But these points have some overlapping observations... Try adding geom_jitter() to the plot above. If you don't like the default arguments of geom_jitter, look up at documentation for geom_jitter (https://ggplot2.tidyverse.org/reference/geom_jitter.html) and add additional arguments.

# A better altnerative to a jittered plot might be a Cleveland dot plot. Before we can get to a Cleveland dot plot, we'll need to do some summarizing.
by_country <- organdata %>% group_by(consent_law, country) %>%
  summarize_if(is.numeric, funs(mean, sd), na.rm = TRUE) %>%
  ungroup()

by_country

# What happened inside this pipeline? --> (comment your answer here)
#First it grouped organdata by consent law and country then it used the summarize function to calculate the mean and 
#standard deviation for all the numeric variables for each country. Finally, it ungrouped the data.
# Now for the Cleveland dot plot:
ggplot(data = by_country,
       mapping = aes(x = donors_mean, y = reorder(country, donors_mean),
                     color = consent_law)) + 
  geom_point(size=3) +
  ggtitle("Highest rates of organ donor procurement in countries with presumed consent laws") +
  labs(x = "Donor Procurement Rate",
       y = "", color = "Consent Law") +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  theme(legend.position="top",
        panel.background = element_rect(fill="white", linetype="solid", color = "gray"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  facet_wrap(~ consent_law)

# Try adding a facet_wrap() by consent law to the plot above. Facet_wrap has additional arguments that you could explore, including scales =, and ncol=. Again, Google is your friend here.

# Finally, add a title and remove gridlines. Once you are happy with your final Cleveland dot plot, save it.

ggsave("plot2.png",
       plot = last_plot(),
       dpi = 300)
