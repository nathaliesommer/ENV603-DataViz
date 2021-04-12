#### ENV 603 / 5-April-2021 / N.R. Sommer
# Dataset 2: Organ transplants for OECD countries

# We'll start by naively graphing some of this data. Take a look at a scatterplot of organ donors vs time. 
ggplot(data = organdata,
       mapping = aes(x = year, y = donors)) + 
  geom_point()

# What does the error message mean here? --> (comment your answer)

# I didn't get an error, but just a 'warning message': "Removed 34 rows containing missing values (geom_point)."  I assumed this means it removed missing or null values from the plot.

# Now let's use geom_line() to plot each country's time series
ggplot(data = organdata,
       mapping = aes(x = year, y = donors)) + 
  geom_line(aes(group = country)) + 
  facet_wrap(~ country)

# Leaving the timeseries aside, we can also look at the number of donors by country:
ggplot(data = organdata,
       mapping = aes(x = country, y = donors)) + 
  geom_boxplot() +
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
  theme(legend.position = "top") +
 geom_jitter()
  
# But these points have some overlapping observations... Try adding geom_jitter() to the plot above. If you don't like the default arguments of geom_jitter, look up at documentation for geom_jitter (https://ggplot2.tidyverse.org/reference/geom_jitter.html) and add additional arguments.

# A better altnerative to a jittered plot might be a Cleveland dot plot. Before we can get to a Cleveland dot plot, we'll need to do some summarizing.
by_country <- organdata %>% group_by(consent_law, country) %>%
  summarize_if(is.numeric, funs(mean, sd), na.rm = TRUE) %>%
  ungroup()

by_country

# What happened inside this pipeline? --> (comment your answer here)

#Data was grouped by the type of consent laws present in that country, then summarized by mean and standard deviation, then N/As were removed. The data was then ungrouped.

# Now for the Cleveland dot plot:
ggplot(data = by_country,
       mapping = aes(x = donors_mean, y = reorder(country, donors_mean),
                     color = consent_law)) + 
  geom_point(size=3) +
  labs(x = "Donor Procurement Rate",
       y = "", color = "Consent Law") +
  theme(legend.position="top")+
  facet_wrap(~consent_law) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ggtitle("Effect of Consent Law Status on Donor Procurement") + theme(plot.title = element_text(hjust=1))
  
# Try adding a facet_wrap() by consent law to the plot above. Facet_wrap has additional arguments that you could explore, including scales =, and ncol=. Again, Google is your friend here.

# Finally, add a title and remove gridlines. Once you are happy with your final Cleveland dot plot, save it.

ggsave("plot2.png",
       plot = last_plot(),
       dpi = 300)
