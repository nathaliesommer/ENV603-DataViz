#### ENV 603 / 5-April-2021 / N.R. Sommer
# Dataset 2: Organ transplants for OECD countries

# We'll start by naively graphing some of this data. Take a look at a scatterplot of organ donors vs time. 
ggplot(data = organdata,
       mapping = aes(x = year, y = donors)) + 
  geom_point()

# What does the error message mean here? --> Several rows have missing values (listed as NA in R), and in
# order to plot the features we're interested in, the year and donors, we need values in those cells. The 
# geom_point function has removed those rows that have missing data so they are not plotted.

# Now let's use geom_line() to plot each country's time series
ggplot(data = organdata,
       mapping = aes(x = year, y = donors)) + 
  geom_line(aes(group = country)) + 
  facet_wrap(~ country)

# Leaving the timeseries aside, we can also look at the number of donors by country:
ggplot(data = organdata,
       mapping = aes(x = country, y = donors)) + 
  coord_flip() + 
  geom_boxplot()

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
  summarize_if(is.numeric, list(mean = mean, sd = sd), na.rm = TRUE) %>% #the 'funs' function was deprecated so I used a list of the stats we are looking for
  ungroup()

by_country

# What happened inside this pipeline? --> This pipeline takes the organ donor data and reorganizes it by country, with the consent type being the first column.
# This means that instead of individual observations, each country's stats are summarized by mean and standard deviation.
# First, we had to group by both consent law and country to get the summary stats, then ungroup at the end to get the table we want.

# Now for the Cleveland dot plot:
ggplot(data = by_country,
       mapping = aes(x = donors_mean, y = reorder(country, donors_mean),
                     color = consent_law)) + 
  geom_point(size=3) +
  labs(x = "Donor Procurement Rate (%)", # added unit of percentage to give additional clarity
       y = "", color = "Consent Law") +
  theme(legend.position="bottom", plot.title = element_text(face = "bold")) + # moved the legend to the bottom and bolded the title
  facet_wrap(~ consent_law, scales ="free_x") + # changed the x-axis scale to be reactive to the range of points in each plot
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + # removed gridlines
  ggtitle("Organ donor procurement rates are greater\nin countries with presumed consent", subtitle = "") # added a title and added a newline character because title was getting cutoff
  
# Try adding a facet_wrap() by consent law to the plot above. Facet_wrap has additional arguments that you could explore, including scales =, and ncol=. Again, Google is your friend here.

# Finally, add a title and remove gridlines. Once you are happy with your final Cleveland dot plot, save it.

ggsave("NMW HW9 Exercise 2 Plot.png",
       plot = last_plot(),
       dpi = 500)
