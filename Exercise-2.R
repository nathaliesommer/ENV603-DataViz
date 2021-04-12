#### ENV 603 / 5-April-2021 / N.R. Sommer
# Dataset 2: Organ transplants for OECD countries

# We'll start by naively graphing some of this data. Take a look at a scatterplot of organ donors vs time. 
ggplot(data = organdata,
       mapping = aes(x = year, y = donors)) + 
  geom_point()

# What does the error message mean here? --> (comment your answer)
#the error message means the plot we just made excluded the rows that have missing value which shows as NA 

# Now let's use geom_line() to plot each country's time series
ggplot(data = organdata,
       mapping = aes(x = year, y = donors)) + 
  geom_line(aes(group = country)) + 
  facet_wrap(~ country)

# Leaving the timeseries aside, we can also look at the number of donors by country:
ggplot(data = organdata,
       mapping = aes(x = country, y = donors)) + 
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
  geom_jitter() +
  labs(x=NULL) + 
  coord_flip() + 
  theme(legend.position = "top")

# But these points have some overlapping observations... Try adding geom_jitter() to the plot above. If you don't like the default arguments of geom_jitter, look up at documentation for geom_jitter (https://ggplot2.tidyverse.org/reference/geom_jitter.html) and add additional arguments.

# A better altnerative to a jittered plot might be a Cleveland dot plot. Before we can get to a Cleveland dot plot, we'll need to do some summarizing.
by_country <- organdata %>% group_by(consent_law, country) %>%
  summarize_if(is.numeric, funs(mean, sd), na.rm = TRUE) %>%
  ungroup()

by_country

# What happened inside this pipeline? --> (comment your answer here)
# In this pipeline, the previous dataset are grouped by countries. Instead of showing the annual values for each country, the pipeline calculated the mean and standard deviation for each country. 

# Now for the Cleveland dot plot:
ggplot(data = by_country,
       mapping = aes(x = donors_mean, y = reorder(country, donors_mean))) + 
  geom_point(size=3) +
  labs(x = "Donor Procurement Rate",
       y = "") +
  theme(legend.position="top") + 
  facet_wrap("consent_law") +  
  ggtitle("Average Organ Donor Procurement Rate in 17 OECD Countries from 1991 to 2002",subtitle =  "Countries with a model of presumed consent usually have higher procurement rate than others") + 
  theme(plot.subtitle = element_text(colour="gray40"), #change subtitle font 
        axis.text.y = element_text(size = 11, colour="black"),
        axis.ticks.y = element_blank(),
        axis.line.y = element_line(size = 0.55, colour ="darkgray"),
        axis.line.x = element_line(size = 0.55, colour ="darkgray"),
        panel.grid.major = element_blank(), #remove grids
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), #remove background color
        plot.margin = unit(c(1,1,1,1), "cm"),
        panel.spacing.x = unit(2, "lines")) + #add spaces between each facet so we can see the entire axis 


# Try adding a facet_wrap() by consent law to the plot above. Facet_wrap has additional arguments that you could explore, including scales =, and ncol=. Again, Google is your friend here.

# Finally, add a title and remove gridlines. Once you are happy with your final Cleveland dot plot, save it.
  ggsave("Exercise2_Cleveland.png",
         plot = last_plot(),
         height=8, width=12)
