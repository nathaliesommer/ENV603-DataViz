#### ENV 603 / 5-April-2021 / N.R. Sommer
# Dataset 2: Organ transplants for OECD countries

# We'll start by naively graphing some of this data. Take a look at a scatterplot of organ donors vs time. 
ggplot(data = organdata,
       mapping = aes(x = year, y = donors)) + 
  geom_point()

# What does the error message mean here? --> Generally, this means that the ranges for the x axis or y axis have been incorrectly specified. However, what is really going on here is that there are 34 rows for which year is NA, and NA can't be plotted by geom_point(), which is why it omits those 34 rows.

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
  geom_jitter(width = 0.2, height = 0.2) +
  theme(legend.position = "top")

# But these points have some overlapping observations... Try adding geom_jitter() to the plot above. If you don't like the default arguments of geom_jitter, look up at documentation for geom_jitter (https://ggplot2.tidyverse.org/reference/geom_jitter.html) and add additional arguments.

# A better altnerative to a jittered plot might be a Cleveland dot plot. Before we can get to a Cleveland dot plot, we'll need to do some summarizing.
by_country <- organdata %>% group_by(consent_law, country) %>%
  summarize_if(is.numeric, funs(mean, sd), na.rm = TRUE) %>%
  ungroup()

by_country

# What happened inside this pipeline? --> Wow this is a crazy pipe. First you took the original organdata data frame, and you groupd rows by consent_law and then by country, and then you summarize the groups created (two-way combinations of consent_law and country) using both mean and sd function, which are combined in a list of functions via the "funs" function. This summary is only conducted on numeric variables, because of the is.numeric argument in the summarize_if function. Then you ungroup the tibble so that subsequent functions will not be conducted on groupings. 

# Now for the Cleveland dot plot:
ggplot(data = by_country,
       mapping = aes(x = donors_mean, y = reorder(country, donors_mean),
                     color = consent_law)) + 
  geom_point(size=3) +
  labs(x = "Donor Procurement Rate",
       y = "", color = "Consent Law") +
  theme(legend.position="right",
        panel.grid.major.y = element_line(colour = "lightgray"),
        panel.grid.major.x = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        axis.ticks = element_blank(),
        strip.background = element_rect(fill = "white"),
        legend.key =  element_rect(fill = "white"),
        plot.title = element_text(hjust = 0)) +
  facet_wrap(~consent_law, ncol = 1, scales = "free_y") + #I choose to make the facet over top of each other so the horizontal variation in donor rates can be compared between the two consent_law categories. I choose to keep the scales fixed to facilitate comparison of donor rates.
  labs(title = "Countries with presumed consent laws \nachieve higher organ donation rates") +
  xlab("Average annual donation rate (%)")

# Try adding a facet_wrap() by consent law to the plot above. Facet_wrap has additional arguments that you could explore, including scales =, and ncol=. Again, Google is your friend here.
# Finally, add a title and remove gridlines. Once you are happy with your final Cleveland dot plot, save it.

ggsave("Commercon_assignment_9_Exercise_2.png",
       plot = last_plot(),
       dpi = 300)
