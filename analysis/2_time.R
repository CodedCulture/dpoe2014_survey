# subset of the data related to survey metadata
# extra columns calculated for finer grain time analysis
df.meta <- select(df, StartDateTZ, EndDateTZ, State, Timezone) %>%
        mutate(Length = as.numeric(EndDateTZ - StartDateTZ),
               # add day for week function since year started on a Tuesday
               StartWeek = factor(week(StartDateTZ + days(1))),
               StartWeekday = wday(StartDateTZ, label = T),
               StartHour = factor(hour(StartDateTZ)))

# outline = F to ignore outliers
par(bty = 'n')
a <-boxplot(df.meta$Length, outline = F, horizontal = T, col = '#B34F89', xlab = 'Minutes')

# count the survey responses by week
g.Weeks <- ggplot(df.meta, aes(x=StartWeek)) +
        geom_histogram(binwidth = 8, fill = '#B34F89') +
        theme(axis.title.x=element_blank(), 
              axis.title.y=element_blank())

# plot the total number of surveys taken
g.Date <- ggplot(df.meta, aes(x=StartDateTZ)) +
        stat_ecdf(col = '#B34F89', size = 1.5) +
        labs(x = "Date",
             y = "Total Percentage of Surveys")

# count the survey responses by week
g.DOW <- ggplot(df.meta, aes(x = StartWeekday)) +
        geom_histogram(fill = '#B34F89') +
        theme(axis.title.x=element_blank(), 
              axis.title.y=element_blank())

# count the survey responses by hour
g.hour <- ggplot(df.meta, aes(x=StartHour)) +
        geom_histogram(fill = '#B34F89') +
        labs(x = "Hour", 
             y = "Number of Surveys")

# run same plot with facets from timezones
g.hourTZ <- ggplot(df.meta, aes(x=StartHour)) +
        geom_histogram(fill = '#B34F89') +
        facet_grid(Timezone ~.) +
        labs(x = "Hour (EDT)", 
             y = "Number of Surveys")