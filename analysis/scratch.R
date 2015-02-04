

## Participants

```{r}
df.types <- select(df, ResponseID, starts_with('T.'), -T.Other) %>%
melt(id = 'ResponseID', variable.name = 'OrgTypeCat', value.name = 'OrgType', na.rm=T)
df.parentorgs <- select(df, ResponseID, starts_with('PO.'), -PO.Other) %>%
melt(id = 'ResponseID', variable.name = 'ParentOrgCat', value.name = 'ParentOrg', na.rm=T)
df.staff <- select(df, ResponseID, starts_with('R.'), -R.Other) %>%
melt(id = 'ResponseID', variable.name = 'Staffing', value.name = 'Staffing', na.rm=T)

df.org <- select(df, ResponseID, State, Timezone, StaffSize, AidAmount)

#        merge(df.types[, c(1, 3)]) %>%
#        merge(df.staff[, c(1, 3)]) %>%
#        merge(df.parentorgs[, c(1, 3)])
```

```{r}
df.cformat <- select(df, ResponseID, starts_with('CF.'), -CF.Other) %>%
melt(id = 'ResponseID', variable.name = 'ContentFormatCat', value.name = 'ContentFormat', na.rm=T)
df.ctypes <- select(df, ResponseID, starts_with('CT.'), -CT.Other) %>%
melt(id = 'ResponseID', variable.name = 'ContentTypeCat', value.name = 'ContentType', na.rm=T)

df.content <- select(df, ResponseID, Importance)

```

## Training Preferences
```{r}
df.rtopic <- select(df, ResponseID, starts_with('RT.'), -ends_with('Other'), -ends_with('OtherComment')) %>%
melt(id = 'ResponseID', variable.name = 'Topic', value.name = 'TopicRanking') %>%
mutate(TopicRanking = factor(TopicRanking))
df.rformat <- select(df, ResponseID, starts_with('RTF.'), -ends_with('Other'), -ends_with('OtherComment')) %>%
melt(id = 'ResponseID', variable.name = 'Format', value.name = 'FormatRanking') %>%
mutate(FormatRanking = factor(FormatRanking))
df.rinperson <- select(df, ResponseID, starts_with('RTIP.'), -RTIP.PtherComment, -RTIP.OtherComment) %>%
melt(id = 'ResponseID', variable.name = 'InPersonFormat', value.name = 'InPersonFormatRanking') %>%
mutate(InPersonFormatRanking = factor(InPersonFormatRanking))
df.rinperson2 <- meltRanks(df, 'RTIP.', 'InPersonFormat', 'InPersonFormatRanking')
df.rlength <- select(df, ResponseID, starts_with('RTL.'), -RTL.Other, -RTL.OtherComment) %>%
melt(id = 'ResponseID', variable.name = 'Length', value.name = 'LengthRanking') %>%
mutate(LengthRanking = factor(LengthRanking))
df.rlength2<- meltRanks(df, 'RTL.', 'Length', 'LengthRanking')
df.routcome <- select(df, ResponseID, starts_with('RN.'), -RN.Other, -RN.OtherComment) %>%
melt(id = 'ResponseID', variable.name = 'Outcome', value.name = 'OutcomeRanking') %>%
mutate(OutcomeRanking = factor(OutcomeRanking))

df.routcome.top <- df.routcome[df.routcome$OutcomeRanking == 1, c(1,2)]

df.rtopic.routcome <- merge(df.rtopic, df.routcome.top, id = 'ResponseID')

ggplot(df.rtopic.routcome, aes(x = TopicRanking)) + 
geom_histogram(fill = '#B34F89') + 
facet_grid(Topic~Outcome)

ggplot(df.rtopic.routcome) + 
        geom_bar(aes(TopicRanking, (..count..)/sum(..count..)), fill = '#B34F89') + 
        scale_y_continuous(labels = percent) + 
        facet_grid(. ~ Topic + Outcome)
```

```{r}
source('functions.R')
df2<-topSlice(df.routcome, df.rformat, 'ResponseID')
topSliceHist(df2, 'FormatRanking', 'Format', 'Outcome')

df.routcome.top <- df.routcome[df.routcome$OutcomeRanking == 1, c(1,2)]

df.rformat.routcome <- merge(df.rformat, df.routcome.top, id = 'ResponseID')

ggplot(df.rformat.routcome, aes(x = FormatRanking)) + 
        geom_histogram(fill = '#B34F89') + 
        facet_grid(Format~Outcome)

ggplot(df.rtopic.routcome) + 
        geom_bar(aes(TopicRanking, (..count..)/sum(..count..)), fill = '#B34F89') + 
        scale_y_continuous(labels = percent) + 
        facet_grid(. ~ Topic + Outcome)
```

```{r}
ggplot(df.rlength, aes(x = LengthRanking)) + 
        geom_histogram() +
        facet_grid(Length ~ .)

df.routcome.top <- df.routcome[df.routcome$OutcomeRanking == 1, c(1,2)]

df.rlength.routcome <- merge(df.rlength, df.routcome.top, id = 'ResponseID')

ggplot(df.rlength.routcome, aes(x = LengthRanking)) + 
        geom_histogram(fill = '#B34F89') + 
        facet_grid(Length~Outcome)

df.rtopic.top <- df.rtopic[df.rtopic$TopicRanking == 1, c(1,2)]

df.rlength.rtopic <- merge(df.rlength, df.rtopic.top, id = 'ResponseID')

ggplot(df.rlength.rtopic, aes(x = LengthRanking)) + 
        geom_histogram(fill = '#B34F89') + 
        facet_grid(Length~Topic)

df.rformat.top <- df.rformat[df.rformat$FormatRanking == 1, c(1,2)]

df.rlength.rformat <- merge(df.rlength, df.rformat.top, id = 'ResponseID')

ggplot(df.rlength.rformat, aes(x = LengthRanking)) + 
        geom_histogram(fill = '#B34F89') + 
        facet_grid(Length~Format)
```

```{r}
ggplot(df.rformat, aes(x = FormatRanking)) + 
        geom_histogram() +
        facet_grid(Format ~ .)

select(df, starts_with('RTF.'), -RTF.Other, -RTF.OtherComment) %>%
        colMeans(na.rm = T)
```



b<-merge(df.parentorgs, df.ctypes)

ddply(b, .(ContentType), summarize, per = count(ContentType)/count(ResponseID))
