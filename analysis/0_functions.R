library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)

topSlice <- function (df, df.merge, idVar) {
        df.top <- df[df[, 3] == 1, c(1,2)]
        df.slice <- merge(df.top, df.merge, id = idVar)
}

topSliceHist <- function (df, df.merge, ranking, xFacet, yFacet) {
        df.top <- df[df[, 3] == 1, c(1,2)]
        df.slice <- merge(df.top, df.merge, id = idVar)
        
        ggplot(df.slice, aes_string(x = ranking)) + 
                geom_histogram(fill = '#B34F89') +
                facet_grid(as.formula(paste(xFacet, '~', yFacet)))
}

facetHist <- function (df, plotx, yFacet, xFacet) {
        g.plot <- ggplot(df, aes_string(x = plotx)) +
                geom_histogram(fill = '#B34F89') +
                theme(axis.title.x=element_blank(), 
                      axis.title.y=element_blank()) 
        
        ifelse(!missing(yFacet) & !missing(xFacet),
               g.plot <- g.plot + facet_grid(as.formula(paste(yFacet, ' ~ ', xFacet))),
               ifelse(!missing(yFacet),
                      g.plot <- g.plot + facet_grid(as.formula(paste(yFacet, ' ~ .'))),
                      ifelse(!missing(xFacet),
                             g.plot <- g.plot + facet_grid(as.formula(paste('. ~ ', xFacet))),
                             g.plot)))

        g.plot
}

facetHistDensity <- function (df, plotx, yFacet) {        
        df <- df %>%
                group_by_(yFacet) %>%
                mutate(Per = 1/length(unique(ResponseID))) %>%
                group_by_(plotx, yFacet) %>%
                summarize(Percent = sum(Per))
        
        g.plot <- ggplot(df, aes_string(x = plotx, y = 'Percent'))
        g.plot <- g.plot +
                geom_bar(fill = '#B34F89', stat = 'identity') +
                facet_grid(as.formula(paste(yFacet, '~ .'))) +
                theme(axis.title.x=element_blank(), 
                      axis.title.y=element_blank()) +
                scale_y_continuous(labels = percent) 
        g.plot
               
}

meltFactor <- function (df, factor, factorName, factorValue) {
        df.melt <- select(df, ResponseID, starts_with(factor), -ends_with('Other'), -ends_with('OtherComment')) %>%
                melt(id = 'ResponseID', variable.name = factorName, value.name = factorValue, na.rm=T)
        df.melt[ ,3] <- factor(df.melt[ ,3])
        df.melt
}