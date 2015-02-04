setwd('~/dev/DPOE14_survey/')

# load packages for data analysis
library(scales)
library(ggplot2)
library(lattice)
library(lubridate)
library(plyr)
library(dplyr)
library(reshape2)

# increase number length before scientific notation is used
options(scipen = 6)

# load functions created during the course of this analysis
source('~/dev/DPOE14_survey/analysis/0_functions.r')

# data frame named df because I'm lazy 
df <- read.csv('~/dev/DPOE14_survey/data/data_clean.csv', stringsAsFactors = T, na.string = c('NA', '', 'N/A'))

# convert dates to POSIX objects
df$StartDate <- mdy_hms(df$StartDate, truncated=1, tz='Europe/London')
df$EndDate <- mdy_hms(df$EndDate, truncated=1, tz='Europe/London')
df$Timezone <- df$State

# assign timezones based on state
levels(df$Timezone) <- list('America/New_York' = c('ME', 'NH', 'VT', 'MA', 'RI', 'CT', 'NY',
                                                   'PA', 'NJ', 'DE', 'MD', 'VA', 'DC', 'NC',
                                                   'SC', 'GA', 'FL', 'WV', 'OH', 'MI', 'IN',
                                                   'KY', 'PR'),
                            'America/Chicago' = c('WI', 'IL', 'TN', 'MS', 'AL', 'LA', 'AR',
                                                  'MO', 'IA', 'MN', 'ND', 'SD', 'NE', 'KS', 
                                                  'OK', 'TX'),
                            'America/Denver' = c('MT', 'WY', 'CO', 'NM', 'AZ', 'UT', 'ID'),
                            'America/Los_Angeles' = c('OR', 'WA', 'CA', 'NV'),
                            'America/Juneau' = c('AK'),
                            'Pacific/Honolulu' = c('HI'))

# normalize times according to timezones
a <- lapply(seq(length(df$StartDate)), function(x) with_tz(df$StartDate[x], tz = as.character(df$Timezone[x]))[1])
df$StartDateTZ <- Reduce('c', a)

b <- lapply(seq(length(df$EndDate)), function(x) with_tz(df$EndDate[x], tz = as.character(df$Timezone[x]))[1]) 
df$EndDateTZ <- Reduce('c', b)

# clean up and order factor names
df$AidAmount <- revalue(df$AidAmount, c('$0 - $250' = '$0-250', '$251 - $500' = '$251-500', '$501 - $750' = '$501-750',
                                        '$751 - $1,000' = '$751-1000', '$1,001 - $3,000' = '$1001-3000', 
                                        '$3,001 - $5,000' = '$3001-5000', 'More than $5,000' = '$5000+', 'I don\'t know' = 'Don\'t know'))
df$AidAmount <- factor(df$AidAmount, c('$0-250', '$251-500', '$501-750', '$751-1000', '$1001-3000', '$3001-5000', '$5000+', 'Don\'t know'))

df$StaffSize <- revalue(df$StaffSize, c('1 - 12' = '1-12', '13 - 24' = '13-24', '25 - 50' = '25-50',
                                        '51 - 200' = '51-200', '201 - 500' = '201-500', '501+'))
df$StaffSize <- factor(df$StaffSize, c('1-12', '13-24', '25-50', '51-200', '201-500', '501+'))