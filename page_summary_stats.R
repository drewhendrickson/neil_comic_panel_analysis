library(ggplot2)

rm(list = ls())

source("paths.R")

page_data <- read.csv(paste0(clean_path,
                             "page_data.csv"))

page_data$StyleBroad <- factor(page_data$StyleBroad)

page_data$Document.Directory <- factor(page_data$Document.Directory)
#page_data$BookOriginalLanguage <- factor(page_data$BookOriginalLanguage)
page_data$RegionSimplified <- factor(page_data$RegionSimplified)

page_data$Publication.Date <- factor(page_data$Publication.Date)


str(page_data)

################
# number of panels

summary(page_data$panel_count)
sd(page_data$panel_count)

ggplot(page_data,
       aes(x = panel_count)) +
  geom_density() +
  theme_bw()


################
# size of panels

summary(page_data$panel_size_prop_mean)
sd(page_data$panel_size_prop_mean )

ggplot(page_data,
       aes(x = panel_size_prop_mean )) +
  geom_histogram() +
  theme_bw()
