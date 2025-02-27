library(tidyverse)
library(ggplot2)


source("paths.R")

data <- read_csv(paste0(clean_path, "page_data.csv"))

data$DocumentID <- factor(data$Document.Directory)
data$RegionSimplified <- factor(data$RegionSimplified)
data$StyleBroad <- factor(data$StyleBroad)
data$Page <- factor(data$Page.Number)
#data$Year <- factor(data$Publication.Date)

####################


dim(data)
length(unique(data$DocumentID))
tmp = table(data$DocumentID)
summary(as.vector(tmp))

summary(data)
sd(data$panel_count)
sd(100 * data$panel_size_prop_mean)


######################
# select data

data <- data %>%
  select(panel_size_prop_mean, panel_count)

str(data)

#####################



#################

ggplot(data,
       aes(x = panel_count)) +
  geom_bar(fill="white", color="black") +
  theme_bw() +
  labs(x = "Panels per Page",
       y = "Count")
ggsave(paste0("plots/panel_count_hist.jpg"),
       dpi = 600, width = 7, height = 5)


#################

ggplot(data,
       aes(x = 100 * panel_size_prop_mean)) +
  geom_histogram(fill="white", color="black", bins = 20) +
  theme_bw() +
  labs(x = "Average Panel Size (% page)",
       y = "Count")
ggsave(paste0("plots/panel_size_hist.jpg"),
       dpi = 600, width = 7, height = 5)
