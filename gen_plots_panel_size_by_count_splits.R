library(tidyverse)
library(ggplot2)


source("paths.R")

data <- read_csv(paste0(clean_path, "page_data.csv"))

data$DocumentID <- factor(data$Document.Directory)
data$RegionSimplified <- factor(data$RegionSimplified)
data$StyleBroad <- factor(data$StyleBroad)
data$Page <- factor(data$Page.Number)
data$Year <- factor(data$Publication.Date)

# compute Decade
data$Decade <- factor(floor(data$Publication.Date / 10) * 10)


########################
# select data columns

data <- data %>%
  select(panel_count, panel_size_prop_mean,
         Decade, RegionSimplified, StyleBroad)
#         LangFinal_WordStructureRaw,
#         LangFinal_WALS_VerbInflection,
#         DocumentID, Page)

str(data)


####################
# add log features

data$log_panel_count = log(data$panel_count)
data$log_panel_size = log(100 * data$panel_size_prop_mean)


#####################
# split plots
# region, style, decade

split_factors = c("RegionSimplified", "StyleBroad", "Decade")
split_labels = c("Global Region", "Graphic Style", "Decade")

for (split_index in 1:length(split_factors)) {

  split_factor <- split_factors[split_index]
  split_label <- split_labels[split_index]


  data$split_factor <- data[[split_factor]]

  ggplot(data,
         aes(x = log_panel_count,
             y = log_panel_size,
             color = split_factor,
         )) +
    geom_point(alpha = 0.5) +
    geom_smooth(method="lm") +
    theme_bw() +
    labs(x = "Log of Panels per Page",
         y = "Log of average Panel Size (%)",
         color = split_label) +
    lims(x = c(0, 4), y = c(0, 5)) +
    facet_grid( ~ split_factor) +
    theme(legend.position="none")
  ggsave(paste0("plots/Panel_SbC_split", split_factor, ".jpg"),
         dpi = 600, width = 12, height = 3)
}
