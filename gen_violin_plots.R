library(tidyverse)
library(ggplot2)


source("paths.R")

data <- read_csv(paste0(clean_path, "page_data.csv"))

data$DocumentID <- factor(data$Document.Directory)
data$RegionSimplified <- factor(data$RegionSimplified)
data$StyleBroad <- factor(data$StyleBroad)
data$Page <- factor(data$Page.Number)
data$Year <- factor(data$Publication.Date)


######################
# select data

data <- data %>%
  select(panel_size_prop_mean, panel_count,
         Year, RegionSimplified, StyleBroad,
         DocumentID, Page)

str(data)


#####################

split_factors = c("RegionSimplified", "StyleBroad")
split_labels = c("Global Region", "Graphic Style")

measures <- c("panel_size_prop_mean", "panel_count")
measure_labels <- c("Average Panel Size", "Panels per Page")

for (measure_index in 1:length(measures)) {

  measure_name <- measures[measure_index]
  measure_label <- measure_labels[measure_index]

  for (split_index in 1:length(split_factors)) {

    split_name <- split_factors[split_index]
    split_label <- split_labels[split_index]


    tmp_data <- data_frame(measure = data[[measure_name]],
                           split_factor = data[[split_name]])

    ggplot(tmp_data,
           aes(y = measure,
               x = split_factor,
               fill = split_factor)) +
      geom_violin(adjust=measure_index) +
      geom_boxplot(width=.1) +
      theme_bw() +
      labs(x = split_label,
           y = measure_label) +
      theme(legend.position="none",
            axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
    ggsave(paste0("plots/violin_", measure_name, "_BY_",
                  split_name, ".jpg"),
           dpi = 600, width = 8, height = 5)

  }
}
