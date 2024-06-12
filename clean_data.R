library(tidyverse)
library(readxl)


source("paths.R")

csv_data <- read.csv(paste0(raw_path,
                            "TINTIN_Panels_021324_All.csv"))

updated_book_info <- read_excel(paste0(raw_path,
                                       "TINTINCorpus_BiblioList_Final.xlsx"))

###################

raw_subset <- tibble(csv_data[, c("Document.Directory",
                                  "Page.Number",
                                  "Region.Area",
                                  "Page.Area")])

#head(raw_subset)

###################


old_book_info <- tibble(unique(csv_data[, c("Document.Directory",
                                            "StyleCluster",
                                            "LangFinal_WordStructureRaw",
                                            "LangFinal_WALS_VerbInflection",
                                            "Publication.Date",
                                            "RegionSimplified")]))


book_info <- tibble(updated_book_info[, c("Document Directory",
                                          "StyleBroad")])

all_book_info <- merge(old_book_info, book_info,
                       by.x = "Document.Directory",
                       by.y = "Document Directory",
                       all = T)


rm(book_info, old_book_info)

#######################
# check values

str(all_book_info)

table(all_book_info$StyleBroad)
table(all_book_info$Publication.Date)
table(all_book_info$RegionSimplified)

table(all_book_info$LangFinal_WordStructureRaw)
table(all_book_info$LangFinal_WALS_VerbInflection)

####################

page_data <- raw_subset %>%
  group_by(Document.Directory, Page.Number) %>%
  summarize(panel_count = n(),
            panel_size_prop_mean = mean(Region.Area / Page.Area, na.rm=T),
            panel_size_prop_range = max(Region.Area / Page.Area, na.rm=T) -
              min(Region.Area / Page.Area, na.rm=T)) %>%
  ungroup()

full_page_data <- merge(page_data,
                        all_book_info,
                        by="Document.Directory", all.x=TRUE)

#head(full_page_data)

write.csv(full_page_data,
          paste0(clean_path,
                 "page_data.csv"),
          row.names = F)

####################

book_data <- page_data %>%
  group_by(Document.Directory) %>%
  summarize(panels_per_page_mean = mean(panel_count),
            panels_per_page_range = max(panel_count) - min(panel_count),
            panel_size_prop_mean_page_mean = mean(panel_size_prop_mean),
            panel_size_prop_mean_page_range = max(panel_size_prop_mean) -
              min(panel_size_prop_mean)) %>%
  ungroup()


full_book_data <- merge(book_data, all_book_info,
                        by="Document.Directory", all.x=TRUE)

#head(full_book_data)

write.csv(full_book_data,
          paste0(clean_path,
                 "book_data.csv"),
          row.names = F)
