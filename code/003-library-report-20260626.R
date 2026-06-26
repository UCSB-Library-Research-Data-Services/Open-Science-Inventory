# ==========================================================
# GOAL: Generate visual for library report
# NOTE:
# Most of code in the file align with those in 001-data-viz, including those
# for data wrangling, chord diagram set up, etc
# Deviations are to test ideas for the library report visual
# Code mostly written for speed, not reproducibility
# ==========================================================


# Load libraries ----
library(tidyverse)
library(readxl)
library(here)
library(circlize)


# Import data sets ----
rm(list = ls())        # clean out environment first

inventory <- read_xlsx("data_raw/OS-Inventory-list-20260518.xlsx")
glimpse(inventory)
## note, this version of the inventory dataset include a column called `Color`
## it will be used to assign color to chords for the library report


# Wrangle data ----
## OS Domain ----
list_domain <- inventory

for (domain in c("Data", "Method", "Source", "Access", "Review", "Education", "Infastructure")) {
  list_domain[[domain]] <- grepl(domain, list_domain$Domain)
}

glimpse(list_domain)

pairs_domain <- list_domain %>%
  select(Category.1 = Category, Item.1 = Item, Viz.ID.1= Viz.ID,
         Data, Method, Source, Access, Review, Education, Infastructure) %>%
  pivot_longer(cols = -c("Category.1", "Item.1", "Viz.ID.1"), 
               names_to = "Item.2", values_to = "Select") %>%
  filter(Select == TRUE) %>%
  mutate(Item.2 = case_when(Item.2 == "Data" ~ "Open data",
                            Item.2 == "Method" ~ "Open methodology",
                            Item.2 == "Source" ~ "Open source",
                            Item.2 == "Access" ~ "Open access",
                            Item.2 == "Review" ~ "Open peer review",
                            Item.2 == "Education" ~ "Open educational resource",
                            Item.2 == "Infastructure" ~ "Open infastructure")) %>%
  left_join(inventory %>% select(Category, Item, Viz.ID),
            by = join_by(Item.2 == Item)) %>%
  select(Category.1, Item.1, Viz.ID.1, Category.2 = Category, Item.2, Viz.ID.2 = Viz.ID)

pairs_domain


## Provider ----
list_provider <- inventory

for (provider in c("CSD", "DREAM", "R&E", "RDS", "SRC", "T&L", "Materials")) {
  list_provider[[provider]] <- grepl(provider, list_provider$Provider)
}

glimpse(list_provider)

pairs_provider <- list_provider %>%
  select(Category.1 = Category, Item.1 = Item, Viz.ID.1= Viz.ID,
         CSD, DREAM, `R&E`, RDS, SRC, `T&L`, Materials) %>%
  pivot_longer(cols = -c("Category.1", "Item.1", "Viz.ID.1"), 
               names_to = "Item.2", values_to = "Select") %>%
  filter(Select == TRUE) %>%
  mutate(Item.2 = case_when(Item.2 == "DREAM" ~ "DREAM Lab",
                            Item.2 == "Materials" ~ "Open Course Materials Program",
                            TRUE ~ Item.2)) %>%
  left_join(inventory %>% select(Category, Item, Viz.ID),
            by = join_by(Item.2 == Item)) %>%
  select(Category.1, Item.1, Viz.ID.1, Category.2 = Category, Item.2, Viz.ID.2 = Viz.ID)

pairs_provider



## Service ----
list_service <- inventory

for (service in c("Carpentries", "Dryad", "DMPTool", "eScholarship", "LibGuide",
                  "ORCiD", "Protocols.io", "RCL", "Agreement")) {
  list_service[[service]] <- grepl(service, list_service$Services)
}

glimpse(list_service)

pairs_service <- list_service %>%
  select(Category.1 = Category, Item.1 = Item, Viz.ID.1= Viz.ID,
         Carpentries, Dryad, DMPTool, eScholarship, LibGuide, 
         ORCiD, Protocols.io, RCL, Agreement) %>%
  pivot_longer(cols = -c("Category.1", "Item.1", "Viz.ID.1"), 
               names_to = "Item.2", values_to = "Select") %>%
  filter(Select == TRUE) %>%
  mutate(Item.2 = case_when(Item.2 == "Carpentries" ~ "Carpentries program",
                            Item.2 == "Agreement" ~ "Transformative Agreements",
                            Item.2 == "LibGuide" ~ "LibGuides (OS-themed)",
                            Item.2 == "RCL" ~ "Reproducible and Collaborative Lab",
                            TRUE ~ Item.2)) %>%
  left_join(inventory %>% select(Category, Item, Viz.ID),
            by = join_by(Item.2 == Item)) %>%
  select(Category.1, Item.1, Viz.ID.1, Category.2 = Category, Item.2, Viz.ID.2 = Viz.ID)

pairs_service



# Create and reorganize pairs dataset ----
pairs_temp <- rbind(pairs_domain, pairs_provider, pairs_service) %>%
  arrange(Viz.ID.1, Viz.ID.2)

rm(pairs_domain, pairs_provider, pairs_service)


pairs_reorg_domain <- pairs_temp %>%
  filter(Category.2 == "OS Domains") %>%        # all OS Domains are in Category.2
  mutate(Category.Temp = Category.1, Item.Temp = Item.1, Viz.ID.Temp = Viz.ID.1) %>%
  select(Category.1 = Category.2, Item.1 = Item.2, Viz.ID.1 = Viz.ID.2,
         Category.2 = Category.Temp, Item.2 = Item.Temp, Viz.ID.2 = Viz.ID.Temp)
  
pairs_reorg_service1 <- pairs_temp %>%
  filter(Category.2 != "OS Domains") %>%        # already in pairs_domain_graph
  filter(Category.1 == "Services & Programs")

pairs_reorg_service2 <- pairs_temp %>%
  filter(Category.2 != "OS Domains") %>%        # already in pairs_domain_graph
  filter((Category.1 == "Instruction & Consultation" & Category.2 == "Services & Programs") ) %>%
  mutate(Category.Temp = Category.1, Item.Temp = Item.1, Viz.ID.Temp = Viz.ID.1) %>%
  select(Category.1 = Category.2, Item.1 = Item.2, Viz.ID.1 = Viz.ID.2,
         Category.2 = Category.Temp, Item.2 = Item.Temp, Viz.ID.2 = Viz.ID.Temp)
  
pairs_reorg_instruction <- pairs_temp %>%
  filter(Category.2 != "OS Domains") %>%        # already in pairs_domain_graph
  filter(Category.1 == "Instruction & Consultation", Category.2 == "Provider")
  
pairs_reorg_engagement <- pairs_temp %>%
  filter(Category.2 != "OS Domains") %>%        # already in pairs_domain_graph
  filter(Category.1 == "Engagement & Community")

## sanity check
nrow(pairs_temp) == nrow(pairs_reorg_domain) + nrow(pairs_reorg_service1) + nrow(pairs_reorg_service2) +
  nrow(pairs_reorg_instruction) + nrow(pairs_reorg_engagement)

# create pairs df for graphing
pairs <- rbind(pairs_reorg_domain, pairs_reorg_service1, pairs_reorg_service2,
               pairs_reorg_instruction, pairs_reorg_engagement) %>%
  arrange(Viz.ID.1, Viz.ID.2)



# Define categories + colors for chord diagram ---- 
xlim_df <- inventory %>%
  group_by(Category) %>%
  summarize(min = min(Viz.ID) - 1.6, 
            max = max(Viz.ID) + 1.6)

xlim_df$Category

sector_colors <- c("OS Domains" = "#09847A", 
                   "Provider" = "#003660", 
                   "Services & Programs" = "#6D7D33", 
                   "Instruction & Consultation" = "#C43424",
                   "Engagement & Community" = "#FEBC11")


inventory_graph <- inventory %>%
  mutate(Category.Color = plyr::revalue(Category, sector_colors))



# Generate chord diagram (version 1) ----
## set up svg export
svglite::svglite("images/library-report-rds-highlight-20260626.svg", width = 7, height = 10)

## initialize
circos.clear()
circos.par(canvas.xlim = c(-1.35, 1.35), canvas.ylim = c(-1.35, 1.35),
           gap.after = rep(0, times = length(unique(inventory_graph$Category))),
           start.degree = -36)

circos.initialize(sectors = inventory_graph$Category, 
                  xlim = as.matrix(xlim_df %>% select(min, max)))

## add items and label
circos.labels(sectors = inventory_graph$Category, x = inventory_graph$Viz.ID, 
              labels = inventory_graph$Item, side = "outside",
              cex = 0.45, padding = 0.0, connection_height = mm_h(0.5))

## annotate buckets
circos.trackPlotRegion(ylim = c(0, 1), track.height = 0.09,
                       panel.fun = function(x, y) {
                         # obtain cell meta data
                         sector_name <- get.cell.meta.data("sector.index")
                         xlim_cell <- CELL_META$xlim
                         ylim_cell <- CELL_META$ylim
                         # draw color coding rectangles
                         circos.rect(xlim_cell[1] + 1.3, ylim_cell[1], xlim_cell[2]-1.3, ylim_cell[2],
                                     col = sector_colors[sector_name],
                                     border = NA)
                         # add text label
                         circos.text(mean(xlim_cell), 0.5, labels = sector_name,
                                     facing = "bending.inside", niceFacing = TRUE,
                                     cex = 0.55, col = "white", font = 2)
                       },
                       # turn off default grid lines
                       bg.border = NA, cell.padding = c(0.01, 0, 0, 0)
)



## loop around each item to add grey chord connections
for(i in nrow(pairs):1){
  circos.link(sector.index1 = pairs$Category.1[i],
              point1 = c(pairs$Viz.ID.1[i] - 0.26, pairs$Viz.ID.1[i] + 0.26), 
              sector.index2 = pairs$Category.2[i],
              point2 = c(pairs$Viz.ID.2[i] - 0.26, pairs$Viz.ID.2[i] + 0.26), 
              col = "#BFBFBF30", h.ratio = 0.5, w = 0.8)
}


## add RDS cord
pairs_rds <- pairs %>% filter(Item.1 == "RDS" | Item.2 == "RDS")

for(i in nrow(pairs_rds):1){
  circos.link(sector.index1 = pairs_rds$Category.1[i],
              point1 = c(pairs_rds$Viz.ID.1[i] - 0.26, pairs_rds$Viz.ID.1[i] + 0.26), 
              sector.index2 = pairs_rds$Category.2[i],
              point2 = c(pairs_rds$Viz.ID.2[i] - 0.26, pairs_rds$Viz.ID.2[i] + 0.26), 
              col = sector_colors["Provider"], h.ratio = 0.5, w = 0.8)
}


## close the device
dev.off()


