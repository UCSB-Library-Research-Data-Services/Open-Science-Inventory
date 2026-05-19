# ==========================================================
# GOAL: Generate visual for library report
# NOTE:
# Most of code in the file align with those in 001-data-viz, including those
# for data wrangling, chord diagram set up, etc
# Deviations are to test ideas for the library report visual
# Code mostly written for speed, not reproducibility
# ==========================================================


# Load library ----
library(tidyverse)
library(readxl)
library(here)
library(circlize)


# Import data sets ----
rm(list = ls())        # clean out environment first

inventory <- read_xlsx("data_raw/OS-Inventory-list-20260518.xlsx")
glimpse(inventory)


# Wrangle data | OS Domain ----
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



# Wrangle data | Provider ----
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



# Wrangle data | Service ----
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



# Create pairs dataset ----
pairs <- rbind(pairs_domain, pairs_provider, pairs_service) %>%
  arrange(Viz.ID.1, Viz.ID.2)



# Chord diagram | Define categories + colors ---- 
xlim_df <- inventory %>%
  group_by(Category) %>%
  summarize(min = min(Viz.ID) - 1.6, 
            max = max(Viz.ID) + 1.6)

xlim_df$Category

sector_colors <- c("OS Domains" = "#003660", 
                   "Provider" = "#09847A", 
                   "Services & Programs" = "#6D7D33", 
                   "Instruction & Consultation" = "#C43424",
                   "Engagement & Community" = "#FEBC11")

status_palette <- c("#64B5F6", "#B58CD2", "#D0D2D3")

list_coded <- inventory %>%
  mutate(Status.Color = case_when(Status == "Active" ~ status_palette[1],
                                  Status == "In development" ~ status_palette[2],
                                  Status == "On hold" ~ status_palette[3],
                                  TRUE ~ "white"),
         Category.Color = plyr::revalue(Category, sector_colors),
  )

domain_colors <- tibble(
  OS.Viz.ID = 1:7, 
  OS.Domain = c("Open data", "Open methodology", "Open source", "Open access",
                "Open peer review", "Open educational resource", "Open infastructure"),
  OS.Color = c("#4E79A770", "#9C6ADE70", "#59A14F70", "#F28E2B70", 
               "#E1575970", "#EDC94870", "#76B7B270")
)



# Chord diagram | Generate visual ----
## initialize
circos.clear()
circos.par(canvas.xlim = c(-1.1, 1.1), canvas.ylim = c(-1.1, 1.1),
           gap.after = rep(0, times = length(unique(list_coded$Category))),
           start.degree = -13)

circos.initialize(sectors = list_coded$Category, 
                  xlim = as.matrix(xlim_df %>% select(min, max)))

## add items and label
circos.labels(sectors = list_coded$Category, x = list_coded$Viz.ID, 
              labels = list_coded$Item, side = "outside",
              cex = 0.3, padding = 0.0, connection_height = mm_h(0.3))

## annotate status
circos.track(sectors = list_coded$Category, ylim = c(0, 1),
             track.height = 0.04, cell.padding = c(0, 0, 0, 0), bg.border = NA)

circos.trackPoints(sectors = list_coded$Category, 
                   x = list_coded$Viz.ID, y = rep(0.5, times = nrow(list_coded)), 
                   col = list_coded$Status.Color, 
                   pch = 16, cex = 0.6)

## annotate buckets
circos.trackPlotRegion(ylim = c(0, 1), track.height = 0.06,
                       panel.fun = function(x, y) {
                         # obtain cell meta data
                         sector_name <- get.cell.meta.data("sector.index")
                         xlim_cell <- CELL_META$xlim
                         ylim_cell <- CELL_META$ylim
                         # draw color coding rectangles
                         circos.rect(xlim_cell[1] + 1.2, ylim_cell[1], xlim_cell[2]-1.2, ylim_cell[2],
                                     col = sector_colors[sector_name],
                                     border = NA)
                         # add text label
                         circos.text(mean(xlim_cell), 0.5, labels = sector_name,
                                     facing = "bending.inside", niceFacing = TRUE,
                                     cex = 0.25, col = "white", font = 2)
                       },
                       # turn off default grid lines
                       bg.border = NA, cell.padding = c(0.01, 0, 0, 0)
)



## add chords to where different OS domains map to
## loop around each OS domain
for (i in 1:nrow(domain_colors)) {
  # filter for chord that extend from each domain
  pairs_graph <- pairs %>%
    filter(Viz.ID.1 == domain_colors$OS.Viz.ID[i] | Viz.ID.2 == domain_colors$OS.Viz.ID[i])
  
  # loop around each chord
  for(j in 1:nrow(pairs_graph)){
    circos.link(sector.index1 = pairs_graph$Category.1[j],
                point1 = c(pairs_graph$Viz.ID.1[j] - 0.26, pairs_graph$Viz.ID.1[j] + 0.26), 
                sector.index2 = pairs_graph$Category.2[j],
                point2 = c(pairs_graph$Viz.ID.2[j] - 0.26, pairs_graph$Viz.ID.2[j] + 0.26), 
                col = domain_colors$OS.Color[i], h.ratio = 0.5, w = 0.8)
  }
}


# Export visual ----
legend(
  x = "topleft",
  inset = c(0.2, 0.08),
  legend = c("Active", "In development", "On hold"),
  col = status_palette,
  pch = 16,
  pt.cex = 0.6,
  cex = 0.4,
  bty = "o",          # draw box
  box.lwd = 0.2,      # thin border
  box.col = "grey55", # border color
  title = "Status",
  title.font = 2      # bold title
)

dev.copy(png, "images/library-report.png",
         width = 5, height = 5, units = "in", res = 600)
dev.off()

       


