# UCSB Library Open Science Inventory

This repository contains the source for the **UCSB Library Open Science Inventory Project** and the output Quarto website that showcases open science-related services, instruction, and community activities across the UCSB Library.


## What the Website Covers

This repository is used to publish the [project website](https://ucsb-library-research-data-services.github.io/Open-Science-Inventory/) via GitHub Pages. The website contains 4 main pages:

- **Home** providing an overview to the project and dispalying the interactive diagram
- **Services & Programs** providing brief description to services & programs
- **Instruction & Consultation** providing brief description to instruction & consultation topics
- **Engagement & Community** providing brief description to engagement & community effrots

A central feature of the site is an **interactive chord diagram** that highlights connections among offerings, providers, and open science domains.


## Repository Structure

```text
.
├── _quarto.yml                           # Quarto website configuration
├── index.qmd                             # Home page 
├── services.qmd                          # Services & Programs section
├── instruction.qmd                       # Instruction & Consultation section
├── engagement.qmd                        # Engagement & Community section
├── styles.css                            # Custom site styling
├── code/
│   └── 001-data-viz.R                    # Script to generate chord diagram
├── data_raw/
│   └── OS-Inventory-list-20260202.xlsx   # Source inventory spreadsheet
├── data_processed/
│   └── label_angles.csv                  # Metadata for interactive image switching
├── images/
│   ├── chord-diagram.gif                 # Animated diagram preview
│   ├── image-preload-links.txt           # Preload tags for static images
│   └── static/                           # JPEG/PNG exports
├── .github/
│   └── workflows/main.yml                # GitHub Actions for Quarto publishing
└── Open-Science_Inventory.Rproj          # RStudio project file
```


## Notes for Maintainers

* Update the Excel inventory in `data_raw/` when the underlying inventory changes
* Regenerate the chord diagram visualization if inventory structure, labels, or relationships change
* Recompute and write label-angle metadata to `data_processed/label_angles.csv`
* Edit page content in the .qmd source files to reflect the updates


## Disclosure

The first version of this ReadMe file was drafted by Codex. The contributors have reviewed the file, make corrections, and expanded on details. 

Additionally, LLMs (e.g., ChatGPT, Google Gemini) were used to troubleshoot code during the development process.





