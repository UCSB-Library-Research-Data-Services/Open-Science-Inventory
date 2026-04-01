# UCSB Library Open Science Inventory

This repository contains the source code for the **UCSB Library Open Science Inventory Project** and the associated Quarto website showcasing open science-related services, instruction, and community activities across the UCSB Library.


## What the Website Covers

This repository is used to publish the [project website](https://ucsb-library-research-data-services.github.io/Open-Science-Inventory/) via GitHub Pages. The website contains 4 main pages:

- **Home** with project overview andthe interactive diagram
- **Services & Programs** with brief descriptions of Open Science related services & programs offered by the UCSB Library
- **Instruction & Consultation** with brief descriptions of instruction & consultation topics
- **Engagement & Community** with brief description of engagement & community efforts

A central feature of the site is an **interactive chord diagram** that highlights connections among offerings, providers, and open science domains.


## Repository Structure

```text
.
├── _quarto.yml                                   # Quarto website configuration
├── index.qmd                                     # Home page 
├── services.qmd                                  # Services & Programs page
├── instruction.qmd                               # Instruction & Consultation page
├── engagement.qmd                                # Engagement & Community page
├── styles.css                                    # Custom site styling
├── code/
│   └── 001-data-viz.R                            # Script to generate chord diagram
├── data_raw/
│   └── OS-Inventory-list-20260202.xlsx           # Source inventory spreadsheet
├── data_processed/
│   └── label_angles.csv                          # Metadata for interactive image switching
├── images/
│   ├── chord-diagram.gif                         # Animated diagram preview
│   ├── image-preload-links.txt                   # Preload tags for static images
│   └── static/                                   # JPEG/PNG exports
├── .github/
│   └── workflows/main.yml                        # GitHub Actions for Quarto publishing
└── Open-Science_Inventory.Rproj                  # RStudio project file
```


## Notes for Maintainers

* Update the Excel inventory in `data_raw/` when the underlying inventory changes
* Regenerate the chord diagram visualization if inventory structure, labels, or relationships change
* Recompute label-angle metadata and write to `data_processed/label_angles.csv`
* Edit page content in the .qmd source files to reflect the updates


## Disclosure

The first version of this README file was drafted by Codex. The contributors have reviewed this file, made corrections, and expanded on details. 

Additionally, LLMs (e.g., ChatGPT, Google Gemini) were used to troubleshoot code during the development process.





