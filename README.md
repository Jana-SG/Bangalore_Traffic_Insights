# Bangalore Traffic Insights (R Shiny Dashboard)

An interactive **R Shiny** dashboard for exploring urban traffic patterns in **Bangalore** using visual analytics.
The app follows a story-driven flow to uncover patterns related to **congestion**, **traffic volume**, **signal compliance**, **roadwork activity**, **weather conditions**, and **temporal trends**.

🔗 **Live App:** https://janagodieh25.shinyapps.io/BangaloreTrafficInsight/

---

## Key Features

- **Exploratory Analysis**: distribution plots, treemaps, correlations, and PCA-based views  
- **Traffic Signal Compliance**: area-level compliance + monthly compliance heatmap  
- **Traffic Volume & Impact**: ridgeline and distribution comparisons across intersections  
- **Roadwork & Construction**: congestion comparison + interactive nested pie (Plotly)  
- **Weather Effects**: congestion/capacity behavior under different conditions  
- **Temporal Insights**: year-over-year comparisons and trend visualizations  
- **Deeper Dive**: interactive **Leaflet** map highlighting key congestion gateways  
- **Expanded Analysis**: hexbin time-density views + parking/public transport relationship plots  

---

## Repository Structure

- `BangaloreTrafficInsightsApp.R` — main Shiny application (UI + server)
- `Bangalore Traffic Insights.pdf` — report / write-up
- `Bangalore traffic presentation.pptx` — presentation deck
- `README.md` — project overview and run instructions

> **Dataset note:** the app reads a CSV file:
> `Banglore_traffic_Dataset.csv` (must be available locally when running the app).

---

## Requirements

- R (recommended: **R 4.1+**)
- R packages used include (not exhaustive):  
  `shiny`, `bslib`, `tidyverse`, `ggplot2`, `plotly`, `leaflet`, `viridis`, `lubridate`,
  `treemapify`, `ggcorrplot`, `ggridges`, `forecast`, `ggrepel`, `ggalluvial`, `ggdist`, and others.

---

## How to Run Locally

1) **Clone the repository**
```bash
git clone https://github.com/Jana-SG/Bangalore_Traffic_Insights.git
cd https://github.com/Jana-SG/Bangalore_Traffic_Insights.git
```
2) **Place the dataset file in the project folder**
  Make sure Banglore_traffic_Dataset.csv is in the same directory as BangaloreTrafficInsightsApp.R
3) **Install packages (first time only)**
  Open R / RStudio and run:
  ```bash
  install.packages(c(
    "shiny","bslib","tidyverse","ggplot2","plotly","leaflet","viridis","lubridate",
    "treemapify","ggcorrplot","ggridges","forecast","ggrepel","ggalluvial","ggdist",
    "ggmosaic","ggforce","cowplot","scales","factoextra","ggthemes","RColorBrewer","CGPfunctions"
  ))
  ```
4) **Run the code**
   ```bash
   shiny::runApp("BangaloreTrafficInsightsApp.R")
   ```

## Notes / Troubleshooting

- If the app fails to launch due to missing packages, install them and re-run.
- If you see a “file not found” error, confirm the dataset file name matches exactly:
Banglore_traffic_Dataset.csv
- Interactive charts (Plotly / Leaflet) work best when run in RStudio Viewer or a web browser.


