# Ahmed-et-al.-2025-JO-
This is the repository containing all the scripts used in Ahmed et al., 2025 (Journal of Oceanography)
# Marine Biodiversity Analysis - Manuscript Figures

This repository contains scripts for generating the 8 figures in the manuscript "eDNA Metabarcoding Analysis of Marine Fish Biodiversity." The figures visualize species distribution, biodiversity indices, and statistical tests related to the performance of different eDNA sampling methods.

## Scripts Overview
The repository includes the following scripts:
1. **Figure 1**: Map of marine sampling cruises with plotted coordinates.
   - Script: `fig1_plot.py`
2. **Figure 2**: Relative abundance bar plot with top species in different sampling methods.
   - Script: `fig2_plot.R`
3. **Figure 3**: Rarefaction curves for different biodiversity indices.
   - Script: `fig3_plot.R`
4. **Figure 4**: Dendrogram of clustering analysis using species data.
   - Script: `fig4_plot.R`
5. **Figure 5**: Map displaying clustered locations with different markers.
   - Script: `fig5_plot.py`
6. **Figure 6**: Boxplots of biodiversity indices with Wilcoxon test results.
   - Script: `fig6_plot.R`
7. **Figure 7**: Indicator species analysis for cluster comparisons.
   - Script: `fig7_analysis.R`
8. **Figure 8**: NMDS plot with environmental variables fitted.
   - Script: `fig8_analysis.R`

## Dependencies
- **Python** (for Figure 1 and 5):
  - `matplotlib`, `cartopy`, `numpy`, `seaborn`, `pandas`
  - To install: `pip install matplotlib cartopy numpy seaborn pandas`
- **R** (for Figures 2, 3, 4, 6, 7, 8):
  - Required R packages: `ggplot2`, `dplyr`, `vegan`, `writexl`, `ggsignif`, `cowplot`, `patchwork`, `indicspecies`
  - To install: 
    ```R
    install.packages(c("ggplot2", "dplyr", "vegan", "writexl", "ggsignif", "cowplot", "patchwork", "indicspecies"))
    ```

## Usage
1. Clone or download the repository.
2. Install necessary dependencies (see Dependencies section above).
3. Prepare your data files in the expected format (e.g., CSV, Excel).
4. Run each script individually for the respective figures.

For example:
- To generate Figure 1 (Map of marine sampling cruises), run:
  ```bash
  python fig1_plot.py

