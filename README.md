# Ahmed-et-al.-2025-JO-
This repository contains all the scripts used in **Ahmed et al., 2025** (Journal of Oceanography).

# Marine Biodiversity Analysis - Manuscript Figures

This repository includes the scripts used to generate the 8 figures in the manuscript "**eDNA Metabarcoding Analysis of Marine Fish Biodiversity**." The figures visualize species distribution, biodiversity indices, and statistical tests related to the performance of different eDNA sampling methods.

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

To run these scripts, you'll need the following dependencies:

### **Python** (for Figures 1 and 5):
- Required Python packages:
  - `matplotlib`, `cartopy`, `numpy`, `seaborn`, `pandas`
  - Install via pip:
    ```bash
    pip install matplotlib cartopy numpy seaborn pandas
    ```

### **R** (for Figures 2, 3, 4, 6, 7, and 8):
- Required R packages:
  - `ggplot2`, `dplyr`, `vegan`, `writexl`, `ggsignif`, `cowplot`, `patchwork`, `indicspecies`
  - Install via R:
    ```R
    install.packages(c("ggplot2", "dplyr", "vegan", "writexl", "ggsignif", "cowplot", "patchwork", "indicspecies"))
    ```

## Usage

1. **Clone or download** the repository to your local machine:
   - You can clone the repository using Git:
     ```bash
     git clone https://github.com/iahmedBD2020/Ahmed-et-al.-2025-JO-.git
     ```
   - Or download it as a ZIP file by clicking the **Download ZIP** option on the GitHub page.

2. **Install necessary dependencies** for Python and R as listed in the Dependencies section above.

3. **Prepare your data files**:
   - Ensure the input data is in the expected format (e.g., CSV, Excel) and is placed in the correct folder for each script.

4. **Run each script**:
   - Execute each script individually in Python or R to generate the respective figures:
     - **Figure 1** (Python): `fig1_plot.py`
     - **Figure 2** (R): `fig2_plot.R`
     - And so on...

## License

This repository is licensed under the MIT License. See LICENSE file for details.

## Disclaimers

- Please refer to the manuscript if you use this code or reproduce any results from this repository.

