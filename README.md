# Aragonite_clumped
This repository serves as a supplement to the paper titled "Temperature dependence of clumped isotopes (∆47) in aragonite", which is currently in preparation.
The paper descibes clumped isotope analyses on aragonitic shells of lab-grown bivalves and discusses the implications for using clumped isotope analyses as a proxy for temperature reconstruction.
The repository contains R code to do the data processing and create figures as descibed in the accompanying paper on the clumped isotope composition of lab-grown aragonitic Arctica islandica shells.
This repository contains 7 R script files and 1 raw data file.

## Dependencies
R files list dependencies required for their operations at the top.
The following packages are used in this repository:

- [tidyverse 1.3.1](https://www.tidyverse.org/packages/)
- [ggplot2 3.3.5](https://ggplot2.tidyverse.org/)
- [ggpubr 0.4.0](https://rpkgs.datanovia.com/ggpubr/)
- [gridExtra 2.3](https://rdrr.io/cran/gridExtra/)
- [bfsl 0.2.0](https://github.com/pasturm/bfsl)
- [RColorBrewer 1.1.2](https://www.r-graph-gallery.com/38-rcolorbrewers-palettes.html)
- [ggrepel 0.9.1](https://github.com/slowkow/ggrepel)

## Running
Download or git clone the directory and source/load the various R scripts.

Alternatively, click the binder button below to launch an interactive RStudio session in the browser.

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/nielsjdewinter/Aragonite_clumped/HEAD)

## File overview
- [`01_Aragonite_compilation.csv`](01_Aragonite_compilation.csv) is the raw
  data file. It contains information about all clumped isotope results (D47)
  used in the paper. The file contains multiple headers which are used in the
  scripts to group data and calculate statistics

- [`02_Data_processing.r`](02_Data_processing.r) is the R script file that
  describes the steps taken to process the raw clumped data in preparation for
  plotting. The data processing workflow includes:
  - Grouping of the data by specimen ("Aisstats") and per temperature ID ("D47stats").
  - Expandeding uncertainties on measurements by bootstrapping (N = 10^5; see "violin_data")
  - A series of regressions fit through the data.
  - Summarizing statistics of these regressions
  - Calculating the residuals of the data relative to the regressions
  - Calculating the mean D47 and temperature offsets of the full Arctica
    islandica dataset with reference to pre established clumped isotope
    regressions

- [`03_Average_error_propagation.r`](03_Average_error_propagation.r) is a
  script descibing functions used to propagate errors on measurements through
  calculations of group averages and standard deviations.

- [`04_Temperature_offset.r`](04_Temperature_offset.r) is a script describing a
  function for calculating the temperature offset of data from a
  D47-temperature regression, used to estimate the temperature bias that would
  result from using the regression.

- [`05_A_islandica_plot.r`](05_A_islandica_plot.r) is a script describing how
  Figure 1 in the manuscript is created (plotting Arctica islandica data
  together with regression results)

- [`06_Full_aragonite_plot.r`](06_Full_aragonite_plot.r) is a script describing
  the plotting of Figure 2 in the manuscript (showing the full aragonite
  dataset together with regression results)

- [`07_Aragonite_residual_plot.r`](07_Aragonite_residual_plot.r) is a script
  describing how Figure 3 in the manuscript is plotted (showing the residuals
  of the data relative to the regression results)

- [`08_Calibration_offset_plot.r`](08_Calibration_offset_plot.r) is a script
  describing the plotting of Figure 4 in the manuscript (showing the offset of
  the full Arctica islandica dataset from the clumped isotope temperature
  calibrations of Anderson et al. and Meinicke et al.)
