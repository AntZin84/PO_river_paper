# PO_river_paper
This README provides a step-by-step guide to reproduce the results and figures presented in the paper on the Po River. Each step corresponds to specific code files and instructions for execution. Ensure you have Stata for seeing the data.

Ensure that all required files are in the same directory for easier execution.

- Execute the Code_und_LAST.txt script to perform the analysis for the undirected network.

- Execute the Code_directed_last.txt script to perform the analysis for the directed network.

- Run the script to generate network statistics and visualizations for both types (Code_und_LAST.txt and Code_directed_last.txt).

- Open the code_distance.txt file and execute the first block of code to calculate the initial distance metrics.

Notes:
Ensure that all scripts are run sequentially to avoid errors due to missing intermediate results.
Save all outputs in the designated folders to ensure figures and results align with the paper's presentation.


# Generating Graphs for the Paper
To generate the plots used in the paper, follow these steps.

Needed directories:
Ensure you have the following 4 subdirectories in the main directory where the source files are stored:

shp_po
lake_po_shp
river_po_shp
data (this is where you should place the following files: sara.csv, community_detection_po_basin.csv, and Spell_features.csv).
For ShapeFiles used in the paper, follow these steps:

Shapefile: Po River Basin
The shapefile containing the boundaries of the Po River Basin was obtained from the official website of the Po River Basin Authority. This dataset provides the official hydrological boundaries of the Po Basin.

Source: Po River Basin Authority
Website
Shapefile Download
Shapefile: Lakes and Rivers
The shapefiles for global lakes and river networks were sourced from the HydroSHEDS project. These datasets are ideal for hydrological and ecological studies.

HydroSHEDS Project
The Output directory will be automatically generated.

Run the scripts in the R environment:

SPI_plot_V3.R: Generates the following graphs:
Figure 1: SPI-12 time series of the Po River Basin with a schematic representation of drought event characteristics: Peak, Duration, and Severity.
Figure 4: Spatial distribution of drought indicators.

NETWORK_plot_V3.R: Generates the following graphs:
Figure 2: Map of the hydrographic and topographic Po River Basin along with ERA5 grid points.
Figure 5: Centrality measures.
Figure 7: Network community detection applied to the Po River Basin.
Figure 8: Distribution of centrality measures across the communities.
Figure 9: Directional network analysis.
Feel free to contact us for further clarifications or assistance!
