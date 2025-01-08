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

To generate the plots used in the paper, follow these steps:

Navigate to the folder code_plot. Use the provided input files to execute the scripts:

Rid_tp_Monthly_ERA5_1940-2023.nc: NetCDF file containing monthly precipitation data.
community_detection_po_basin.csv: Data from community detection analysis in the Po Basin.
Spell_features.csv: Features related to drought.
sara.csv: Supplementary dataset for further analysis.
Run the scripts in the R environment. Upon execution, the plots required for the paper will be automatically generated.

For ShapeFiles used in the paper, you can follow the next steps:

Shapefile: Po River Basin
The shapefile containing the boundaries of the Po River Basin was obtained from the official website of the Po River Basin Authority. This dataset provides the official hydrological boundaries of the Po Basin.
Source: Po River Basin Authority
https://www.adbpo.it/
https://www.adbpo.it/download/webgis/cartografia_di_base/

Shapefile: Lakes and Rivers
The shapefiles for global lakes and river networks were sourced from the HydroSHEDS project. These datasets are ideal for hydrological and ecological studies.
https://www.hydrosheds.org/products/hydrobasins



Feel free to contact us for further clarifications or assistance!
