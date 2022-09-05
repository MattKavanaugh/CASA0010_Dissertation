# CASA0010_Dissertation

The objectives of this dissertation are:
1. To create a Land Development Model (LDM) for Turin, Italy;
2. To integrate outputs of the HARMONY Land Use Transportation Interaction Model (LUTI) into the LDM; and
3. To determine what sort of insights can be gained, and at what spatial extent.

To carry out the analysis, run the following files in order: NOTE: use the same working directory for each script.

- 1_data_processing.r - This file downloads and cleans the constraint and attractor layers. NOTE: files are quite large.
- 2_data_rasterization.r - This file explains the rasterization. NOTE: completed in QGIS with plans to replicate in code in future.
- 3_data_modelling.ipynb - This file takes the model input layers

NOTE: 
- due to the size of files invovled, the scripts should automatically download files and none are stored on GitHub.
- the scripts should automatically create the folder structure in your working directory.

Folder Structure: 
- 1_data_processing.r
    - raw_data/zipped - data will be downloaded to here
    - raw_data/unzipped - data will be unzipped to here
    - clean_data - cleaned data will be exported to here
- 2_data_rasterization.r 
    - clean_data - data should be loaded from here
    - inputs_TUR - data should be exported to here
- 3_data_modelling.ipynb
    - inputs_TUR - data will be loaded from here
    - outputs_TUR - data will be exported to here
