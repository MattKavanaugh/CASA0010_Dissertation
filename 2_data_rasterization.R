# Introduction -----------------------------------------------------------------

# Due to HARMONY project requirements, the rasterization was completed in QGIS. 
# However, this script outlines the steps followed in QGIS to rasterize the layers 
# required for the land development model. The entire rasterization process is 
# intended to be automated with a script in the future.

# In this analysis, the land development model is intended to display values of 
# 0 to 1 for all pixels within the study area at a resolution of 10 metres by 10 
# metres. The values represent the development potential of each pixel on a scale 
# of 0 (no potential) to 1 (highest potential) based on different variables 
# representing various development suitability or desirability factors.

# Conversion of each variable to raster data follows three cases for this analysis:

  # CASE 1 - study limits - binary layers (0 or 1)
    # This is the masking layer, the boundary layer that limits the analysis to 
    # the study area. This layer will be converted into a binary raster image with 
    # pixel values either 0 (ex. pixels outside study area) or 1 (ex. pixels inside 
    # study area).
        # layers:
        #        tur_fua

  # CASE 2 - development suitability - binary layers (0 or 1)
    # every layer that prohibits development (ex. conversation area layer) will need 
    # to be converted into a binary raster image with pixel values either 0, when 
    # within the layer boundaries (ex. pixels within conversation area) or 1, when 
    # outside the layer boundaries (ex. pixels outside conversation area). 
        # layers:
        #        bdtre_rail
        #        bdtre_road
        #        bdtre_wtr
        #        pai_fz
        #        ppr_ca
        #        prg_cem
        #        prg_edu
        #        prg_hlth
        #        prg_ind
        #        prg_png
        #        ptc_fc
        #        ptc_unesco
        #        ptc_urbg

  # CASE 3 - development suitability - non-binary layers (0 to 1, cannot be negative)
    # every layer that limits or restricts the development potential in a non-binary
    # manner that can vary per pixel across the study area (ex. slope which changes 
    # per pixel according to elevation). These layers are converted into normalized 
    # raster images with pixel values reflecting any value on a scale of 0 to 1, all 
    # inclusive (ex. 0 being the least and 1 being the most sloped pixel(s) in the 
    # study area). Note the inversion required here, since a higher value (closer 
    # to 1) actually reflects a greater slope, which restricts development, whereas
    # in our model a higher value (closer to 1) actually reflects greater development 
    # potential.
        # layers:
        #        dem_slp

  # CASE 4 - development desirability - non-binary layers (must = 100, can be positive or negative)
    # every layer that ranks zones by an attribute as more or less attractive than 
    # another zone (ex. accessibility score by car per zone). These layers are 
    # converted into raster images, normalized by zone, with pixel values reflecting 
    # the normalized value of the zone they belong to on a scale of 0 to 1, all 
    # inclusive (ex. the least and most accessible pixels by car, grouped 
    # by zone, in the study area). All of these values are columns of the 
    # zones_luti layer.
        # variables:
        #        luti_JAbus19
        #        luti_JArail19
        #        luti_JAcar19
        #        luti_HAbus19
        #        luti_HArail19
        #        luti_HAcar19
        #        luti_JAbus30
        #        luti_JArail30
        #        luti_JAcar30
        #        luti_HAbus30
        #        luti_HArail30
        #        luti_HAcar30

# Normalization for Case 4 occurs in the python script "3_model". After 
# the values are normalized, pixel values for all 27 variables above are combined 
# into various scenarios as inputs into a regression equation. The inversion of 
# slope values for Case 3 occurs as part of the equation (1 - normalized slope values).
# The output is a raster image with calculated values between 0 and 1, inclusive, 
# for every pixel in the study area representing development potential.

# All layers to be rasterized were cleaned in the "1_pre_processing" R script and 
# the steps below assume the same working directory.


# Setup Environment ------------------------------------------------------------

# Open QGIS and use the "Browser" tab to navigate to the folder where your layer
# files are stored. In this case, all the data is stored in the "clean_data/" directory.

# 1. Load all layers by double clicking on them. The layers for this analysis are:
      # "clean_data/bdtre_rail.gpkg" = rail cover
      # "clean_data/bdtre_road.gpkg" = road cover
      # "clean_data/bdtre_wtr.gpkg"  = water cover
      # "clean_data/dem_slp.tif"     = slope
      # "clean_data/zones_luti.gpkg" = land use transportation interaction model zones
      # "clean_data/pai_fz.gpkg"     = flood zones
      # "clean_data/ppr_ca.gpkg"     = conservation areas
      # "clean_data/prg_cem.gpkg"    = cemetery land use
      # "clean_data/prg_edu.gpkg"    = educational land use
      # "clean_data/prg_hlth.gpkg"   = healthcare land use
      # "clean_data/prg_ind.gpkg"    = industrial land use
      # "clean_data/prg_png.gpkg"    = parks and gardens land use
      # "clean_data/ptc_fc.gpkg"     = forest areas
      # "clean_data/ptc_unesco.gpkg" = unesco heritage sites
      # "clean_data/ptc_urbg.gpkg"   = urban green areas
      # "clean_data/tur_fua.gpkg"    = study area

      # Return to the "Layers" tab and you should see all 16 layers loaded.

# 2. Set the CRS
      # a. holding the "shift" key, click the top listed layer, and still holding the 
      #    the "shift" key, click the bottom layer. right click and navigate to "Set 
      #    CRS" in the drop down menu. 
      # b. navigate to the "filter" search bar in the "Coordinate Reference System 
      #    Selector", type "32632", and click "WGS 84 / UTM zone 32 N"
      # c. click "okay"
      # d. to confirm, right click any layer, click properties in the drop down menu, click
      #    the settings tab (wrench infront of a gear), and look at the CRS indicated
      #    under "Geometry and Coordinate Reference System"

# 3. Save the QGIS project as "2_rasterize" in the working directory by clicking 
#    the "save" button in top left.

# 4. Create two new folders in the working directory called "inputs_TUR" and "outputs_TUR"


# PART 1 - Create Covering Layer --------------------------------------------------------

# The covering will be used to create the extent of all of the rasters to ensure 
# all rasters cover the exact same area. This is key to ensure the pixels of all 
# raster layers are an exact match.

# 1. Ensure that the tur_fua layer is visible

# 2. Click "Layer" from top toolbar

# 3. Navigate to "Create Layer" from the drop down menu

# 4. Click "New Shapefile Layer" from the drop down sub menu.

# 5. In the "New Shapefile Layer" window:

    # a. click the 3 dots on the right of "File name", navigate to the "raw_data/" 
    #    folder, name the layer "covering" and click save. the "file name" field 
    #    will automatically populate

    # b. under "Geometry type" select "Polygon" from the drop down menu

    # c. click CRS and make sure it is "WGS 84 / UTM zone 32 N"

    # d. under "New Field", navigate to the "Name" field and type "new", then 
    #    click the "Add to Fields List" button, and click "okay". a new layer
    #    called "covering" will automatically be created in your workspace.

# 6. Make a rectangle

    # a. Click the layer you just created

    # b. click the "toggle editing" button (pencil) from the top toolbar

    # c. click the "Add rectangle from extent" button (rectangle with a star 
    #    in bottom right corner) from the top toolbar

    # d. click the top left of the study area (tur_fua) and drag the box to the 
    #    bottom right. right click your mouse when you are satisfied that the 
    #    rectangle includes your entire study area (tur_fua). Try to make the 
    #    rectangle as small as possible while including the entire study area. 
    #    If you make a mistake, hit the "esc" key to start over.

    # e. In the "layer - Feature Attributes" window, type "111" in the "id" 
    #    field and click "okay". The rectangle should be created in the "covering" 
    #    layer.

# 6. Fix the extent of the rectangle to ensure it matches desired pixel size

    # a. ensure the "covering" layer and the"toggle editing" button (pencil) are
    #    still clicked

    # b. click the "Vertex Tool" button (elbow joint with hammer and screwdriver
    #    in the bottom right) from the top toolbar

    # c. hover over a corner of the rectangle until a red circle appears, then 
    #    right click. A "Vertex Editor" panel should appear. If you left click
    #    by accident, press the "esc" key to start over. 

    # d. in the Vertex Editor" panel, look at the x and y columns and note that 
    #    the numbers are not even/whole. manually round the numbers to a multiple 
    #    of your desired pixel size. For this analysis we want the pixel size to 
    #    reflect 10 metre by 10 metre squares of our study area. therefore, we 
    #    will round all x and y values to multiples of 10.

    # e. double click each value of x and y one by one, round the number, and 
    #    hit the "enter" key. as an example, for this analysis, an x value of 
    #    364946.8283 will become 364950 which is a multiple of 10.

    # f. unclick the "toggle editing" button (pencil) from the top toolbar and 
    #    click "save"


# PART 2 - Create Raster Masking Layer (CASE 1) ------------------------------------

# This section shows how to create rasterized masking layer by converting vector 
# data to 10 metre by 10 metre pixels using the extents of the covering layer.
# This section should be repeated for all layers listed under Case 1.
  
# 1. Select the "covering" layer.

# 2. Click "Raster" from top toolbar

# 3. Navigate to "Conversion" from the drop down menu

# 4. Click "Rasterize (Vector to Raster)" in drop down sub menu

# 5. In the "Rasterize (Vector to Raster)" window:

    # a. under "input layer" select the study area, which for this analysis 
    #    is the "tur_fua" layer.
    
    # b. under "A fixed value to burn", type 1
    #    **VERY IMPORTANT** - this number indicates the value to assign to all 
    #    pixels for the area within the "input layer", which in this case is the study
    #    area boundaries. We want all pixel values within the layer to start at an initial 
    #    value of 1.
    
    # c. under "Output raster size units" select "Georeferenced units" 
    
    # d. under "Width/Horizontal resolution" type 10 (since we want 10m pixel width)
    
    # e. under "Height/Vertical resolution" again type 10 (since we want 10m 
    #    pixel height)
    
    # f. under "Output extent", click the 3 dots on the right, navigate to 
    #    "Use Layer Extent"
    
    # g. in the "Select Extent" window, select the "covering" layer in the drop down 
    #    menu, and click "okay"
    
    # h. under "Assign a specified nodata value to output bands", type -9999
    
    # i. under "Pre-initialize the output image with value", type 0
    #    **VERY IMPORTANT** - this number indicates the value to assign to all 
    #    pixels for the area outside the "input layer" which in this case, again, 
    #    is our study area boundaries. We want all pixel values outside the study 
    #    area to be 0 (zero), or no development potential, since we are not
    #    including these pixel in the analysis.
    
    # j. under "Rasterized", click the 3 dots on the right, and click to "Save to File" 
    
    # k. navigate to the "inputs_TUR" folder, type "tur_fua" in "File Name", ensure 
    #    "Save as type" is set to "Tif files", and click "save"
    
    # l. click "run" in the bottom right. A new layer called "tur_fua" will 
    #    automatically appear in the work space. Looking at it, the study area 
    #    should be white (value of 1) and everywhere outside the study area should 
    #    be black (value of 0)


# PART 3 - Rasterize Vector Layers (CASE 2)-------------------------------------------------

# This section shows how to create a rasterized layer by converting vector 
# data to 10 metre by 10 metre pixels using the extents of the covering layer.
# This section should be repeated for all layers listed under Case 2.

# 1. Select the desired layer under Case 2. For this example, I will use "bdtre_rail".

# 2. Click "Raster" from top toolbar

# 3. Navigate to "Conversion" from the drop down menu

# 4. Click "Rasterize (Vector to Raster)" in drop down sub menu

# 5. In the "Rasterize (Vector to Raster)" window:

    # a. under "input layer" select any layer found under Case 2.
    
    # b. under "A fixed value to burn", type 0
    #    **VERY IMPORTANT** - this number indicates the value to assign to all 
    #    pixels for the area within the "input layer", which in this case are the areas
    #    in the study area where development is prohibited. Therefore, we want all 
    #    pixel values within the layer to equal 0.
    
    # c. under "Output raster size units" select "Georeferenced units" 
    
    # d. under "Width/Horizontal resolution" type 10 (since we want 10m pixel width)
    
    # e. under "Height/Vertical resolution" again type 10 (since we want 10m 
    #    pixel height)
    
    # f. under "Output extent", click the 3 dots on the right, navigate to 
    #    "Use Layer Extent"
    
    # g. in the "Select Extent" window, select the "covering" layer in the drop down 
    #    menu, and click "okay"
    
    # h. under "Assign a specified nodata value to output bands", type -9999
    
    # i. under "Pre-initialize the output image with value", type 1
    #    **VERY IMPORTANT** - this number indicates the value to assign to all 
    #    pixels for the area outside the "input layer" which in this case, again, are 
    #    the areas in the study area where development is prohibited. Therefore, we 
    #    want all pixel values outside the layer to be 1.
    
    # j. under "Rasterized", click the 3 dots on the right, and click to "Save to File" 
    
    # k. navigate to the "inputs_TUR" folder, type "bdtre_rail" in the "File Name"
    #    field, ensure "Save as type" is set to "Tif files", and click "save".
    
    # l. click "run" in the bottom right. A new layer with the name "bdtre_rail"  
    #    will automatically appear in the work space. Looking at it, the layer area  
    #    should be black (value of 0) and everywhere outside the layer area should 
    #    be white (value of 1)

    # m. Repeat 5.a. to 5.l. for the remaining 12 layers found under Case 2.


# PART 4 - Convert Slope Raster Layer (CASE 3)-------------------------------------------------

# This section shows how to convert a raster with a resolution of 5 metre by 5 
# metre pixels to a raster with a resolution of 10 metre by 10 metre pixels using 
# the extents of the covering layer. This section should be repeated for slope 
# layer listed under Case 3, which we already created from the digital elevation 
# model during pre-processing.

# 1. Select the "dem_slp" layer.

# 2. Right click and select properties, navigate to the information tab on the left,
#    and look at "Pixel Size" under "Information from Provider". You will see 5,-5.
#    This means that the pixels of the dem_slp layer are 5 metres by 5 metres, 
#    However, we need them to be 10 metres by 10 metres.

# 3. Click "Cancel" in the bottom right.

# 4. Right click the "dem_slp" layer again, navigate to "Export", and select 
#    "Save As" from the drop down menu

# 5. In the "Save Raster Layer as" window:

    # a. Click the 3 dots to the right of "File name" and navigate to the "inputs_TUR"
    #    folder 

    # b. Type "dem_slp_10m" in the "File name" field, ensure "Save as type" is 
    #    GeoTIFF, and click the "Save" button
    
    # c. under "CRS" make sure "EPSG:32632 - WGS 84/UTM zone 32N" is selected
    
    # d. under "Extent (current layer)", click "Calculate from Layer", and select 
    #    the "covering" layer
    
    # e. under "Resolution (current:layer)", change "Horizontal" field to 10, and
    #    change "Vertical" field to 10 (since we want 10m pixel width by 10m 
    #    pixel height)
    
    # f. click "okay" button. A new layer with the name "dem_slp_10m" will 
    #    automatically appear in the work space. Zoom in on an edge and compare the
    #    "dem_slp_10m" layer to the "dem_slp". You should see that the pixels have
    #    doubled in size.


# PART 5 - Rasterize Vector Layers by Attribute (CASE 4)---------------------------------

# This section shows how to rasterize vector data to 10 metre by 10 metre pixels 
# using the extents of the covering layer. This section should be repeated for 
# all variables of the zones_luti layer listed under Case 4.

# 1. Select the zones_luti layer.

# 2. Click "Raster" from top toolbar

# 3. Navigate to "Conversion" from the drop down menu

# 4. Click "Rasterize (Vector to Raster)" in drop down sub menu

# 5. In the "Rasterize (Vector to Raster)" window:

    # a. under "input layer" select "zones_luti" layer.
    
    # b. under "Field to use for a burn-in value" select one of the variables 
    #    listed under Case 4. For this example, I will use "HAcar19".

    # b. under "A fixed value to burn" make sure it is blank (it should say "Not 
    #    set" in faint grey).
    
    # c. under "Output raster size units" select "Georeferenced units" 
    
    # d. under "Width/Horizontal resolution" type 10 (since we want 10m pixel width)
    
    # e. under "Height/Vertical resolution" again type 10 (since we want 10m 
    #    pixel height)
    
    # f. under "Output extent", click the 3 dots on the right, navigate to 
    #    "Use Layer Extent"
    
    # g. in the "Select Extent" window, select the "covering" layer in the drop down 
    #    menu, and click "okay"
    
    # h. under "Assign a specified nodata value to output bands", type -9999
    
    # i. under "Pre-initialize the output image with value" make sure it is blank 
    #    (it should say "Not set" in faint grey).
    
    # j. under "Rasterized", click the 3 dots on the right, and click to "Save to File" 
    
    # k. navigate to the "inputs_TUR" folder, type "luti_JAcar19" in "File Name", 
    #    ensure "Save as type" is set to "Tif files", and click "save".
    
    # l. click "run" in the bottom right. A new layer called "luti_JAcar19" will 
    #    automatically appear in the work space. Looking at it, areas with higher 
    #    job accessibility will be lighter (closer to 1) and areas with lower
    #    job accessibility will be darker (closer to 0)
    
    # m. Repeat 5.a. to 5.l. for the remaining 11 layers found under Case 4.


# END OF RASTERIZATION SCRIPT --------------------------------------------------------

# we now have 27 tif files within the "inputs_TUR" folder

# See script "3_model" for next steps

# NOTE: Due to HARMONY porject requirements, script "3_model" is in python.


# QGIS CREDITS! --------------------------------------------------------

# How to change the raster resolution. (2022). Available at: https://www.youtube.com/watch?v=U-ZfGa_8Wqw (Accessed: 5 September 2022).
