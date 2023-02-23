# Basic RGB clustering

This R code can be used to manually identify clusters in RGB raster images.  While it will work on any RGB raster image, it was written to be used on stacks of X-ray maps and/or backscattered electron (BSE) images.

### Dependencies:
- "raster" and "rgdal" to process raster images
- "rgl" to make 3D plots

### Basic workflow:
- Configure the import path variables so R can find the image
- Run the "Initialization" section to import packages and 
- Run the self-explanatory, yet poorly-named "Retrieve flagged pixels over image area" section
- Make some plots from the following sections as needed
- Scrutinize the plots with your eyeballs to see if any clusters can be clearly identified
- Finally, in the "clusters" section, plot a ternary diagram and then create as many variables as needed to hold clusters manually identified using the locate_poly function.
- Be careful with the locate_poly function. sometimes the locate function doesn't work correctly
- Save cluster maps to csv and a raster image, and check to see if you're happy with the results -- maybe open the csv as a text image in ImageJ

### TODO:
- blog update for this release
- MIT license
- add comments within functions
- Clean up some of the section names and unused code
- write a function with a "while" loop and graphical menu to assist in the identification of clusters using the locate_poly function
- the ternary plot appears to sometimes show data outside the triangle.  That don't make sense none!
- add some code to do scatter plots on the clusters to make sure they are unimodal.
