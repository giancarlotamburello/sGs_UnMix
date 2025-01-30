<h1> sGs UnMix </h1>

This application streamlines spatial prediction workflows and minimizes subjectivity in statistical analysis, making it accessible to the entire geoscience community. The sGs Web App is developed using the Shiny package for R Studio and is structured into four primary panels. These panels facilitate tasks such as data loading and coordinate projection, separating data sources and defining thresholds, modelling spatial continuity through variograms, and performing sequential Gaussian simulation (sGs) for spatial prediction. The app features automated variogram fitting and data unmixing to reduce user bias and improve reproducibility. Predicted value heat maps, overlaid on satellite or geographic map layers, are dynamically refreshed based on input values, allowing for rapid spatial patterns and anomalies visualisation. This web app can serve as a standardized method for estimating volcanic volatile fluxes (e.g., soil CO2 emissions) both locally and globally, and it can also be applied across various geoscience disciplines, including ore deposit mapping, hydrocarbon exploration, environmental monitoring, and climate research. Unlike existing geostatistical tools, this web app provides automated functionalities, enhanced interactivity, and dynamic, responsive outputs such as tables and plots. Additionally, it offers the flexibility of a platform-independent, standalone web-based solution, making it an invaluable resource for researchers and practitioners in the field.

sGs UnMix can be used online at [http:\\shiny.bo.ingv.it:8790](http:\\shiny.bo.ingv.it:8790)

<h1> App dependencies </h1>

You can run this application locally, but it needs several packages to be installed. The code for manual installation is as follows:

```r
install.packages("shiny")
install.packages("fields")
install.packages("leaflet")
install.packages("leaflet.extras")
install.packages("leaflet.providers")
install.packages("DT")
install.packages("mixtools")
install.packages("gstat")
install.packages("sp")
install.packages("raster")
install.packages("splancs")
```

<h1> User Guide </h1>

sGs UnMix consists of five tab panels: Load data, Data, Mix model, Variogram, and sGs. The Data panel shows data in a table on the left and a summary and histogram of a selected variable on the right. The remaining four panels have a left sidebar for inputs and a large main area for outputs, such as figures and tables. The sidebars contain several fields and a brief description to guide the user. A new user can take these steps to analyze the example found in data_examples.

<h2> 1. Load data panel </h2>

Use the `Separator` radio buttons to choose the correct column separator (comma in this instance), then click the `Browser` button to select and upload the file `example1.csv`. The .csv file should contain a minimum of three columns: two for coordinates (longitude and latitude with any geodetic datum) and one continuous variable for spatial prediction. It is important to select the separator before uploading the file. After that, in the `Select columns` field, the user must select in this order: 1) longitude, 2) latitude, and 3) the continuous variable for spatial prediction. No columns will appear in the `Select columns` field if an incorrect separator is selected. In that case, change the separator and re-upload the file.

The geodetic datum of the coordinate system must be specified as an EPSG numeric code (available at https://epsg.io). If coordinates are expressed as decimal longitude and latitude (EPSG: 4326), only the `Output EPSG` field has to be filled with the corresponding EPSG code. Otherwise, if the coordinates are not decimal WGS84, only the Input EPSG field must be compiled. In our example the user must insert [32633](https://epsg.io/32633).  The spatial points are plotted on a GIS layer (Esri World Imagery by default) and colored with a viridis gradient to have a first glance at the spatial variability of the data. The color scale can be switched from logarithmic to raw data through a panel on the right side of the map, along with others color palette. The user can navigate into the interactive world map and choose the preferred GIS-layer between ESRI World Imagery and OpenStreetMap. In addition, one can measure distances or areas through the rectangle above the polygon on the left side of the map.

The user must draw a perimeter on the map to enclose the data for processing. This can be done by drawing a rectangle or a polygon. Alternatively, a .csv file with the vertices of a polygon can be uploaded by pressing `Upload Polygon` (and downloaded LINK). The `Remove Polygon` button can remove the uploaded polygon to define a new perimeter.


<h2> 2. Data panel </h2>

Test

<h2> 3. Mix model tab panel </h2>

Test

<h2> 4. Variogram tab panel </h2>

Test

<h2> 5. sGs tab panel </h2>



Test

