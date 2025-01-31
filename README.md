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

<img src="https://github.com/giancarlotamburello/sGs_UnMix/blob/main/GIF/sgsunmix_loaddata.gif" width="100%">

Use the `Separator` radio buttons to choose the correct column separator (comma in this instance), then click the `Browser` button to select and upload the file [cf_July2000.csv](https://raw.githubusercontent.com/giancarlotamburello/sGs_UnMix/refs/heads/main/data_examples/cf_July2000.csv). The .csv file should contain a minimum of three columns: two for coordinates (longitude and latitude with any geodetic datum) and one continuous variable for spatial prediction. It is important to select the separator before uploading the file. After that, in the `Select columns` field, the user must select in this order: 1) longitude, 2) latitude, and 3) the continuous variable for spatial prediction. No columns will appear in the `Select columns` field if an incorrect separator is selected. In that case, change the separator and re-upload the file.

The geodetic datum of the coordinate system must be specified as an EPSG numeric code (available at https://epsg.io). If coordinates are expressed as decimal longitude and latitude (EPSG: 4326), only the `Output EPSG` field has to be filled with the corresponding EPSG code. Otherwise, if the coordinates are not decimal WGS84, only the Input EPSG field must be compiled. In our example the user must insert [23033](https://epsg.io/23033).  The spatial points are plotted on a GIS layer (Esri World Imagery by default) and colored with a viridis gradient to have a first glance at the spatial variability of the data. The user can navigate into the interactive world map and choose the preferred GIS-layer between ESRI World Imagery and OpenStreetMap. Additionally, distances or areas can be measured using the rectangle above the polygon on the left side of the map. The color scale palette can be changed with the `Color scale` cascade menu on the right side of the map. The map defaults to displaying the logarithm of the continuous variable; if negative values exist, the app will instead show the raw values. A radio button tool on the right enables users to toggle between raw and logarithmic values if the variable follows a normal or log-normal distribution. 

The user must draw a perimeter on the map to enclose the data for processing. This can be done by drawing a rectangle or a polygon. Alternatively, a .csv file with the vertices of a polygon can be uploaded by pressing `Upload Polygon`. Download the file [cf_July2000_polygon.csv](https://raw.githubusercontent.com/giancarlotamburello/sGs_UnMix/refs/heads/main/data_examples/cf_July2000_polygon.csv) as example. The `Remove Polygon` button can remove the uploaded polygon to define a new perimeter.

<h2> 2. Data panel </h2>

<img src="https://github.com/giancarlotamburello/sGs_UnMix/blob/main/GIF/sgsunmix_data.gif" width="100%">

This panel enables you to review the imported data set. The table lets you examine individual values and search through them. On the right, a panel allows you to select each column of the dataset and view a statistical summary (`Summary`) and a frequency histogram (`Visualization`).

<h2> 3. Mix model tab panel </h2>

<img src="https://github.com/giancarlotamburello/sGs_UnMix/blob/main/GIF/sgsunmix_mix.gif" width="100%">

This panel enables the total density distribution of the data to be unmixed into two or more individual populations, which could reflect different data sources. By default, the sidebar panel is set on an automatic fitting procedure, that is sGs UnMix considers the distribution of the data as made by two lognormal populations and their means, standard deviations, and proportions are calculated using the EM-algorithm by maximizing the log-likelihood. Data distribution can be changed from lognormal to normal, enabling one to work with negative values. The user can change the number of populations in automatic mode or can decide to manually fit the distribution by switching to Manual fit and specifying the parameters defining each individual population in the Mean and St. dev. fields. Alternatively, the user can specify initial parameters using the Initial Guess mode, which is then accommodated by the web app to find the best solution. The log-likelihood output in the sidebar panel provides a measure of the goodness of fit, whose higher the value, the better the fit. To help select the proper number of populations while avoiding overfitting, the user can calculate information criteria, such as the Akaike Information Criterion  (Akaike, 1973). The parameters defined for each population are printed at the bottom of the sidebar and the results are displayed in the main area of the tab panel as density and Q-Q (Quantile-Quantile) plots. The Q-Q plot is a graphical tool used to compare the distribution of a dataset to a theoretical distribution. A single normal distribution appears as a 45° line in the Q-Q plot, whereas n normal populations result in n lines combined by n–1 inflection points. Both density and Q-Q plots can be downloaded as PDF by selecting the Save Plot as PDF button.

<h2> 4. Variogram tab panel </h2>

<img src="https://github.com/giancarlotamburello/sGs_UnMix/blob/main/GIF/sgsunmix_variogram.gif" width="100%">

Test

<h2> 5. sGs tab panel </h2>

<img src="https://github.com/giancarlotamburello/sGs_UnMix/blob/main/GIF/sgsunmix_sgs1.gif" width="100%">


Test

<img src="https://github.com/giancarlotamburello/sGs_UnMix/blob/main/GIF/sgsunmix_sgs2.gif" width="100%">
