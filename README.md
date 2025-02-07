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

By default, the variogram is calculated using a spherical model through an autofitting procedure, which is based on the minimization of a weighted sum of square residuals, and displayed as a table and variogram and variogram cloud in the main area of the tab panel. The user is required to choose a model from the `Variogram model` field (spherical, exponential, pentaspherical, gaussian, circular, bessel, linear) that minimizes the weighted sum of squared errors (SSE, printed in the sidebar), which provides a measure of the goodness of fit. The automatic lag distance is calculated using a formula depending on the density of the data over the grid and its geometry (principal diagonal of the polygon divided by 45) and is printed in the sidebar panel. The app uses it as a starting input. Then, the average distance at which semivariances are calculated and the respective number of pairs (N(h)) used are displayed in the table on the right side of the main panel. If the number of pairs (N(h)) for the lag distance (h) is small, the user can manually insert it in the `Lag distance` field while remaining in autofit mode. If outliers exist within the data set, the variogram cloud shows data clusters with high semivariance at short distances. These outliers would drastically raise the nugget of the variogram. In such a case, the user is advised to enable the `Robust variogram` field or manually remove the outliers. Finally, the user can manually fit the variogram by switching the `Fit` field from `Automatic` to `Manual` and adjusting the structure of the variogram by providing numeric inputs into the `Nugget`, `Partial sill`, and `Range` fields in the sidebar. The variogram plots are reactive to changes in the input of the variogram parameters, updating the empirical variogram fit in real-time. Both table and variogram plot can be downloaded as .csv and PDF, respectively, by pressing the `Save Table as CSV` and `Save Plot as PDF` buttons.

<h2> 5. sGs tab panel </h2>

<img src="https://github.com/giancarlotamburello/sGs_UnMix/blob/main/GIF/sgsunmix_sgs1.gif" width="100%">

This panel enables the user to perform spatial prediction through sequential Gaussian simulation. The sidebar of this panel shows a summary of the grid extent in m (`X min`, `X max`, `Y min`, and `Y max` fields), the cell size of the grid (in m) over which perform sGs (`Delta X` and `Delta Y` fields), and the `Number of simulations` field. By default, the cells have 5 x 5 m spacing, and the algorithm performs 200 simulations, but the user can adjust these values as desired from 1 to 1000. Once these parameters have been set up, the simulations start by pressing the `Run sGs` button. A pop-up message will suggest waiting for completion. After a few seconds, the result is plotted in the main panel as a heat map of the mean values of the N number of simulations, overlaying a GIS-based layer (Esri World Imagery by default). The user can then change the color scale (on the right side of the map) or adjust its minimum and maximum values and the transparency of the heat map by adjusting the Z min, Z max, and Opacity fields in the sidebar and pressing the refresh button. These adjustments can improve the localisation of the spatial distribution of patterns and their relationship with the area morphology, helping to understand possible sources and the processes driving spatial anomalies. This panel also enables calculating the selected variable double integration over the area defined in the Load data panel and its uncertainty, which are printed at the bottom of the left side panel after pressing the Calculate 2D integral button. These values reflect the mean and standard deviation of the outputs estimated for each n equiprobable realizations generated by sGs. In the case of soil CO2 flux data, or any other input variable measured in g m–2 d–1, sGs UnMix  returns the total output and its standard deviation in g d–1. In addition to a mean heat map, sGs UnMix calculates a probability map for the measured parameter exceeding a specified threshold by inserting a number in the probability threshold field before pressing the run sGs button. To help choose the threshold, the user can inspect the populations disentangled from the total data distribution in the Mix model tab. The user can visualize the probability heat map through the layer selection on the left side panel and adjust the color scale and opacity as for the mean heat map. Finally, the Save raster button allows downloading both mean and probability heat maps as Tagged Image File Format (TIFF) for further elaborations on GIS software.

<img src="https://github.com/giancarlotamburello/sGs_UnMix/blob/main/GIF/sgsunmix_sgs2.gif" width="100%">
