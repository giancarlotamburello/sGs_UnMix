<h1> sGs UnMix </h1>

This application streamlines spatial prediction workflows and minimizes subjectivity in statistical analysis, making it accessible to the entire geoscience community. The sGs Web App is developed using the Shiny package for R Studio and is structured into four primary panels. These panels facilitate tasks such as data loading and coordinate projection, separating data sources and defining thresholds, modelling spatial continuity through variograms, and performing sequential Gaussian simulation (sGs) for spatial prediction. The app features automated variogram fitting and data unmixing to reduce user bias and improve reproducibility. Predicted value heat maps, overlaid on satellite or geographic map layers, are dynamically refreshed based on input values, allowing for rapid spatial patterns and anomalies visualisation. This web app can serve as a standardized method for estimating volcanic volatile fluxes (e.g., soil CO2 emissions) both locally and globally, and it can also be applied across various geoscience disciplines, including ore deposit mapping, hydrocarbon exploration, environmental monitoring, and climate research. Unlike existing geostatistical tools, this web app provides automated functionalities, enhanced interactivity, and dynamic, responsive outputs such as tables and plots. Additionally, it offers the flexibility of a platform-independent, standalone web-based solution, making it an invaluable resource for researchers and practitioners in the field.

sGs UnMix can be used online at [http:\\shiny.bo.ingv.it:8790](http:\\shiny.bo.ingv.it:8790)

<h1> App dependencies </h1>

This application requires several packages to be installed for local execution. The code for manual installation is as follows:

```r
install.packages("shiny")
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

sGs UnMic consists of five tab panels: Load data, Data, Mix model, Variogram, and sGs. The Data panel shows data in a table on the left and a summary and histogram of a selected variable on the right. The remaining four panels have a left sidebar for inputs and a large main area for outputs, such as figures and tables. The sidebars contain several fields and a brief description to guide the user.


<h2> 1. Load data panel </h2>

Test

<h2> 2. Data panel </h2>

Test

<h2> 3. Mix model tab panel </h2>

Test

<h2> 4. Variogram tab panel </h2>

Test

<h2> 5. sGs tab panel </h2>



Test

