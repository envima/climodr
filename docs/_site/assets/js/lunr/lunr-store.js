var store = [{
        "title": "Welcome to Jekyll!",
        "excerpt":"You’ll find this post in your _posts directory. Go ahead and edit it and re-build the site to see your changes. You can rebuild the site in many different ways, but the most common way is to run jekyll serve, which launches a web server and auto-regenerates your site when...","categories": ["jekyll","update"],
        "tags": [],
        "url": "http://localhost:4000/climodr/jekyll/update/2024/10/28/welcome-to-jekyll.html",
        "teaser": null
      },{
        "title": "Environment",
        "excerpt":"This website is intended to provide additional information for the climodr package, although the provided introductions, explanations and examples might be useful for self-study only, too. This package was created at the Department of Environmental Informatics in Marburg. Development version You can find the most recent development version of the...","categories": ["General Information"],
        "tags": ["Unit","00"],
        "url": "http://localhost:4000/climodr/unit00/unit00-01_learning_environment.html",
        "teaser": null
      },{
        "title": "Recommendations",
        "excerpt":"To run climodr, you need a PC with a running version of R. Using an Interface like R-Studio is also strongly recomended. Hardware requirements In theory you can run climodr on every PC setup. Practically we recommend: at least 8GB of RAM a powerfull CPU for computing models Depending on...","categories": ["General Information"],
        "tags": ["Unit","00"],
        "url": "http://localhost:4000/climodr/unit00/unit00-02_recommendations.html",
        "teaser": null
      },{
        "title": "Frequently Asked Questions",
        "excerpt":"No Frequently Asked Questions yet.  ","categories": ["General Information"],
        "tags": ["Unit","00"],
        "url": "http://localhost:4000/climodr/faq.html",
        "teaser": null
      },{
        "title": "Overview",
        "excerpt":"The Environment in climodr serves as a method to setup the same workspace for every usecase of climodr. The idea of this is to make climodr easy to share and reproduce. By fixing the folder structure there is only one path a user has to enter. The full project can...","categories": ["Environment"],
        "tags": ["Unit","01"],
        "url": "http://localhost:4000/climodr/unit01/unit01-01_overview.html",
        "teaser": null
      },{
        "title": "envi.create",
        "excerpt":"With the envi.create() function, one points out a path where the package should store all its data. There is also the memfrac argument. This argument allows you to change the fraction of your RAM the terra-package is allowed to use. By default this number is pretty low, so this way...","categories": ["Environment"],
        "tags": ["Unit","01"],
        "url": "http://localhost:4000/climodr/unit01/unit01-02_envicreate.html",
        "teaser": null
      },{
        "title": "clim.sample",
        "excerpt":"Load example data Description Climodr comes with a full set of example data. But since this package runs primarily with data, that is not linked to the global environment, but saved in local folders build via ‘envi.create’, one can’t just load example data. This function will load all the example...","categories": ["Environment"],
        "tags": ["Unit","01"],
        "url": "http://localhost:4000/climodr/unit01/unit01-03_climsample.html",
        "teaser": null
      },{
        "title": "Overview",
        "excerpt":"The Pre-Processing phase cleans up your data, creates additional information like spectral indices and makes the data ready for spatial modelling. You can basically jump in using climodr at every step of this phase, just make sure you provide the missing data in the workflow structure in the same format...","categories": ["Pre-Processing"],
        "tags": ["Unit","02"],
        "url": "http://localhost:4000/climodr/unit02/unit02-01_overview.html",
        "teaser": null
      },{
        "title": "prep.csv",
        "excerpt":"Preparing CSV-Data Description Crops input data to the extent size and removes NA-Values Usage prep.csv(method = \"proc\", save_output = TRUE, ...) Arguments method character. “proc” for ready-to-use data in separate .csv-files. “tube” for raw-data from the Tube Data Base. Default “proc”-Method. save_output logical. If cleaned data should be saved permanently...","categories": ["Pre-Processing"],
        "tags": ["Unit","02"],
        "url": "http://localhost:4000/climodr/unit02/unit02-02_prepcsv.html",
        "teaser": null
      },{
        "title": "proc.csv",
        "excerpt":"Processing CSV-Data Description Calculate averaged sensor values aggregated to a given time interval. Usage proc.csv(method = \"monthly\", rbind = TRUE, save_output = TRUE, ...) Arguments method character. Either “daily”, monthly” or “annual”. Also depends on the available data. rbind logical. Create a single file with all climate stations. If FALSE,...","categories": ["Pre-Processing"],
        "tags": ["Unit","02"],
        "url": "http://localhost:4000/climodr/unit02/unit02-03_proccsv.html",
        "teaser": null
      },{
        "title": "spat.csv",
        "excerpt":"Spatial aggregation for CSV-Data Description Extract station coordinates from meta-data and reproject the coordinates to the project coordinate reference system. Usage spat.csv(method = \"monthly\", des_file, crs = NULL, save_output = TRUE, ...) Arguments method character. Either “daily”, monthly” or “annual”. Also depends on the available data. des_file character. The filename...","categories": ["Pre-Processing"],
        "tags": ["Unit","02"],
        "url": "http://localhost:4000/climodr/unit02/unit02-04_spatcsv.html",
        "teaser": null
      },{
        "title": "crop.all",
        "excerpt":"Cropping tiff data Description Crops input data to the extent size and reprojects them into project Coordinate reference system. Usage crop.all( method = \"MB_Timeseries\", crs = NULL, ext = NULL, overwrite = FALSE, ... ) Arguments method character. Use “MB_Timeseries” for now. More methods are planned and will be added...","categories": ["Pre-Processing"],
        "tags": ["Unit","02"],
        "url": "http://localhost:4000/climodr/unit02/unit02-05_cropall.html",
        "teaser": null
      },{
        "title": "calc.indices",
        "excerpt":"Calculate spectral indices Description Calculates a set of spectral indices to have more predictor variables available when further modeling. Usage calc.indices( vi = \"all\", bands = c(\"blue\", \"green\", \"red\", \"nir\", \"nirb\", \"re1\", \"re2\", \"re3\", \"swir1\", \"swir2\"), overwrite = FALSE ) Arguments vi Character. Either “all” or vector containing the preferred...","categories": ["Pre-Processing"],
        "tags": ["Unit","02"],
        "url": "http://localhost:4000/climodr/unit02/unit02-06_calcindices.html",
        "teaser": null
      },{
        "title": "fin.csv",
        "excerpt":"Final aggregation for CSV-Data Description Extract the raster values of all raster layers from a scene at the station coordinates at each time stamp. The extracted data will be attached to the station data so there is a .csv-file with coordinates, sensor data (response values) and extracted raster data (predictor...","categories": ["Pre-Processing"],
        "tags": ["Unit","02"],
        "url": "http://localhost:4000/climodr/unit02/unit02-07_fincsv.html",
        "teaser": null
      },{
        "title": "Overview",
        "excerpt":" ","categories": ["Processing"],
        "tags": ["Unit","03"],
        "url": "http://localhost:4000/climodr/unit03/unit03-01_overview.html",
        "teaser": null
      },{
        "title": "autocorr",
        "excerpt":"Test for Autocorrelation Description Tests the final.csv created with ‘fin.csv’ on autocorrelation to produce reliable models. Usage autocorr( method = \"monthly\", resp, pred, plot.corrplot = TRUE, corrplot = \"Coef\" ) Arguments method character. Choose the time scale your data is preserved in. Either “annual”, “monthly” or “daily”. resp numerical. Vector...","categories": ["Processing"],
        "tags": ["Unit","03"],
        "url": "http://localhost:4000/climodr/unit03/unit03-02_autocorr.html",
        "teaser": null
      },{
        "title": "calc.model",
        "excerpt":"Modelling Description Creates Models for each climate value Usage calc.model( method = \"monthly\", timespan, climresp, classifier = c(\"rf\", \"pls\", \"lm\", \"glm\"), seed = NULL, p = 0.8, folds = \"all\", predrows, mnote = NULL, k = NULL, tc_method = \"cv\", metric = \"RMSE\", doParallel = FALSE, autocorrelation = FALSE, ......","categories": ["Processing"],
        "tags": ["Unit","03"],
        "url": "http://localhost:4000/climodr/unit03/unit03-03_calcmodel.html",
        "teaser": null
      },{
        "title": "climpred",
        "excerpt":"Predict sensor data area wide Description Use the models created using ‘calc.model’ to predict the modeled data onto a full spatial raster scene. Usage climpred(method = \"monthly\", mnote, AOA = TRUE) Arguments method Character. Either “daily”, monthly” or “annual”. Also depends on the available data. mnote Character. Model note to...","categories": ["Processing"],
        "tags": ["Unit","03"],
        "url": "http://localhost:4000/climodr/unit03/unit03-04_climpred.html",
        "teaser": null
      },{
        "title": "Overview",
        "excerpt":" ","categories": ["Plotting"],
        "tags": ["Unit","04"],
        "url": "http://localhost:4000/climodr/unit04/unit04-01_overview.html",
        "teaser": null
      },{
        "title": "climplot",
        "excerpt":"Create Maps using the ‘terra’ package graphic parameters Description Plot results of climodr into maps. Right now maps are created using the terra package. The maps created are very basic. Will be updated to run with tidyterra in future. Usage climplot( mnote, sensor, aoa = FALSE, mapcolors = rev(grDevices::terrain.colors(50)), scale_position...","categories": ["Plotting"],
        "tags": ["Unit","04"],
        "url": "http://localhost:4000/climodr/unit04/unit04-02_climplot.html",
        "teaser": null
      },]
