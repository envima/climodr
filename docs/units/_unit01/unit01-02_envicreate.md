---
title: envi.create
toc: true
header:
  image: '/assets/images/titleimage/envi_create.png'
  caption: 'Image: Environmental Informatics Marburg'
---

With the *envi.create()* function, one points out a path where the package should store all its data. There is also the *memfrac* argument. This argument 
allows you to change the fraction of your RAM the terra-package is allowed to use. By default this number is pretty low, so this way the process can be sped up.
<!--more-->

## Create climodr environment

### Description
Creates an environment climodr will use during the calculation process. A list is returned with all paths to all folders. After creating the environment, all necessary data should be stored into the depending Input sub-folders. There is also an additional temp-folder, where temporary data is stored, which can be deleted after not being used anymore.

### Usage
```r
envi.create(proj_path, memfrac = NULL, ...)
```

### Arguments
**proj_path** 	character. Path to project directory. Climodr will work exclusively in this folder and create all project folders in here.

**memfrac**	numeric. Value between 0 and 0.9. The fraction of RAM that may be used by the terra package

### Value
list. Contains all paths to each folder in the project directory. Necessary for climodr to operate its functions.

## Examples
```r
## Not run: 
# create climodr environment and allow terra-functions to use 70% of RAM
envi.create("C:/user/climodr_user/project_directory",
             memfrac = 0.7)

## End(Not run)
```

Climodr then creates an environment with three main folders:\
- Input (for all necessary data the user must bring)\
- Output (for ready-to-use data created by climodr)\
- Workflow (for climodr to store data during the process)\

![Climodr Environment](../assets/images/unit01/Environment.png)

The Input-Directory is the place, where all data, which shall be used for modelling, should be saved beforehand. It consists of four different folders:\
- dep (Dependency, like a resolution image or metadata)\
- raster (Raster data, work in progress)\
- tabular (Tabular data, containing climate data from the climate stations)\
- vector (Vector data, like the study area or climate station point data)\

See [list of possible inputs](link) for further details, what kind of input-data can be used.\
The Output-Folder is the place, where all final data, which is created by the package, is stored in. It consists of three different folders:\
- maps (basic ready-to-use maps)\
- predictions (plain prediction imagery)\
- statistics (perfomance of the predictions and other statistics)\

The Output-Directory contains all the reade-to-use data in some basic formats, which should be publication-ready if no other needs are wanted or required. 

The Workflow-Directory contains all steps in between the Input and the Output. In here there are models, test and training data, clean tabular data, and so on.
<!--more-->
 
* Do not delete any of these folders, since climodr requires those to run properly!
* The higher you set the fraction of RAM that climodr will use, the slower the PC will become when running climodr, in case you want to do something in parallel on the PC. Using a fraction > 0.8 can even make it hard to use a browser while using climodr.
{: .notice--info}
