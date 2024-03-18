# FruitPhenoBox

This repository contains the Matlab and R scripts of the _FruitPhenoBox_ (https://doi.org/10.21203/rs.3.rs-2632704/v1). The required hardware is described in the publication. Only the software is described here.

## FastSheets

The output of the _apple fruit features extractor_ (_affe_) can be furhter analysed and visualised using the R scripts in the folder **`FactSheets`**:
- **`BuildFactSheets.R`**:
  - Makes a fact sheet per badge oder cultivar.
  - Input:
    - output folder of _affe_
    - orchard plan as a text file (including position of trees & cultivar information)
  - Output:
    - PDF document with one fact sheet per cultivar or badge per page
    - various tab-delimeted text files combining the features of all fruits in a single file
- The other R scripts contain functions that are required by `BuildFactSheets.R`:
  - **`define.shape.per.badge.R`**: extracts the most common shape for each badge according to its correlation with the predefined shapes.
  - **`get.exp.info.R`**: reads in the orchard plan in the required format.
  - **`process.color_txt.R`**: processes the output files in the `color_text` folder. The lines in at the end of the scripts can be uncommented to store the data in a single tab-delimited text file.
  - **`process.polar_txt.R`**: processes the output files in the `polar_text` folder. The lines in at the end of the scripts can be uncommented to store the data in tab-delimited text files.
  - **`process.shape.phenobox.R`**: reorganises the _.txt_ output of _affe_. A folder (`processed`) with three tab-delimited text files is created:
    - `corshapes.txt`: correlation to the 13 predefined shapes per apple and camera
    - `traits.txt`: weight, width, height and symmetry per apple. For the traits extracted from the images (all except weight), the mean and the standard deviation per trait are given.
    - `traitspercam.txt`: width, height and symmetry per apple and camera.
