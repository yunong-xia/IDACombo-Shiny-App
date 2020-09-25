# IDACombo Shiny App

This is a R Shiny App for **IDACombo** package provided by **Alexander Ling**.



## App Structure
Backbone: ui.R, server.R, global.R
Data loader: dataset_import.R
Functionalities for each page: (Normal + Batch)
1. 2 Drug Prediction:
	a. two_drug.R
	b.two_drug_batch.R
2. Control Plus One:
	a. control_plus_one.R
	b.control_plus_one_batch.R
3. Test Vs Control:
	a. test_vs_control.R
	b.test_vs_control_batch.R

There are three main functionalities in IDACombo package: 2 Drug Prediction, Control Plus One, and Test Vs Control. Each of them has a batch version.

All imported libraries and global environment variables are specified in global.R.

Each functionality file define its corresponding ui elements and server logics. Then the wrapped ui functions and server functions will be called in ui.R and server.R

## IDACombo Package Installation in R
IDACombo package is currently only available on GitHub.
````
devtools::install_github("Alexander-Ling/IDACombo")
````

## App Usage
Data should be loaded first. The shiny app has already provided some datasets which you can load. Users can also load their custom shiny app. Instructions are on the Dataset Loader page.

Manuscript of IDACombo functionalities is available in the package. You can learn details of what those functionalities do.


## Contact
Alexander Ling, alling@umn.edu
Yunong Xia, xia00045@umn.edu
