# Analyzing and imputing missing values of mobility data.
This is a repository for the final project in PSTAT 234 at UCSB (Spring 2021)

---

## Cloning and running the project locally

This project should be run with R-Studio. For a smoother experience please follow the instructions: 

1. [Download and Install R](https://cran.r-project.org/) for your platform; 
2. [Download and Install R-studio](https://www.rstudio.com/products/rstudio/download/); 
3. Download the file with [mobility data](https://drive.google.com/file/d/1YCmR5DcO5Nn1hEsc6M_wxXszyktV_JBO/view?usp=sharing) from Google drive and store it under data/ directory, so that the full path to the object is set to **data/mobility_subset.zip**. Please, note that you need to be logged in from the UCSB gmail account in order to access the file. Due to file size limitation on Github and due to privacy issues, we decided not to host the file in the public Github repo. The data directory is thus included in the .gitignore file and is not git synced; 
4. Git clone the repository to your local machine; 
5. Run the *.rproj file; 
6. Open and run the *.R script of your interest; 

---

## What this project is about

This project tests five different algorithms for imputation of missing values in mobility data: 

1. Amelia
2. missMDA
3. missForest
4. softImpute
5. MICE

![](https://raw.githubusercontent.com/barguzin/typora-images/master/img/man_maps.png)

The accuracy is assessed via RMSE(D) and is provided in the figure below: 

![](https://raw.githubusercontent.com/barguzin/typora-images/master/img/35aRSF5)



![](https://raw.githubusercontent.com/barguzin/typora-images/master/img/rmse_bar.png)
