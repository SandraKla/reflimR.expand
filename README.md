# *reflimR.expand*: Advanced Reference Limit Estimation Using Routine Laboratory Data

Reference intervals play a crucial role in the medical interpretation and statistical evaluation of laboratory results. By definition, reference intervals include the central 95% of results measured in non-diseased reference individuals.

The *reflimR.expand* package provides a powerful extension to the existing [*reflimR* package](https://cran.r-project.org/web/packages/reflimR/index.html). While *reflimR* focuses on estimating reference intervals from laboratory data, *reflimR.expand* adds methods specifically designed to address challenges in real-world data, such as:

* handling values below the limit of detection (LOD),
* applying sliding window techniques for age-dependent reference intervals,
* estimates age-dependent reference intervals groups using decision tree partitioning with *rpart*, and
* and generate new test datasets.

## Installation

You can install `reflimR.expand` directly from GitHub. The snippet below automatically checks and installs required dependencies:

```r
if ("reflimR.expand" %in% rownames(installed.packages())) {
  library(reflimR.expand)
} else {
  if ("devtools" %in% rownames(installed.packages())) {
    library(devtools)
  } else {
    install.packages("devtools")
    library(devtools)
  }
  devtools::install_github("SandraKla/reflimR.expand")
  library(reflimR.expand)
}
```

Once the installation is complete, load the package into your R session using the following command:

```r
library(reflimR.expand)
```

The package will then be ready for use in your R environment (e.g. in RStudio). To see the documentation of the package with all its help files, please enter

```r
help(package = reflimR.expand)
```

## Usage

After installing and loading the package, you can use the main functions to compute reference intervals with enhanced methods:

- **make_data / generate_data_from_ri**: Generates age-dependent synthetic laboratory datasets using mathematical trends or provided reference intervals. Supports Limit of Detection (LOD) threshold flags.
- **reflimLOD**: Handles data values below the limit of detection (LOD). See `vignette("reflimLOD")` for detailed usage.
- **rpart**: Estimates age-dependent reference intervals groups using decision tree partitioning. See `vignette("sliding_rpart")` for detailed usage.
- **lab_mclust**: Indirect reference interval estimation using Gaussian Finite Mixture Models. See `vignette("lab_mclust")` for detailed usage.
- **zlog**: Standardizes laboratory measurements using the zlog transformation. See `vignette("zlog")` for detailed usage.
- **sliding_window**: Applying sliding window techniques for age-dependent reference intervals. See `vignette("sliding_window")` for detailed usage.

## Publication

