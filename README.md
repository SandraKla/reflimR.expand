# *reflimR.expand*: Advanced Reference Limit Estimation Using Routine Laboratory Data

Reference intervals play a crucial role in the medical interpretation and statistical evaluation of laboratory results. By definition, reference intervals include the central 95% of results measured in non-diseased reference individuals.

The *reflimR.expand* package provides a powerful extension to the existing *reflimR* package. While *reflimR* focuses on estimating reference intervals from laboratory data, *reflimR.expand* adds methods specifically designed to address challenges in real-world data, such as:

* handling values below the limit of detection (LOD), and
* applying sliding window techniques for age-dependent reference intervals.

### Installation

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

## Usage

After installing and loading the package, you can use the main functions to compute reference intervals with enhanced methods:

- **make_data / generate_data_from_ri**: Generates age-dependent synthetic laboratory datasets using mathematical trends or provided reference intervals. Supports Limit of Detection (LOD) threshold flags.
- **reflimLOD**: Handles data values below the limit of detection (LOD).
- **rpart sliding window**: Estimates continuous age-dependent reference intervals using decision tree partitioning and sliding window algorithms (ported from [AdRI](https://github.com/SandraKla/AdRI/blob/master/R/window.R) and [AdRI_rpart](https://github.com/SandraKla/AdRI_rpart/blob/main/rpart.R)). See `vignette("sliding_rpart")` for detailed usage.
- **lab_mclust**: Indirect reference interval estimation using Gaussian Finite Mixture Models (ported from [VeRIf](https://github.com/SandraKla/VeRIf)). See `vignette("lab_mclust")` for detailed usage.
- **zlog**: Standardizes laboratory measurements using reference-limit-based z-scores and logarithmic transformations (ported from [VeRIf](https://github.com/SandraKla/VeRIf)). See `vignette("zlog")` for detailed usage.