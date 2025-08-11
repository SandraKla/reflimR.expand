# *reflimR.expand*: Advanced Reference Limit Estimation Using Routine Laboratory Data

Reference intervals play a crucial role in the medical interpretation and statistical evaluation of laboratory results. By definition, reference intervals include the central 95% of results measured in non-diseased reference individuals.

The *reflimR.expand* package provides a powerful extension to the existing *reflimR* package. While *reflimR* focuses on estimating reference intervals from laboratory data, *reflimR.expand* adds methods specifically designed to address challenges in real-world data, such as:

* handling values below the limit of detection (LOD), and
* applying sliding window techniques for age-dependent reference intervals.

## Installation

To use the *reflimR.expand* package, you must first install its dependency, *reflimR*, from CRAN. Open R and enter the following command in the console:

```bash
install.packages("reflimR")
```

This will download and install the *reflimR* package. Next, install the *reflimR.expand* package directly from GitHub using the *devtools* package:

```bash
# If devtools is not installed yet:
install.packages("devtools")

# Install reflimR.expand from GitHub
devtools::install_github("SandraKla/reflimR.expand")
```

Once the installation is complete, load the package into your R session with:

```bash
library(reflimR.expand)
```

# Usage

After installing and loading the package, you can use the main functions to compute reference intervals with enhanced methods:

- Use the Sliding Window functions to calculate reference intervals based on a sliding window approach, allowing for reference interval estimation across covariates such as age.
- Use the reflimLOD functions to handle data values below the limit of detection (LOD).
