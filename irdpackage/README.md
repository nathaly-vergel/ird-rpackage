# irdpackage: An R package for Interpretable Regional Descriptors 

`irdpackage` is an R package for computing Interpretable Regional Descriptors
(IRDs). IRDs describe regions around an observation where feature values can
change while the model prediction remains within a user-defined range.

In practical terms, IRDs answer questions like:

> For this observation, how far can the input features vary while the prediction
> stays effectively the same?

The package provides a unified R6-based interface for several model-agnostic IRD
methods, plus tools for post-processing, analyzing and visualizing the resulting
hyperboxes.

## Available methods

The implemented methods are adapted or modified versions of:

- [Maximum-Box Approach (MaxBox)](https://link.springer.com/article/10.1023/A:1020546910706)
- [Patient Rule Induction Method (PRIM)](https://link.springer.com/article/10.1023/A:1008894516817)
- [Model-Agnostic Interpretable Rule Extraction Procedure (MAIRE)](https://link.springer.com/chapter/10.1007/978-3-030-84060-0_21)
- [Anchors](https://ojs.aaai.org/index.php/AAAI/article/view/11491)

The package also includes a post-processing procedure for refining generated
boxes.

## Installation

You can install the development version of **irdpackage** directly from GitHub:

```r
install.packages("remotes")
remotes::install_github("nathaly-vergel/ird-rpackage", subdir = "irdpackage")
```

Then load the package:

```r
library(irdpackage)
```

## Developer Setup

If you want to contribute or work on the package locally, clone the repository:

```bash
git clone https://github.com/nathaly-vergel/ird-rpackage.git
cd ird-rpackage/irdpackage
```

Then in R:

```r
install.packages("renv")
renv::restore()

devtools::load_all()
```

## Reproducibility
This project uses `renv` to make the R package environment reproducible. After
opening the project in R or RStudio, restore the recorded dependency versions:

```r
install.packages("renv")
renv::restore()
```

After the environment is restored, load the package for development:

```r
devtools::load_all()
```

Useful development commands:

```r
devtools::document()
devtools::test()
devtools::check()
```

The package also contains a vignette in `vignettes/irdpackage-vignette.Rmd`.
Build it with:

```r
devtools::build_vignettes()
```

## Quick Example

The high-level helper `find_ird()` computes an IRD for a single observation
using one of the available methods:

```r
library(irdpackage)
library(iml)
library(randomForest)

set.seed(1)
model = randomForest(mpg ~ ., data = mtcars)

predictor = Predictor$new(
  model = model,
  data = mtcars,
  y = "mpg"
)

x_interest = mtcars[1, ]
prediction = predict(model, x_interest)

box = find_ird(
  predictor = predictor,
  x_interest = x_interest,
  method = "prim",
  desired_range = c(prediction - 2, prediction + 2)
)

box
```

The generated descriptor can then be refined with `postprocess_box()`:

```r
box_refined = postprocess_box(
  object = box,
  predictor = predictor,
  x_interest = x_interest,
  desired_range = c(prediction - 2, prediction + 2)
)
```

## Package Structure

- `R/`: package source code
- `man/`: generated documentation
- `tests/testthat/`: unit tests
- `vignettes/`: longer-form documentation and examples
- `renv.lock`: reproducible dependency lockfile

## License

This package is licensed under LGPL-3. See `LICENSE` for details.
