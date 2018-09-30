# Analysis of Motorcyle insurance data

This repo contains an analysis of policies and claims from a (now closed) Swedish insurance company named Wasa.

## Development

The source code is written in `R Markdown` and can be found in the file `analysis.Rmd`. To render the output in the GitHub friendly `Markdown`  simply run

```r
> rmarkdown::render("analysis.Rmd")
``` 
in a `R` console.

## Dependencies

```r
install.package("rmarkdown")
install.package("ggplot2")
install.package("insuranceData")
```