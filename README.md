# USevolve Package

## Package Overview

For millions of people around the world, 2020 has been one of the most difficult years in memory. Millions of people have contracted the novel Coronavirus known as Covid-19 and as of December 2020, more than 1.5 million people worldwide have now died of the disease. On top of this, the United States held its most highly divided and controversial election in history, causing many Americans to be further stressed by the added tension. Inspired by the rather crazy and intense events that have shaped this year, we have created a two-part package that helps to visualize both election data and Covid-19 data in the United States. The `USevolve` package simplifies the creation of chloropleth maps and line graphs using `ggplot2` to aid users looking to create specific visualizations with the latest coronavirus and US election data. As new data comes in, we hope this package will provide ongoing insights into both events.

![Corona Virus (Covid-19)](coronavirus.png)

![2020 Presidential Election](election20.png)

---

## Download & Install Instructions

To download this package use the following code.
```
# install dsproject package
if (!require(devtools)) {
  install.packages("devtools") 
}
devtools::install_github("bnorthrop/USevolve", build_vignettes = TRUE)
```

The source code is available [here](https://github.com/bnorthrop/USevolve).



