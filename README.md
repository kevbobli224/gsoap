# gsoap

A package for visualisation of gene set over-representation enrichment analysis.

## Features
<p align="justify">
Per dafault, <code>gsoap_layout</code> will calculate Jaccard distance between instances (e.g. pathways, GO terms, etc.), i.e. will measure relative overlaps between their query genes. Multidimensional scaling (other options include tSNE, CCA, Isomap) is then applied to project instances into 2-dimensional space. Circle packing is then applied to increase visual clarity of the layout. Significance of the instance is calculated as *-log10(pvalue)* and is used as instance weight to calculate instance closeness. Finally, cluster analysis is performed to identify clusters of instances.
</p>

Obtained layout is an R data.frame that contains the following columns:
  * x, y - coordinates of the circles representing pathways
  * radius - circle radius
  * size - number of pathway gene members (effect size)
  * significance - pathway significance defined as -log10(p.value)
  * closeness - pathway closeness
  * cluster - pathway cluster membership

<p align="justify">
Layout can be then visualized by <code>gsoap_plot</code>, that allows user to select columns to be used as color and alpha aesthetics, as well as the indices or names of the instances which should be annotated by labels.
</p>

## Installation
```S
require(devtools)
install_github("tomastokar/gsoap", dependencies=T)
```

## Usage

### Load GSOAP package
```S
library(gsoap)
```

### Load example dataset 
```S
data("pxgenes")
```
<p align="justify">
The example dataset contains results of the over-representation analysis of 72 differentially expressed genes from [Tokar et al. 2018]. The analysis was performed using Pathway Data Integration Portal (pathDIP) [Rahmati et al., 2016]. 
</p>

It is an R data.frame, whose rownames are pathway names and columns are:
  * Source - original source of the pathway
  * Pathway - pathway name
  * p.value - statistical significance of the obtained enrichment
  * FDR - false discovery rate of the obtained enrichment
  * Members - list of query genes belonging to the given pathway (in the following format: ``GENE1/GENE2/..'')
  
### Create GSOAP layout
```S
layout = gsoap_layout(pxgenes, 'Members', 'p.value')
```

### Create GSOAP plot
```S
# Order instances by their significance
layout = layout[order(layout$significance, decreasing = TRUE),]

# Create gsoap plot
gsoap_plot(layout, as.color = 'cluster', as.alpha = 'significance', which.label = 1:5)
```

![gsoap_example](https://user-images.githubusercontent.com/46754141/59162870-67bb5a00-8ac6-11e9-95b0-25e60fc3b97b.png)

## References
 * Tokar, Tomas, et al. "Differentially expressed microRNAs in lung adenocarcinoma invert effects of copy number aberrations of prognostic genes." Oncotarget 9.10 (2018): 9137.
 * Rahmati, Sara, et al. "pathDIP: an annotated resource for known and predicted human gene-pathway associations and pathway enrichment analysis." Nucleic acids research 45.D1 (2016): D419-D426.
