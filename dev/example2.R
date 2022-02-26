# SVG generation example

# Load GSOAP package
library(gsoap)
library(ggforce)
library(ggrepel)
library("ggiraph")

# Load example dataset
data(pxgenes)

# Reduce to top 100 instances
pxgenes <-  head(pxgenes[order(pxgenes$FDR),], 100)

# Create GSOAP layout
layout <-  gsoap_layout(pxgenes, 'Members', 'p.value')

# Order instances by their significance
layout <-  layout[order(layout$significance, decreasing = TRUE),]
rownames(layout) <- seq(1:nrow(layout))

# Create svg with layout as parameters
p <- gsoap_svg(layout, as.color = 'cluster', as.alpha = 'significance', which.labels = c("a6b1 and a6b4 Integrin signaling","Cardiac_Hypertrophy"))
p <- gsoap_svg(layout, as.color = 'cluster', as.alpha = 'significance', which.labels = 1:10)

# Produce html svg
girafe(ggobj = p)

# Saving svg to path
dsvg(file = "./dev/svg/test.svg")
plot(p)
dev.off()
