# SVG generation example

# Load GSOAP package and respective packages
library(gsoap)
library(ggforce)
library(ggrepel)
library("ggiraph")
library(htmlwidgets)

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

# Produce interactive html svg
w <- ggiraph(ggobj = p, width_svg = 7, height_svg = 7)

# Create an output directory, if exists warning will be hidden
dir.create(file.path(getwd(), "output"), showWarnings = FALSE)

# Save the html widget
saveWidget(w, file="./output/widgets.html", selfcontained = TRUE, libdir = NULL)

# htmlwidgets has a persistent issue(>1y.o) where dependencies created as a
# directory will not be deleted afterwards when selfcontained = TRUE
unlink(file.path(getwd(), "output/widgets_files"), recursive = TRUE)



# To view in RStudio's viewer:
w

# To save as pure svg
dsvg(file="./output/widgets.svg")
plot(p)
dev.off()
