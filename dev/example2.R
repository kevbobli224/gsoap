# SVG generation example

# Load GSOAP package and respective packages
library(gsoap)
library(ggforce)
library(ggrepel)
library(ggiraph)
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
p <- gsoap_svg(layout, as.color = 'cluster', as.alpha = 'significance')

# If needed to generate labels on svg:
#p <- gsoap_svg(layout, as.color = 'cluster', as.alpha = 'significance', which.labels = 1:10)

# Produce interactive html svg
w <- ggiraph(ggobj = p)
w <- girafe_options(w, opts_toolbar(position = "topleft"), opts_zoom(min = 0.2, max = 5))

# To view in RStudio's viewer:
w

# Create an output directory, if exists warning will be hidden
dir.create(file.path(getwd(), "output"), showWarnings = FALSE)

# Save the html widget
saveWidget(w, file="./output/widgets.html", selfcontained = TRUE, libdir = NULL)

# htmlwidgets has a persistent issue(>1y.o) where dependencies created as a
# directory will not be deleted afterwards when selfcontained = TRUE
unlink(file.path(getwd(), "output/widgets_files"), recursive = TRUE)




# To save as pure svg, uncomment
# dsvg(file="./output/widgets.svg")
# plot(p)
# dev.off()

# This attaches necessary javascript dependencies and our own script
library("xml2")
h <- read_html("./output/widgets.html")

# Attaches Jquary dependencies
jQ <- read_xml('<script src="https://code.jquery.com/jquery-3.6.0.min.js"></script>')
jQUI <- read_xml('<script src="https://code.jquery.com/ui/1.13.0/jquery-ui.js"></script>')
jQss <- read_xml('<link href="https://code.jquery.com/ui/1.13.0/themes/base/jquery-ui.css" type="text/css" rel="stylesheet"></link>')

xml_add_child(xml_children(h)[1], jQ)
xml_add_child(xml_children(h)[1], jQUI)
xml_add_child(xml_children(h)[1], jQss)

# Attaches our own interactive js script
tag <- paste0("<script></script>")
post_proc <- read_xml(tag)
xml_text(post_proc) <- as.character(readLines("./output/postproc_min.js"))

xml_add_child(xml_children(h)[1], post_proc)

# Write to file
write_html(h, "./output/widgets_modified.html")
