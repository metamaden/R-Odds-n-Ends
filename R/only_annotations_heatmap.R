# Heatmap with only annotations, example taken from ComplexHeatmap Bioconductor tutorials: 
# https://bioconductor.org/packages/release/bioc/vignettes/ComplexHeatmap/inst/doc/s4.heatmap_annotation.html
library(ComplexHeatmap)

ha = HeatmapAnnotation(df = data.frame(value = runif(10), type = rep(letters[1:2], 5)),
                       barplot = anno_barplot(runif(10)),
                       points = anno_points(runif(10)))
                       
zero_row_mat = matrix(nrow = 0, ncol = 10)
colnames(zero_row_mat) = letters[1:10]

Heatmap(zero_row_mat, top_annotation = ha, column_title = "only annotations")
