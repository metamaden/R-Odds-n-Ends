library(ComplexHeatmap)
library(circlize)

# Take normally dist data as heatmap matrix
hm_data <- rnorm(200); qdat <- quantile(colMeans(hm_matrix))
hm_matrix <- matrix(hm_data,ncol=10)
canno_var1 <- colMeans(hm_matrix); 
canno_var1 <- ifelse(canno_var1<=qdat[3],"low","high")
canno_var2 <- apply(hm_matrix,1,sd); sds <- quantile(canno_var2)
canno_var2<-ifelse(canno_var2<=sds[3],"low","high")
canno_df <- data.frame(samp_means=canno_var1,samp_sd=canno_var2)

# hm col annotations can contain complex layered output and colored annotaions
hm_colanno <- HeatmapAnnotation(boxplot=anno_boxplot(hm_matrix,axis=TRUE),
                                points = anno_points(hm_matrix, axis = TRUE),
                                show_legend = TRUE,
                                df = data.frame(samp_sd=canno_var2,
                                                samp_means=canno_var1), 
                                col = list(samp_means = c("low" =  "blue","high" = "pink"),
                                           samp_sd = c("low"="gray","high"="black")),
                                name = "Sample Type",
                                annotation_height = unit(c(0.5, 0.5, 1.5, 1), "cm"))

# row annotations added manually with draw, not in Heatmap()
hm_rowanno = rowAnnotation(df = data.frame(type2 = c(rep("A", 10), rep("B", 30))), 
                       col = list(type2 = c("A" =  "red", "B" = "yellow")), 
                       width = unit(1, "cm"))

breaks=seq(min(hm_data),max(hm_data),0.5)
hmcol = colorRamp2(breaks,colorRampPalette(c("purple","orange","gray"))(n=length(breaks)))

# heatmap 1
hm_no_rowanno <- Heatmap(hm_matrix,col=hmcol, km=3,
        cluster_columns = TRUE,
        show_heatmap_legend = TRUE,
        top_annotation = hm_colanno,
        name="Rnorm_val",
        show_row_names = TRUE,
        show_column_names = TRUE,
        column_title = "HM1 COLS", 
        column_dend_reorder = TRUE,
        row_dend_reorder = TRUE,
        heatmap_legend_param = list(color_bar = "continuous"),
        row_title = "HM2 ROWS")

# heatmap 2, add blankspaces for missing/na data
hm_data <- rnorm(200); qdat <- quantile(colMeans(hm_matrix))
hm_matrix <- matrix(hm_data,ncol=10)
canno_var1 <- colMeans(hm_matrix); 
canno_var1 <- ifelse(canno_var1<=qdat[3],"low","high")
canno_var2 <- apply(hm_matrix,1,sd); sds <- quantile(canno_var2)
canno_var2<-ifelse(canno_var2<=sds[3],"low","high")
canno_df <- data.frame(samp_means=canno_var1,samp_sd=canno_var2)
hm_colanno <- HeatmapAnnotation(boxplot=anno_boxplot(hm_matrix,axis=TRUE),
                                points = anno_points(hm_matrix, axis = TRUE),
                                show_legend = FALSE,
                                df = data.frame(samp_null=rep("null",length(canno_var2)),
                                                samp_means=canno_var1), 
                                col = list(samp_means = c("low" =  "blue","high" = "pink"),
                                           samp_null = c("null"="white")),
                                name = "Sample Type",
                                annotation_height = unit(c(0.5, 0.5, 1.5, 1), "cm"))
# mask legend to avoid redundancies
# only 1 hm need be split on clusters with "km", then both hm and row annos are split with draw()
hm_no_rowanno2 <- Heatmap(hm_matrix,col=hmcol,km=3,
                         cluster_columns = TRUE,
                         show_heatmap_legend = FALSE,
                         top_annotation = hm_colanno,
                         name="Rnorm_val2",
                         show_row_names = TRUE,
                         show_column_names = TRUE,
                         column_title = "HM2 COLS", 
                         column_dend_reorder = TRUE,
                         row_dend_reorder = TRUE,
                         heatmap_legend_param = list(color_bar = "continuous"),
                         row_title = "HM2 ROWS")

hmlist <- hm_no_rowanno+hm_no_rowanno2+ hm_rowanno
draw(hmlist,row_title="Rnorm rows",column_title="Rnorm columns",heatmap_legend_side="top",
     annotation_legend_side="left")
