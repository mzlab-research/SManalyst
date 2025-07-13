runxy<-function(data,sample="sample",pc=30,vg=3000,binsize=1){
data %<>% arrange(y) %>% arrange(x)
data$x <- trunc(data$x / binsize) * binsize
data$y <- trunc(data$y / binsize) * binsize
data$intensity <- as.numeric(data$intensity)  # 将整数类型转换为数值类型
setDT(data)
data <- data[, .(counts = sum(intensity)), by = .(mz, x, y)]
#' create sparse matrix from stereo
data$cell <- paste0(sample, ':', data$x, '_', data$y)
data$mzx <- match(data$mz, unique(data$mz))
data$cellIdx <- match(data$cell, unique(data$cell))
mat <- sparseMatrix(i = data$mzx, j = data$cellIdx, x = data$counts,
                    dimnames = list(unique(data$mz), unique(data$cell)))
#sparseMatrix稀疏矩阵函数
cell_coords <- unique(data[, c('cell', 'x', 'y')])
#unique去除重复函数，删除cell,x和y都一样的行
rownames(cell_coords) <- cell_coords$cell

seurat_spatialObj <- CreateSeuratObject(counts = mat, project = 'Stereo', assay = 'Spatial',
                                        names.delim = ':', meta.data = cell_coords)
#' create pseudo image
cell_coords$x <- cell_coords$x - min(cell_coords$x) + 1
cell_coords$y <- cell_coords$y - min(cell_coords$y) + 1

tissue_lowres_image <- matrix(1, max(cell_coords$y), max(cell_coords$x))
#matrix(aa,x,y)以aa为输入向量，创建一个x行y列的矩阵
#构造一个seruat image

tissue_positions_list <- data.frame(row.names = cell_coords$cell,
                                    tissue = 1,
                                    row = cell_coords$y, col = cell_coords$x,
                                    imagerow = cell_coords$y, imagecol = cell_coords$x)


scalefactors_json <- toJSON(list(fiducial_diameter_fullres = binsize,
                                 tissue_hires_scalef = 1,
                                 tissue_lowres_scalef = 1))
#toJSON: 把json格式 转换成 list格式


#' function to create image object
generate_spatialObj <- function(image, scale.factors, tissue.positions, filter.matrix = TRUE){
  if (filter.matrix) {
    tissue.positions <- tissue.positions[which(tissue.positions$tissue == 1), , drop = FALSE]
  }
  
  unnormalized.radius <- scale.factors$fiducial_diameter_fullres * scale.factors$tissue_lowres_scalef
  
  spot.radius <- unnormalized.radius / max(dim(x = image))
  
  return(new(Class = 'VisiumV1',
             image = image,
             scale.factors = scalefactors(spot = scale.factors$tissue_hires_scalef,
                                          fiducial = scale.factors$fiducial_diameter_fullres,
                                          hires = scale.factors$tissue_hires_scalef,
                                          lowres = scale.factors$tissue_lowres_scalef),
             coordinates = tissue.positions,
             spot.radius = spot.radius))
}

spatialObj <- generate_spatialObj(image = tissue_lowres_image,
                                  scale.factors = fromJSON(scalefactors_json),
                                  tissue.positions = tissue_positions_list)
#' import image into seurat object
spatialObj <- spatialObj[Cells(x = seurat_spatialObj)]
DefaultAssay(spatialObj) <- 'Spatial'

seurat_spatialObj[['slice1']] <- spatialObj

#' filter out empty cell
seurat_spatialObj <- subset(seurat_spatialObj, subset = nCount_Spatial > 0)

obj <- seurat_spatialObj
maxCount <- max(obj$nCount_Spatial)
maxFeature <- max(obj$nFeature_Spatial)
obj <- subset(obj, subset = nCount_Spatial >= 0 & nCount_Spatial <= maxCount &
                nFeature_Spatial >= 0 & nFeature_Spatial <= maxFeature)

deal_measure_data_rds <- SCTransform(obj, method = "glmGamPoi",assay = 'Spatial', verbose = FALSE, variable.features.n = as.numeric(vg),
                                     return.only.var.genes = FALSE, n_genes=NULL, min_cells=5)
return(deal_measure_data_rds)
}