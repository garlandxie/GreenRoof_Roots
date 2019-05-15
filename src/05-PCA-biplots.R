# libraries --------------------------------------------------------------------
library(here)
library(factoextra)

# import -----------------------------------------------------------------------

pca_matrix_WD <- readRDS(here::here("data/project_data/final", 
                              "pca_matrix_WD.rds"))

pca_matrix_WW <- readRDS(here::here("data/project_data/final", 
                                    "pca_matrix_WW.rds"))


# plot - pca WD ----------------------------------------------------------------
  
row.names(pca_matrix_WD$rotation) <- c("RMF", "SRL", "RLD", "MRD", "MD", "PS")
pca_WD <- fviz_pca_biplot(pca_matrix_WD, 
                repel = TRUE, 
                label = "var",
                col.ind = "red", 
                col.var = "black", 
                alpha.ind = 0.3,
                geom.var = c("point", "text"), 
                title = "")

# plot - pca WW ----------------------------------------------------------------

row.names(pca_matrix_WW$rotation) <- c("RMF", "SRL", "RLD", "MRD", "MD", "PS")
pca_WW <- fviz_pca_biplot(pca_matrix_WW, 
                repel = TRUE, 
                label = "var",
                col.ind = "blue", 
                col.var = "black", 
                alpha.ind = 0.3,
                geom.var = c("point", "text"), 
                title = "")

# multipanel figure ------------------------------------------------------------

ggsave(plot = pca_WW, 
       here::here("output/figures", "pca_WW.png"),
       height = 3.48, width = 3.32, 
       device = "png")

ggsave(plot = pca_WD, 
       here::here("output/figures", "pca_WD.png"),
       height = 3.48, width = 3.32, 
       device = "png")

