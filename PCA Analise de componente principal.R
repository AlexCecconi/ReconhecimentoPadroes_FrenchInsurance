#install.packages(c('ggplot2', 'ggfortify', 'gridExtra','carData','car','factoextra','corrplot','readxl'))
#Referencia -- https://rpubs.com/KarolinaSzczesna/862710

library("readxl")
library("ggplot2")
library("ggfortify")
library("gridExtra")
library("carData")
library("car")
library("factoextra")
library("corrplot")


#Carregando os dados
decathlon2 <- read_excel('C:\\Users\\Alex\\Downloads\\df.xlsx')

decathlon2 <- decathlon2[sample(nrow(decathlon2), size=100), ]

summary(decathlon2)


#Methodology of PCA - Visualization and Interpretation
res.pca <- prcomp(decathlon2, scale = TRUE)
print(res.pca)

summary(res.pca)

eig.val<-get_eigenvalue(res.pca)
eig.val

fviz_eig(res.pca, col.var="blue")


#PCA results for variables

var <- get_pca_var(res.pca)
var

head(var$cos2)

corrplot(var$cos2, is.corr=FALSE)


fviz_cos2(res.pca, choice = "var", axes = 1:2)


fviz_pca_var(res.pca,
             col.var = "cos2", # Color by the quality of representation
             gradient.cols = c("darkorchid4", "gold", "darkorange"),
             repel = TRUE
)


# Contributions of variables to PC1
a<-fviz_contrib(res.pca, choice = "var", axes = 1)
# Contributions of variables to PC2
b<-fviz_contrib(res.pca, choice = "var", axes = 2)
grid.arrange(a,b, ncol=2, top='Contribution of the variables to the first two PCs')


#PCA results for individuals
ind <- get_pca_ind(res.pca)
ind

#Embaralhar os dados e rodar com amostra pequena esta parte de baixo
fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("darkorchid4", "gold", "darkorange"),
             repel = TRUE
)



# Total contribution on PC1 and PC2
fviz_contrib(res.pca, choice = "ind", axes = 1:2)


autoplot(res.pca, loadings=TRUE, loadings.colour='darkorchid4', loadings.label=TRUE, loadings.label.size=3)


#Final results and analysis
kmeans<-eclust(decathlon2, k=4)

autoplot(res.pca, data=kmeans, colour="cluster")




