# M?ster -> Detecci?n de anomal?as
# Juan Carlos Cubero. Universidad de Granada

###########################################################################
# MULTIVARIATE STATISTICAL OUTLIERS. CLUSTERING OUTLIERS 
###########################################################################

# Los outliers son respecto a un conjunto de variables.


#####################################################################
# Lectura de valores y Preprocesamiento
#####################################################################

# Trabajamos sobre las columnas num?ricas de iris [1:4]
# Este conjunto de datos est? disponible en R
# Tanto LOF como clustering usan distancias entre registros, por lo que habr?
# que trabajar sobre los datos previamente normalizados

# Construimos los siguiente conjuntos:

# mis.datos.numericos -> con las columnas 1:4 de iris
# mis.datos.numericos.normalizados -> con los valores normalizados
# a Los rownames de mis.datos.numericos.normalizados les asignamos los rownames de mis.datos.numericos

# Establecemos la variable numero.de.outliers a 5 y numero.de.clusters a 3


mis.datos.numericos   = iris[,1:4]
#mis.datos.numericos   = mis.datos.originales[,sapply(mis.datos.originales, is.numeric)]
mis.datos.numericos.normalizados           = scale(mis.datos.numericos)
rownames(mis.datos.numericos.normalizados) = rownames(mis.datos.numericos)

numero.de.outliers   = 5
numero.de.clusters   = 3

set.seed(2)  # Para establecer la semilla para la primera iteraci?n de kmeans


###########################################################################
# C?mputo de los outliers seg?n la distancia eucl?dea de cada dato 
# al centroide de su cluster
# El centroide podr? ser cualquiera (podr? provenir de un k-means 
# o ser un medoide, por ejemplo)
###########################################################################



###########################################################################
# k-Means

# Construimos el modelo kmeans (modelo.kmeans) con los datos normalizados. 
# Para ello, usamos la funci?n de R llamada "kmeans"

# A partir del resultado de kmeans, accedemos a:

# a) $cluster para obtener 
#   los ?ndices de asignaci?n de cada dato al cluster correspondiente 
#   El resultado lo guardamos en la variable indices.clustering.iris
#   Por ejemplo, si el dato con ?ndice 69 est? asignado al tercer cluster,
#   en el vector indices.clustering.iris habr? un 3 en la componente n?mero 69

# b) $centers para obtener los datos de los centroides.
#   Los datos est?n normalizados por lo que los centroides tambi?n lo est?n.
#   El resultado lo guardamos en la variable centroides.normalizados.iris


# indices.clustering.iris
# 1   2   3   4   ... 69  70  71 ...
# 1   1   1   1   ... 3   3   2  ...

# centroides.normalizados.iris
#    Sepal.Length Sepal.Width Petal.Length Petal.Width
# 1  -1.01119138  0.85041372   -1.3006301  -1.2507035
# 2   1.13217737  0.08812645    0.9928284   1.0141287
# 3  -0.05005221 -0.88042696    0.3465767   0.2805873



# COMPLETAR
modelo <- kmeans(mis.datos.numericos.normalizados, centers = numero.de.clusters )
modelo$cluster
modelo$centers

# -------------------------------------------------------------------------

# Calculamos la distancia eucl?dea de cada dato a su centroide (con los valores normalizados)
# Para ello, usad la siguiente funci?n:

distancias_a_centroides = function (datos.normalizados, 
                                    indices.asignacion.clustering, 
                                    datos.centroides.normalizados){
  
  sqrt(rowSums(   (datos.normalizados - datos.centroides.normalizados[indices.asignacion.clustering,])^2   ))
}

# dist.centroides.iris
# 1          2          3             ......
# 0.21224719 0.99271979 0.64980753    ......

# Ordenamos dichas distancias a trav?s de la funci?n order y obtenemos
# los ?ndices correspondientes. Nos quedamos con los primeros
# (tantos como diga la variable numero.de.outliers)

# top.outliers.iris
# [1]  42  16 132 118  61



# COMPLETAR
distancias <- distancias_a_centroides(mis.datos.numericos.normalizados, modelo$cluster, modelo$centers)
distancias
top.outlier.iris <- order(distancias, decreasing = TRUE)[0:numero.de.outliers]
top.outlier.iris

