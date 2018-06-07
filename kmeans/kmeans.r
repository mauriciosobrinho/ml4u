
my.kmeans <- function(dataset, k=2, threshold=1e-3) {

	if (is.vector(dataset)) {
		dataset = as.data.frame(matrix(dataset, ncol=1))
	}

	ids = sample(1:nrow(dataset), size=k)
	centroid = as.data.frame(dataset[ids,])
	div = 2 * threshold

	while (div > threshold) {
		# Para cada centroide
		dists = NULL
		for (i in 1:k) {
			# calcular a distancia euclidiana de cada ponto do
			# espaço em relação a cada centroide
			euclidean = apply(dataset, 1, 
					  function(row) {
						  sqrt(sum((row - centroid[i,])^2))})
			dists = cbind(dists, euclidean)
		}

		# Descobrir qual centroide cada ponto está associado
		associated.centroids = apply(dists, 1, function(row) { which.min(row) } )

		# Atualizar a posição dos centroides
		div = 0
		for (i in 1:k) {
			ids = which(associated.centroids == i)
			new.position = colMeans(as.data.frame(dataset[ids,]))
			div = div + sqrt(sum((centroid[i,] - new.position)^2))
			centroid[i,] = new.position
		}
		div = div / k

		cat("Divergence: ", div, "\n")
	}


	ret = list()
	ret$k = k
	ret$centroid = centroid
	ret$clusters = associated.centroids

	return (ret)
}
