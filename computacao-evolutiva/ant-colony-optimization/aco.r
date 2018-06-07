
mysample <- function(prob) {
	bar = cumsum(prob)
	value = runif(min=0, max=1, n=1)
	idx = which(value <= bar)[1]
	return (idx)
}

aco <- function(adj.matrix, ants=100, iterations=1000, deltaP=0.1,
			nestId=1, foodId=6, epsilon=1e-7, alpha=0.9) {

	ants.position = rep(nestId, ants)
	ants.direction = rep(0, ants)

	pheromone = matrix(epsilon,nrow=nrow(adj.matrix),ncol=ncol(adj.matrix))

	for (i in 1:iterations) {
		cat("Iteration: ", i, "\n")

		# Walking
		pheromone.aux = 
		    matrix(0,nrow=nrow(adj.matrix),ncol=ncol(adj.matrix))

		for (j in 1:ants) {
			position = ants.position[j]

			# Saindo do ninho em direção ao alimento
			if (ants.direction[j] == 0) {
				dists = adj.matrix[position, 
						(position+1):ncol(adj.matrix)]
				pher = pheromone[position, 
						(position+1):ncol(adj.matrix)]
				prob = pher / dists
				prob = as.vector(ts(prob / sum(prob)))
				nextId = position + mysample(prob)
			} else { # Voltando pro ninho
				dists = adj.matrix[position, 1:(position-1)]
				pher = pheromone[position, 1:(position-1)]
				prob = pher / dists
				prob = as.vector(ts(prob / sum(prob)))
				nextId = mysample(prob)
			}

			pheromone.aux[ants.position[j], nextId] =
				pheromone.aux[ants.position[j], nextId] + 
					deltaP

			pheromone.aux[nextId, ants.position[j]] =
				pheromone.aux[nextId, ants.position[j]] + 
					deltaP
			ants.position[j] = nextId

			if (ants.position[j]==nestId) {ants.direction[j] = 0}
			if (ants.position[j]==foodId) {ants.direction[j] = 1}
		}

		# Update pheromone
		pheromone = alpha * pheromone + pheromone.aux
	}

	ret = list()
	ret$adj.matrix = adj.matrix
	ret$pheromone = pheromone

	return (ret)
}
