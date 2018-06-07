
# Caixeiro Viajante
# Travelling Salesperson Problem

# ncities = 5
# 1, 2, 3, 4, 5

# _2_ _5_ _4_ _1_ _3_

# Distances
#
#    1 2 3 4 5
# 1  0 x
# 2  y 0
# 3      0
# 4        0
# 5          0
#

fitness <- function(subject, distances) {

	sum = 0
	for (i in 1:(length(subject)-1)) {
		sum = sum + distances[subject[i], subject[i+1]]
	}
	sum = sum + distances[subject[length(subject)], subject[1]]
	return (1/sum)
}

second.model <- function(distances, k=5, popsize=100, ngenerations=100) {

	output = NULL
	ncities = nrow(distances)
	population = NULL
	fitness = rep(0, popsize)
	for (i in 1:popsize) {
		subject = sample(1:ncities)
		population = rbind(population, subject)
		fitness[i] = fitness(subject, distances)
	}

	for (i in 1:ngenerations) {
		children = NULL
		children.fitness = rep(0, k)
		for (j in 1:k) {
			parentId = sample(1:popsize, size=1)
			new = population[parentId,]
			# 1 4 3 2 5
			selected = sample(1:ncities, size=2)
			aux = new[selected[1]]
			new[selected[1]] = new[selected[2]]
			new[selected[2]] = aux
			children = rbind(children, new)
			children.fitness[j] = fitness(new, distances)
		}

		for (j in 1:k) {
			id = sample(1:popsize, size=1)
			if (fitness[id] < children.fitness[j]) {
				population[id,] = children[j,]
				fitness[id] = children.fitness[j]
			}
		}

		output = rbind(output, cbind(i, mean(fitness), sd(fitness)))
	}

	return (output)
}




