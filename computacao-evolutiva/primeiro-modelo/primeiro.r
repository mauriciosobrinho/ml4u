

# escalonamento de processos
#
# 10 processos
# k núcleos
#
# Representar/Codificar cada indivíduo:
#
# 1 1 3 2 5 7 3 3 3 k

fitness <- function(subject, capacity, cost=1000) {
	# MIPS
	# 1000 800 700
	# 25 processos
	# 10   8   7

	v = rep(0, length(capacity))

	for (i in 1:length(subject)) {
		coreId = subject[i]
		v[coreId] = v[coreId] + cost / capacity[coreId]
	}

	return (1 / max(v))
}

modelo1 <- function(capacity, nprocess=10, popsize=100, ngenerations=1000) {

	ncores = length(capacity)

	# Gerar M indivíduos para compor a minha populacao
	population = NULL
	for (i in 1:popsize) {
		element = rep(0, nprocess)
		for (j in 1:nprocess) {
			element[j] = sample(1:ncores, size=1)
		}
		population = rbind(population, element)
	}

	output = NULL

	for (generationId in 1:ngenerations) {
		parentId = sample(1:popsize, size=1)
		# Clonagem
		new = population[parentId,]
		# Trazendo alguma nova informação para o/a filho/a
		id = sample(1:nprocess, size=1)
		new[id] = sample(1:ncores, size=1)
		# Selecionar um elemento aleatório da população para competição
		selectedId = sample(1:popsize, size=1)

		if (fitness(new, capacity) > 
				fitness(population[selectedId,], capacity)) {
			population[selectedId,] = new
		}

		# Calcular o fitness médio da população
		# Calcular o desvio padrão do fitness da população
		fit = rep(0, nrow(population))

		for (i in 1:nrow(population)) {
			fit[i] = fitness(population[i,], capacity)
		}

		output = rbind(output, c(generationId, mean(fit), sd(fit)))
	}

	return (output)
}
