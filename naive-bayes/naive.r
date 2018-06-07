
naive <- function(dataset, query) {
	colId = ncol(dataset)
	classes = unique(dataset[,colId])

	P = rep(0, length(classes))
	for (i in 1:length(classes)) {
		P_classe_i = sum(dataset[,colId] == classes[i]) / nrow(dataset)
		prod = 1
		for (j in 1:length(query)) {
			if (query[j] != "?") {
				P_aj_given_classe_i = sum(dataset[,j] == query[j] & dataset[,colId] == classes[i])/sum(dataset[,colId] == classes[i])
				prod = prod * P_aj_given_classe_i
			}
		}
		prod = prod * P_classe_i
		P[i] = prod
	}
	ret = list()
	ids = sort.list(classes)
	ret$classes = classes[ids]
	ret$prob = P[ids] / sum(P[ids])

	return (ret)
}

dataset = as.matrix(read.table("tenis.dat", header=T))
r = naive(dataset, dataset[1,])

