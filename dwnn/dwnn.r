
weight <- function(euclidean.dist, sigma) {
	return (exp(-euclidean.dist^2 / (2*sigma^2)))
}

dwnn <- function(knowledge.base, query, sigma=0.5) {

	classId = ncol(knowledge.base)

	euclidean.dist = 
	  apply(knowledge.base, 1, function(row) { 
			sqrt(sum((row[,1:(classId-1)] - query)^2)) })

  	w = weight(euclidean.dist)
	Y = knowledge.base[,classId]

	y = sum(w * Y) / sum(w)

	return (y)
}
