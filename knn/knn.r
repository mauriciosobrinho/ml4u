require(rgl)
require(tseriesChaos)

# distance-weighted nearest neighbors

dwnn <- function(knowledge.base, query, sigma=0.5) {

	classId = ncol(knowledge.base)
	X = knowledge.base[,1:(classId-1)]
	Y = knowledge.base[,classId]

	euclidean = apply(X, 1, function(row) { sqrt(sum((row - query)^2)) } )
	activation = exp(-euclidean / (2*sigma^2))

	y = sum(activation * Y) / sum(activation)

	return (y)
}

test <- function(query, sigma) {
	dataset = cbind(rnorm(mean=0, sd=1, n=10), rnorm(mean=0, sd=1, n=10), 1)
	dataset = rbind(dataset, 
		cbind(rnorm(mean=10, sd=1, n=10), rnorm(mean=10, sd=1, n=10), -1))

	plot(dataset[,1:2], col=dataset[,3]+2)

	return (dwnn(dataset, query, sigma))
}

sin.predict <- function(sigma, m=2, d=1, train.size=0.75) {
	
	time = seq(0,9,len=1000)
	series = sin(2*pi*time)

	#plot(series)

	# Takens' embedding theorem 1981
	dataset = embedd(series, m, d)
	#plot3d(dataset)

	# m=3, d=1
	# x(t-d), x(t), x(t+d)
	train = floor(nrow(dataset) * train.size)
	base = dataset[1:train,]

	sqerror = 0
	counter = 0
	for (i in (train+1):nrow(dataset)) {
		query = dataset[i,1:2]
		expected = dataset[i,3]

		obtained = dwnn(base, query, sigma)

		error = (expected - obtained)^2
		sqerror = sqerror + error

		counter = counter + 1
	}

	return (sqerror/counter)
}

test.sigmas.sin <- function() {

	data = NULL
	for (sigma in seq(1e-15, 5, length=100)) {
		sqerror = sin.predict(sigma, m=3, d=1, train.size=0.8)
		data = rbind(data, c(sigma, sqerror))
	}

	plot(data)
}












