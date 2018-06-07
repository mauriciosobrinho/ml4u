require(tm)

process.directory <- function(from.dir, to.dir.train, to.dir.test, train.size=0.7) {
	
	files = system(paste("ls ", from.dir), intern=TRUE)

	size = length(files) * train.size
	train.set = sample(files, size=size)
	test.set = setdiff(files, train.set)

	# Copying training set
	for (i in 1:length(train.set)) {
		system(paste("cp ", from.dir,"/",train.set[i], " ", to.dir.train,"/", sep=""), intern=T)
	}

	# Copying test set
	for (i in 1:length(test.set)) {
		system(paste("cp ", from.dir,"/",test.set[i], " ", to.dir.test,"/", sep=""), intern=T)
	}
}

concat.files <- function(filename, directory) {

	files = system(paste("ls ", directory), intern=T)
	for (i in 1:length(files)) {
		system(paste("cat ",directory,"/",files[i]," >> ", filename, sep=""), intern=T)
	}
}

text.preprocessing <- function(filename) {

	tmpfile = tempfile()
	system(paste("iconv -f ISO-8859-1 -t UTF-8 ", filename," > ", tmpfile))
	system(paste("mv ", tmpfile, " ", filename))
	unlink(tmpfile)

	#fp <- file(filename, "rb", encoding = "UTF-8")
	#nchars = file.info(filename)$size
	#text = readChar(fp, nchars)
	text = readLines(fp <- file(filename, encoding = "UTF-8"))
	close(fp)

	text = tolower(text)
	text = removeWords(text, stopwords())
	text = stemDocument(text, language = "english")
	text = removePunctuation(text)
	text = removeNumbers(text)

	return (text)
}

text.save <- function(filename, text) {

	fp <- file(filename, "w")
     	#writeChar(text, fp, eos = NULL)
	writeLines(text, con = fp, sep = "\n")
     	close(fp)
}

train.prepare <- function(dataset.dir, destination.dir, train.size=0.7) {

	classes = system(paste("ls ", dataset.dir, sep=""), intern=TRUE)
	train.filenames = c()
	test.directories = c()

	readyDir = paste(destination.dir,"/ready", sep="")
	system(paste("mkdir -p ", readyDir, sep=""), intern=T)

	for (i in 1:length(classes)) {

		cat("Running for class ", classes[i], "\n")
		from.dir = paste(dataset.dir,"/",classes[i], sep="")
		to.dir.train = paste(destination.dir,"/train/", classes[i], sep="")
		to.dir.test = paste(destination.dir,"/test/", classes[i], sep="")

		system(paste("mkdir -p ", to.dir.train), intern=T)
		system(paste("mkdir -p ", to.dir.test), intern=T)
	
		test.directories = c(test.directories, to.dir.test)

		cat("\tProcessing directories for class ", classes[i], "\n")
		process.directory(from.dir, to.dir.train, to.dir.test, train.size)

		cat("\tConcatenating files for class ", classes[i], "\n")
		train.filename = paste(destination.dir,"/",classes[i],"-train.txt",sep="")
		concat.files(train.filename, to.dir.train)

		cat("\tProcessing text for class ", classes[i], "\n")
		text = text.preprocessing(train.filename)
		train.filename = paste(destination.dir,"/ready/",classes[i], sep="")

		cat("\tSaving text for class ", classes[i], "\n")
		text.save(train.filename, text)
		train.filenames = c(train.filenames, train.filename)
	}

	ret = list()
	ret$readyDir = readyDir
	ret$classes = classes
	ret$train.filenames = train.filenames
	ret$test.directories = test.directories

	return (ret)
}

naive.train <- function(dataset.dir="../datasets/20newsgroup/20news-18828", 
		  destination.dir="../datasets/20newsgroup", 
		  train.size=0.7) {

	cat("Removing the preprocessed directory...\n")
	system(paste("rm -rf ", destination.dir, "/processed", sep=""), intern=TRUE)

	cat("Processing directory...\n")
	prepared.object = train.prepare(dataset.dir, destination.dir, train.size)

	cat("Generating Corpus...\n")
     	corpus = VCorpus(DirSource(prepared.object$readyDir), 
		     readerControl = list(language = "en"))
	td.matrix = TermDocumentMatrix(corpus)

	cat("Computing matrices...\n")
	ret = list()
	ret$weightTf = weightTf(td.matrix)
	ret$weightTfIdf = weightTfIdf(td.matrix)

	return (ret)
}

document.test <- function(weight.matrix, query.document, epsilon=1e-7) {
	
	system(paste("rm -rf /tmp/aux"), intern=TRUE)
	system(paste("mkdir -p /tmp/aux"), intern=TRUE)
	query.text = text.preprocessing(query.document)
	text.save("/tmp/aux/filename", query.text)

     	file = VCorpus(DirSource("/tmp/aux"), 
		     readerControl = list(language = "en"))
	mat = TermDocumentMatrix(file)
	term.list = row.names(as.data.frame(as.matrix(mat)))

	classes = colnames(as.data.frame(as.matrix(weight.matrix)))
	P_cj = log(rep(1/length(classes), length(classes)))

	probs = rep(0, length(classes))
	# para cada classe
	frequencies = as.data.frame(as.matrix(weight.matrix))[term.list,]
	frequencies[is.na(frequencies)] = 0
	P_at = apply(frequencies, 1, function(x) { x / sum(x+epsilon) })

	ret = list()
	probs = log(rowSums(P_at)) + P_cj
	probs = exp(probs)
	probs = probs / sum(probs)

	ret$classes = classes
	ret$probs = probs
	ret$obtained.class = classes[sort.list(probs, dec=T)[1]]

	return (ret)
}

naive.test <- function(model, test.directories, matrix.type="tf", epsilon=1e-7) {

	right = 0
	wrong = 0

	weight.matrix = model$weightTf

	if (matrix.type == "tfidf") {
		weight.matrix = model$weightTfIdf
	}

	classes = system(paste("ls ", test.directories, sep=""), intern=TRUE)

	for (cj in classes) {
		cat("Running for class: ", cj, "\n")
		files = system(paste("ls ", test.directories,"/",cj, sep=""), intern=TRUE)

		for (doc in files) {
			filename = paste(test.directories,"/",cj,"/",doc, sep="")
			ret = document.test(weight.matrix, filename, epsilon)

			if (ret$obtained.class == cj) {
				right = right + 1
			} else {
				wrong = wrong + 1
			}

			accuracy = right / (right + wrong)
			cat("Partial accuracy: ", filename, ": ",accuracy, "\n")
		}
	}

	accuracy = right / (right + wrong)

	return (accuracy)
}














