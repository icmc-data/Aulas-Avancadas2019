
dwnn <- function(D, query, sigma) {
	classId = ncol(D)
	X = matrix(D[,1:(classId-1)], ncol=classId-1)
	Y = D[,classId]
	E = apply(X, 1, function(row) { 
		      sqrt(sum((row - query)^2))})
	A = exp(-E^2 / (2*sigma^2))
	return (A %*% Y / sum(A))
}

test <- function(sigma) {
	D = cbind(-5:5, -5:5)
	R = NULL
	for (query in seq(-5,5,len=100)) {
		y = dwnn(D, query, sigma)
		R = rbind(R, c(query, y))
	}
	plot(D)
	points(R, col=2)
}
