
cost <- function(x) {
	return (x^2)
}

pso <- function(numberOfParticles=10, iterations=100, omega=0.9, 
			phip=0.7, phig=0.3, lower=-100, upper=100) {

	particles = rep(0, numberOfParticles)
	velocity = rep(0, numberOfParticles)
	costlocalbest = rep(0, numberOfParticles)
	localbest = rep(0, numberOfParticles)
	globalbest = 0

	# Inicialização de posição das partículas
	for (i in 1:numberOfParticles) {
		particles[i] = runif(min=lower, max=upper, n=1)
		localbest[i] = particles[i]
		costlocalbest[i] = cost(particles[i])
	}

	# Inicialização da solução global
	id = which.min(costlocalbest)
	globalbest = particles[id]

	# Inicialização das velocidades das partículas
	for (i in 1:numberOfParticles) {
		velocity[i] = 
			runif(min=-abs(upper-lower), max=abs(upper-lower), n=1)
	}

	# Partículas devem se mover
	for (i in 1:iterations) {

		# Calcular a velocidade seguinte
		# Alterar a posição das partículas
		for (j in 1:numberOfParticles) {
			rp = runif(min=0, max=1, n=1)
			rg = runif(min=0, max=1, n=1)
			velocity[j] = omega * velocity[j] +
				phip * rp * (localbest[j]-particles[j]) +
				phig * rg * (globalbest-particles[j])
			particles[j] = particles[j] + velocity[j]

			# se a posicao atual for melhor
			if (cost(particles[j]) < costlocalbest[j]) {
				localbest[j] = particles[j]
				costlocalbest[j] = cost(particles[j])
			}
		}

		# Verificar se a solução global alterou
		id = which.min(costlocalbest)
		if (particles[id] < cost(globalbest)) {
			globalbest = particles[id]
		}
	}

	ret = list()
	ret$globalbest = globalbest

	return (ret)

}
