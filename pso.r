newParticle = function(pos, vel, fitFun) {
	return (list(
		pos = pos,
		vel = vel,
		best = list(
			pos = pos,
			val = fitFun(pos)
		)
	))
}

calculateBest = function(particles) {
	best = particles[[1]]$best
	for(i in 1:length(particles)){
		if(particles[[i]]$best$val < best$val){
			best = particles[[i]]$best
		}
	}
	return(best)
}

updatePos = function(particle){
	personal = config$c1 * runif(1) * (particle$best$pos - particle$pos)
	social = config$c2 * runif(1) * (gbest$pos - particle$pos)
	
	particle$vel = (config$inertia * particle$vel) + personal + social	
	particle$vel = apply(as.matrix(particle$vel), 1, function(x){min(x,config$max_vel)})
	particle$vel = apply(as.matrix(particle$vel), 1, function(x){max(x,-config$max_vel)})
	
	particle$pos = particle$pos + particle$vel	
	particle$pos = apply(as.matrix(particle$pos), 1, function(x){min(x,config$upper)})
	particle$pos = apply(as.matrix(particle$pos), 1, function(x){max(x,config$lower)})
	
	val = config$fun(particle$pos);
	if(val < particle$best$val){
		particle$best$pos = particle$pos
		particle$best$val = val
	}
	return (particle)
}

push = function(arr, element){
	len = length(arr)
	arr[[len + 1]] = element
	return(arr)
}

pso = function(config){
	particles = list()
	iteration = 0
	
	for(i in 1:config$swarm_size){
		pos = runif(config$dim) * (config$upper - config$lower) + config$lower
		vel = rep(0, config$dim)
		particles = push(particles, newParticle(pos, vel, config$fun))
	}
	
	gbest = calculateBest(particles);
	
	cost = c()
	for(it in 1:config$iterations){
		pos = c()		
		for(i in 1:length(particles)){
			particles[[i]] = updatePos(particles[[i]])
			gbest = calculateBest(particles);
			pos = rbind(pos, cbind(particles[[i]]$pos[1], particles[[i]]$pos[2]))
			cost = c(cost,gbest$val)
		}
		# plot das particulas
		# plot(pos, ylim=c(config$lower,config$upper), xlim=c(config$lower,config$upper))
	}
	plot(cost, type = "s")
	return (gbest)
}


config = c()
config$dim = 2
config$upper = 100
config$lower = -100
#config$fun = function(x){return(sum(x ^ 2))}
config$fun = function(x){
	return(20 + (x[1] ^ 2) - cos(10 * pi * x[1]) + (x[2]^2) + (10*cos(2 * pi * x[2])))
}
config$swarm_size = 20
config$c1 = 2.05
config$c2 = 2.05
config$max_vel = 2
config$inertia = 0.9
config$iterations = 50

pso(config)