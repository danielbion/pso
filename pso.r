PSO = function(config){

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

	updatePos = function(particle, gbest){
		personal = config$c1 * runif(1) * (particle$best$pos - particle$pos)
		social = config$c2 * runif(1) * (gbest$pos - particle$pos)
		
		particle$vel = (config$inertia * particle$vel) + personal + social	
		particle$vel = apply(as.matrix(particle$vel), 1, function(x){min(x,config$max_vel)})
		particle$vel = apply(as.matrix(particle$vel), 1, function(x){max(x,-config$max_vel)})
		
		particle$pos = particle$pos + particle$vel	
		for(i in 1:config$dim){
			particle$pos[i] = min(particle$pos[i], config$upper)
			particle$pos[i] = max(particle$pos[i], config$lower)
		}
		
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

	#Main loop
	particles = list()
	
	for(i in 1:config$swarm_size){
		pos = c()
		for(i in 1:config$dim){
			pos[i] = runif(1) * (config$upper - config$lower) + config$lower
		}
		vel = rep(0, config$dim)
		particles = push(particles, newParticle(pos, vel, config$fun))
	}
	
	gbest = calculateBest(particles);
	
	cost = c()
	for(it in 1:config$iterations){
		for(i in 1:length(particles)){
			particles[[i]] = updatePos(particles[[i]], gbest)			
		}
		gbest = calculateBest(particles)
		cost = c(cost,gbest$val)
	}
	return (list(gbest, cost))
}