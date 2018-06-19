PSO_DLS_SA = function(config){

	newParticle = function(pos, vel, swarm, fitFun, temp) {
		return (list(
			pos = pos,
			vel = vel,
			best = list(
				pos = pos,
				type = 1,
				val = fitFun(pos),
				temp = temp
			),
			type = 1,
			swarm = swarm
		))
	}

	calculateBest = function(particles) {
		globalBest = particles[[1]]$best
		globalBest$val = 100000
		localBest = list()
		for(i in 1:config$sub_swarms){
			localBest[[i]] = globalBest
		}

		for(i in 1:length(particles)){
			swarm = particles[[i]]$swarm
			p = acceptProbability(localBest[[swarm]]$val, particles[[i]]$best$val, particles[[i]]$best$temp)
			r = runif(1)
			if(p > r){
				localBest[[swarm]] = particles[[i]]$best
			}			
			if(particles[[i]]$best$val < globalBest$val){
				globalBest = particles[[i]]$best
			}
		}

		for(i in 1:length(particles)){
			swarm = particles[[i]]$swarm
			if(particles[[i]]$best$val == localBest[[swarm]]$val){
				particles[[i]]$best$temp = particles[[i]]$best$temp * (1 - config$coolingRate)
			}else{
				particles[[i]]$best$temp = particles[[i]]$best$temp  + (particles[[i]]$best$temp * config$heatingRate)
				if(particles[[i]]$best$temp > config$initialTemp){
					particles[[i]]$best$temp = config$initialTemp
				}
			}
		}
		
		pos = c()
		for(i in 1:length(localBest)){
			pos = rbind(pos, localBest[[i]]$pos)
		}
		pos = colMeans(as.matrix(pos))
		unitedLocalbest = globalBest
		unitedLocalbest$pos = pos
		unitedLocalbest$val = config$fun(pos);
		
		best = list(globalBest, localBest, unitedLocalbest, particles)
		return(best)
	}

	updatePos = function(particle, p, lbest, unitedLbest){
		personal = config$c1 * runif(1) * (particle$best$pos - particle$pos)
		
		chance = runif(1)
		if(chance <= p){
			particle$type = 2
			particle$best$type = 2
			social = config$c2 * runif(1) * (unitedLbest$pos - particle$pos)
		}else {
			particle$type = 1
			particle$best$type = 1
			social = config$c2 * runif(1) * (lbest[[particle$swarm]]$pos - particle$pos)
		}

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

	acceptProbability = function(currentFit, newFit, temperature){
		if(newFit < currentFit){
			return (1);
		}
		return (exp((currentFit - newFit) * temperature))
	}

	# Main loop
	particles = list()
	p = 0
	
	# Init particles with random position and velocity
	for(swarmIndex in 1:config$sub_swarms){
		for(j in 1:config$swarm_size){
			pos = c()
			for(i in 1:config$dim){
				pos[i] = runif(1) * (config$upper - config$lower) + config$lower
			}
			vel = rep(0, config$dim)
			particles = push(particles, newParticle(pos, vel, swarmIndex, config$fun, config$initialTemp))
		}
	}
	
	# Calculate initial best particles
	best = calculateBest(particles);	
	gbest = best[[1]]
	lbest = best[[2]]
	unitedLbest = best[[3]]
	particles = best[[4]]
	
	cost = c()	
	for(it in 1:config$iterations){		
		# Update p
		p = it / config$iterations
		
		# Update particle positions
		for(i in 1:length(particles)){
			particles[[i]] = updatePos(particles[[i]], p, lbest, unitedLbest)
		}	
		
		# Update gbest, lbest, unitedLbest
		best = calculateBest(particles);	
		gbest = best[[1]]
		lbest = best[[2]]
		unitedLbest = best[[3]]
		particles = best[[4]]
		
		cost = c(cost,gbest$val)	
	}
	return (list(gbest, cost))
}