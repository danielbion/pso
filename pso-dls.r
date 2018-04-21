newParticle = function(pos, vel, swarm, fitFun) {
	return (list(
		pos = pos,
		vel = vel,
		best = list(
			pos = pos,
			val = fitFun(pos)
		),
		type = 1,
		swarm = swarm
	))
}

calculateBest = function(particles) {
	globalBest = particles[[1]]$best
	localBest = list()
	for(i in 1:config$sub_swarms){
		localBest[[i]] = globalBest
	}
	
	for(i in 1:length(particles)){
		swarm = particles[[i]]$swarm
		if(particles[[i]]$best$val < localBest[[swarm]]$val){
			localBest[[swarm]] = particles[[i]]$best
		}
		if(particles[[i]]$best$val < globalBest$val){
			globalBest = particles[[i]]$best
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
	
	best = list(globalBest, localBest, unitedLocalbest)
	return(best)
}

updatePos = function(particle, p, lbest, unitedLbest){
	personal = config$c1 * runif(1) * (particle$best$pos - particle$pos)
	
	chance = runif(1)
	if(chance <= p){
		particle$type = 2
		social = config$c2 * runif(1) * (unitedLbest$pos - particle$pos)
	}else {
		particle$type = 1
		social = config$c2 * runif(1) * (lbest[[particle$swarm]]$pos - particle$pos)
	}

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
	p = 0
	
	for(swarmIndex in 1:config$sub_swarms){
		for(j in 1:config$swarm_size){
			pos = runif(config$dim) * (config$upper - config$lower) + config$lower
			vel = rep(0, config$dim)
			particles = push(particles, newParticle(pos, vel, swarmIndex, config$fun))
		}
	}
	
	best = calculateBest(particles);	
	gbest = best[[1]]
	lbest = best[[2]]
	unitedLbest = best[[3]]
	
	cost = c()	
	for(it in 1:config$iterations){		
	# it = it + 1
		p = it / config$iterations
		pos = c()		
		swarm = c()	
		for(i in 1:length(particles)){
			particles[[i]] = updatePos(particles[[i]], p, lbest, unitedLbest)
			pos = rbind(pos, cbind(particles[[i]]$pos[1], particles[[i]]$pos[2]))
			swarm = rbind(swarm, particles[[i]]$swarm)
		}	
		best = calculateBest(particles);	
		gbest = best[[1]]
		lbest = best[[2]]
		unitedLbest = best[[3]]
		cost = c(cost,gbest$val)		
		plot(pos, ylim=c(config$lower,config$upper), xlim=c(config$lower,config$upper), col = c("green", "red", "blue")[swarm])
		points(lbest[[1]]$pos[1], lbest[[1]]$pos[2], ylim=c(config$lower,config$upper), xlim=c(config$lower,config$upper), lwd = 3, col="green", pch=25)
		points(lbest[[2]]$pos[1], lbest[[2]]$pos[2], ylim=c(config$lower,config$upper), xlim=c(config$lower,config$upper), lwd = 3, col="red", pch=25)
		points(lbest[[3]]$pos[1], lbest[[3]]$pos[2], ylim=c(config$lower,config$upper), xlim=c(config$lower,config$upper), lwd = 3, col="blue", pch=25)
		points(unitedLbest$pos[1], unitedLbest$pos[2], ylim=c(config$lower,config$upper), xlim=c(config$lower,config$upper), lwd = 3, col="pink", pch=23)
		
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
config$swarm_size = 10
config$c1 = 2.05
config$c2 = 2.05
config$max_vel = 2
config$inertia = 0.9
config$iterations = 100
config$sub_swarms = 3

pso(config)