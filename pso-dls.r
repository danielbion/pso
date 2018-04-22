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

savePng = function(){
	return (config$savePng && config$dim == 2)
}

pso = function(config){
	particles = list()
	p = 0
	
	# Init particles with random position and velocity
	for(swarmIndex in 1:config$sub_swarms){
		for(j in 1:config$swarm_size){
			pos = runif(config$dim) * (config$limits[[swarmIndex]][2] - config$limits[[swarmIndex]][1]) + config$limits[[swarmIndex]][1]
			vel = rep(0, config$dim)
			particles = push(particles, newParticle(pos, vel, swarmIndex, config$fun))
		}
	}
	
	# Calculate initial best particles
	best = calculateBest(particles);	
	gbest = best[[1]]
	lbest = best[[2]]
	unitedLbest = best[[3]]
	
	cost = c()	
	for(it in 1:config$iterations){		
		# Update p
		p = it / config$iterations
		
		if(savePng()){
			name = paste(config$savePngPath, "pso-dls-", it, ".png", sep="")
			png(filename=name)
			plot("", ylim=c(config$lower,config$upper), xlim=c(config$lower,config$upper))
		}
		
		# Update particle positions
		for(i in 1:length(particles)){
			particles[[i]] = updatePos(particles[[i]], p, lbest, unitedLbest)	
			if(savePng()){
				points(particles[[i]]$pos[1], particles[[i]]$pos[2], ylim=c(config$lower,config$upper), xlim=c(config$lower,config$upper), col = c("green", "red", "blue", "cyan")[particles[[i]]$swarm], pch=c(1,8)[particles[[i]]$type])
			}
		}	
		
		# Update gbest, lbest, unitedLbest
		best = calculateBest(particles);	
		gbest = best[[1]]
		lbest = best[[2]]
		unitedLbest = best[[3]]
		
		if(savePng()){
			for(i in 1:config$sub_swarms){
				points(lbest[[i]]$pos[1], lbest[[i]]$pos[2], ylim=c(config$lower,config$upper), xlim=c(config$lower,config$upper), lwd = 3, col= c("green", "red", "blue", "cyan")[i], pch=25)
			}
			points(unitedLbest$pos[1], unitedLbest$pos[2], ylim=c(config$lower,config$upper), xlim=c(config$lower,config$upper), lwd = 3, col="orange", pch=23)
			dev.off()
		}
		
		cost = c(cost,gbest$val)	
	}
	
	if(savePng()){
		name = paste(config$savePngPath, "pso-dls-0.png", sep="")
		png(filename=name)
		plot(cost, type = "s")
		dev.off()
	}
	return (gbest)
}


config = c()
config$dim = 2
config$lower = -100
config$upper = 100

#config$fun = function(x){return(sum(x ^ 2))}
config$fun = function(x){
	return(20 + (x[1] ^ 2) - cos(10 * pi * x[1]) + (x[2]^2) + (10*cos(2 * pi * x[2])))
}
config$swarm_size = 10
config$c1 = 2.05
config$c2 = 2.05
config$max_vel = 1
config$inertia = 0.9
config$iterations = 100

config$sub_swarms = 4
#config$limits = list(c(-100, -50), c(-50, 0), c(0, 50), c(50, 100))
config$limits = list(c(-100, 100), c(-100, 100), c(-100, 100), c(-100, 100))

config$savePng = TRUE
config$savePngPath = "C:/Projects/pso/pso/plot/"

pso(config)