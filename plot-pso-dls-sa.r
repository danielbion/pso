PSO_DLS = function(config){

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
		globalBest$val = 10000000
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

		for(j in 1:length(localBest)){
			if(localBest[[j]]$temp < config$initialTemp / 10){
				for(i in 1:length(particles)){
					swarm = particles[[i]]$swarm
					if(swarm == j){
						p = acceptProbability(localBest[[j]]$val, particles[[i]]$best$val, particles[[i]]$best$temp)
						r = runif(1)
						if(p > r){
							localBest[[j]] = particles[[i]]$best
						}	
					}
				}
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
			particle$pos[i] = min(particle$pos[i], config$upper[i])
			particle$pos[i] = max(particle$pos[i], config$lower[i])
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
	    if(newFit == currentFit) {
            return (0)
        }
		if(newFit < currentFit){
			return (1)
		}
		return (exp((currentFit - newFit) / temperature))
	}

	savePng = function(){
		return (config$savePng && config$dim == 2)
	}

	# Main loop
	colors = c("gray0", "deeppink4", "green", 
	"red", "darkblue", "cyan", 
	"darkorchid", "deepskyblue2", "darkgreen", 
	"chocolate1", "darkgoldenrod1")
	particles = list()
	p = 0
	
	# Init particles with random position and velocity
	for(swarmIndex in 1:config$sub_swarms){
		for(j in 1:config$swarm_size){
			pos = c()
			for(i in 1:config$dim){
				pos[i] = runif(1) * (config$limitUpper - config$limitLower) + config$limitLower
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
		p = it / config$iterations * 2
		if(p > 1) p = 1
		
		#print (temp)
		if(savePng()){
			dir.create("plot/dls", showWarnings = FALSE)
			if(it < 10) it = paste("0", it, sep="")
			if(it < 100) it = paste("0", it, sep="")
			name = paste(config$savePngPath, "dls/dls", it, ".png", sep="")
			png(filename=name)
			plot("", ylim=c(config$lower[2],config$upper[2]), xlim=c(config$lower[1],config$upper[2]))
		}
		
		# Update particle positions
		for(i in 1:length(particles)){
			particles[[i]] = updatePos(particles[[i]], p, lbest, unitedLbest)	
			if(savePng()){
				points(particles[[i]]$pos[1], particles[[i]]$pos[2], ylim=c(config$lower[2],config$upper[2]), xlim=c(config$lower[1],config$upper[1]), col = colors[particles[[i]]$swarm], pch=c(1,4)[particles[[i]]$type])
			}
		}	
		
		# Update gbest, lbest, unitedLbest
		best = calculateBest(particles);	
		gbest = best[[1]]
		lbest = best[[2]]
		unitedLbest = best[[3]]
		particles = best[[4]]
		# for(i in 1:length(particles)){print(particles[[i]]$best$temp)}
		
		if(savePng()){
			for(i in 1:config$sub_swarms){
				points(lbest[[i]]$pos[1], lbest[[i]]$pos[2], ylim=c(config$lower[2],config$upper[2]), xlim=c(config$lower[1],config$upper[1]), cex = 2, lwd = 2, col= colors[i], pch=c(1,4)[lbest[[i]]$type])
			}
			points(unitedLbest$pos[1], unitedLbest$pos[2], ylim=c(config$lower[2],config$upper[2]), xlim=c(config$lower[1],config$upper[1]), lwd = 3, col="orange", pch=23)
			dev.off()
		}
		
		cost = c(cost,gbest$val)	
	}
	
	if(savePng()){
		name = paste(config$savePngPath, "dls/dls0.png", sep="")
		png(filename=name)
		plot(cost, type = "s")
		dev.off()
	}
	return (gbest)
}

path = "C:/Projects/pso/pso"
setwd(path)
source("functions.r")

config = c()
config$dim = 2
config$lower = c(-100, -100)
config$upper = c(100, 100)
config$fun = cf01
config$swarm_size = 10
config$c1 = 1.49445
config$c2 = 1.49445
config$max_vel = 6
config$inertia = 0.9
config$iterations = 500
config$coolingRate = 0.05
config$heatingRate = 0.005
config$initialTemp = 1000

config$sub_swarms = 10
config$limitLower = -100
config$limitUpper = 100

config$savePng = TRUE
config$savePngPath = "C:/Projects/pso/pso/plot/"

PSO_DLS(config)