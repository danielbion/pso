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

	savePng = function(){
		return (config$savePng && config$dim == 2)
	}

	# Main loop
	particles = list()
	p = 0
	
	# Init particles with random position and velocity
	for(i in 1:config$swarm_size){
		pos = c()
		for(i in 1:config$dim){
			pos[i] = runif(1) * (config$upper - config$lower) + config$lower
		}
		vel = rep(0, config$dim)
		particles = push(particles, newParticle(pos, vel, config$fun))
	}
	
	# Calculate initial best particles
	gbest = calculateBest(particles);
	
	cost = c()
	for(it in 1:config$iterations){
		
		if(savePng()){
			dir.create("plot/pso", showWarnings = FALSE)
			if(it < 10) it = paste("0", it, sep="")
			if(it < 100) it = paste("0", it, sep="")
			name = paste(config$savePngPath, "pso/pso", it, ".png", sep="")
			png(filename=name)
			plot("", ylim=c(config$lower,config$upper), xlim=c(config$lower,config$upper))
		}
		
		for(i in 1:length(particles)){
			particles[[i]] = updatePos(particles[[i]], gbest)	
			if(savePng()){
				points(particles[[i]]$pos[1], particles[[i]]$pos[2], ylim=c(config$lower,config$upper), xlim=c(config$lower,config$upper), col = c("blue"))
			}			
		}
		gbest = calculateBest(particles)
		cost = c(cost,gbest$val)
		if(savePng()){
			points(gbest$pos[1], gbest$pos[2], ylim=c(config$lower,config$upper), xlim=c(config$lower,config$upper), lwd = 3, col="red", pch=23)
			dev.off()
		}
	}
	if(savePng()){
		name = paste(config$savePngPath, "pso/pso0.png", sep="")
		png(filename=name)
		plot(cost, type = "s")
		dev.off()
	}
	return (gbest)
}

source("functions.r")

config = c()
config$dim = 2
config$lower = -100
config$upper = 100
config$fun = cf01
config$swarm_size = 40
config$c1 = 1.49445
config$c2 = 1.49445
config$max_vel = 2
config$inertia = 0.9
config$iterations = 1000

config$savePng = TRUE
config$savePngPath = "C:/Projects/pso/pso/plot/"

PSO(config)