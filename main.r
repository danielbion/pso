path = "C:/Projects/pso/pso"
setwd(path)
source("pso.r")
source("pso-dls.r")
source("functions.r")

cfg = c()
cfg$dim = 10
cfg$lower = -100
cfg$upper = 100
cfg$swarm_size = 20
cfg$c1 = 2.05
cfg$c2 = 2.05
cfg$max_vel = 2
cfg$inertia = 0.9
cfg$iterations = 500

cfg$sub_swarms = 4

functions = c("twopeaks_func", "fiveuneven_func")
monteCarlo = 30

test = function(method, cfg, functions, monteCarlo){
	results = list()
	for(i in 1:length(functions)){	
		result = c()
		
		# Choose the Fitness Function
		cfg$fun = eval(parse(text=functions[i]))
		
		result$pso_mean_cost = c()
		result$pso_gbest = c()
		
		for(j in 1:monteCarlo){
			retorno = method(cfg)
			
			# Store best fit for each MC iteration
			result$pso_gbest = c(result$pso_gbest, retorno[[1]]$val)
			
			# Store and update the mean of gbest progress of MC runs
			if(j == 1){
				result$pso_mean_cost = retorno[[2]]
			}		
			result$pso_mean_cost = colMeans(rbind(result$pso_mean_cost, retorno[[2]]))
		}
		
		results[[i]] = result
	}
	return (results)
}

PSO_result = test(PSO, cfg, functions, monteCarlo)
PSO_DLS_result = test(PSO_DLS, cfg, functions, monteCarlo)