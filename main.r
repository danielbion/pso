path = "C:/Projects/pso/pso"
setwd(path)
source("pso.r")
source("pso-dls.r")
source("functions.r")

test = function(method, cfg, functions, monteCarlo){
	results = list()
	for(i in 1:length(functions)){	
		print(paste("Function ", i))
		result = c()
		
		# Choose the Fitness Function
		cfg$fun = eval(parse(text=functions[i]))
		
		result$pso_mean_cost = c()
		result$pso_gbest = c()
		
		for(j in 1:monteCarlo){
			print(paste("MC iteration ", j))
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

cfg = c()
cfg$dim = 10
cfg$lower = -100
cfg$upper = 100
cfg$swarm_size = 40
cfg$c1 = 1.49445
cfg$c2 = 1.49445
cfg$max_vel = 100
cfg$inertia = 0.9
cfg$iterations = 2500

# Listing the functions to test
functions = c("twopeaks_func", "fiveuneven_func")

# Setup MC experiments
monteCarlo = 30
dir.create("results", showWarnings = FALSE)

# Test standard PSO
PSO_result = test(PSO, cfg, functions, monteCarlo)
# Save the results for PSO, use: PSO_result = readRDS("results/pso.rds") to read later
saveRDS(PSO_result, "results/pso.rds")

# Test PSO DLS
cfg$swarm_size = 4
cfg$sub_swarms = 10
PSO_DLS_result = test(PSO_DLS, cfg, functions, monteCarlo)
# Save the results for PSO-DLS, use: PSO_DLS_result = readRDS("results/pso-dls.rds") to read later
saveRDS(PSO_DLS_result, "results/pso-dls.rds") 


# Save the cost history (convergence) for each function
for(i in 1:length(functions)){	
	minY = min(min(PSO_result[[i]]$pso_mean_cost), min(PSO_DLS_result[[i]]$pso_mean_cost))
	maxY = max(max(PSO_result[[i]]$pso_mean_cost), max(PSO_DLS_result[[i]]$pso_mean_cost))
		
	name = paste("results/function-", i,".png", sep="")
	png(filename=name)
	plot("", ylim = c(minY, maxY), xlim = c(0, cfg$iterations),xlab="Iteration",ylab="")
	legend("topright", legend = c("PSO", "PSO-DLS"), fill=c("blue", "red"), bty="n")
	lines(PSO_result[[i]]$pso_mean_cost, col = "blue")
	lines(PSO_DLS_result[[i]]$pso_mean_cost, col = "red")
	dev.off()
}


getMetrics = function(vec){
	return (list(
		min = min(vec),
		max = max(vec),
		mean = mean(vec),
		median = median(vec),
		sd = sd(vec)
	))
}

functionsResult = list()

for(i in 1:length(functions)){
	pso = PSO_result[[i]]$pso_gbest
	pso_dls = PSO_DLS_result[[i]]$pso_gbest
	
	functionsResult[[i]] = list()
	functionsResult[[i]]$tests = wilcox.test(pso_dls, pso, paired = TRUE, alternative = "less", conf.level = 0.95)
	functionsResult[[i]]$pso = getMetrics(pso)
	functionsResult[[i]]$pso_dls = getMetrics(pso_dls)
	
	saveRDS(functionsResult, "results/tests.rds") 
}

