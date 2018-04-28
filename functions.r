twopeaks_func = function(z) {
  nx = length(z)
  f = 0
  t = 0
  for (i in 1:nx) {  		
    if (z[i] < 0.0) {
      t = z[i]^2 - 160.0
    }
    else if (z[i] <= 15.0) {
      t = (z[i] - 15.0)*160.0/15.0
    }
    else if (z[i] <= 20.0) {
      t = 40.0*(15.0 - z[i])
    }
    else {
      t = (z[i] - 20)^2 - 200.0
    }
    f = f + t
  }
  f = f + 200.0*nx
  return (f)	
}

fiveuneven_func = function(z) {
  nx = length(z)
  f = 0
  t = 0
  for (i in 1:nx) {  
    if (z[i] < 0.0) {
      t = (z[i]^2.0) - 200.0
    }
    else if (z[i] < 2.5) {
      t = 80.0*(z[i] - 2.5)
    }
    else if (z[i] < 5) {
      t = 64.0*(2.5 - z[i])
    }
    else if (z[i] < 7.5) {
      t = 64.0*(z[i] - 7.5)
    }
    else if (z[i] < 12.5) {
      t = 28.0*(7.5 - z[i])
    }
    else if (z[i] < 17.5) {
      t = 28.0*(z[i] - 17.5)
    }
    else if (z[i] < 22.5) {
      t = 32.0*(17.5 - z[i])
    }
    else if (z[i] < 27.5) {
      t = 32.0*(z[i] - 27.5)
    }
    else if (z[i] <= 30) {
      t = 80.0*(27.5 - z[i])
    }
    else {
      t = (z[i] - 30.0)^2.0 - 200.0
    }
    f = f + t
  }
  f = f + 200.0*nx
  return (f)
}

equalmin_func = function(z) {
  nx = length(z)
  f = 0
  for (i in 1:nx) {  
    if ((z[i] >= 0.0) & (z[i] <= 1.0)) {
      f = f - sin(5*pi*z[i])^6.0
    }
    else {
      f = f + z[i]^2.0
    }
  }
  f = f + nx
  return (f)
}

decreasemin_func = function(z) {
  nx = length(z)
  f = 0
  for (i in 1:nx) {  
    if ((z[i] >= 0.0) & (z[i] <= 1.0)) {
      f = f - exp(-2.0*log(2.0)*(((z[i]-0.1)/0.8)^2.0))*(sin(5.0*pi*z[i])^6.0)
    }
    else {
      f = f + z[i]^2.0
    }
  }
  f = f + nx
  return (f)
}

unevenmin_func = function(z) {
  nx = length(z)
  f = 0
  for (i in 1:nx) {  
    if ((z[i] >= 0.0) & (z[i] <= 1.0)) {
      f = f - sin(5.0*pi*(z[i]^0.75-0.05))^6.0
    }
    else {
      f = f + z[i]^2.0
    }
  }
  f = f - nx
  return (f)
}

himmelblau_func = function(z) { 
  nx = length(z)
  f = 0
  i = 1
  while (i <= nx - 1) {
    f = f + ((z[i]^2.0+z[i+1]-11.0)^2.0)+((z[i]+z[i+1]^2.0-7.0)^2.0)
    i = i + 2
  }
  return (f)
}

camelback_func = function(z){ 
  nx = length(z)
  f = 0
  i = 1
  while (i <= nx - 1){  
    f = f - 4.0*((4.0-2.1*(z[i]^2.0)+(z[i]^4.0)/3.0)*(z[i]^2.0) + z[i]*z[i+1] + (4.0*(z[i+1]^2.0)-4.0)*(z[i+1]^2.0))
      i = i + 2
  }
  return (f)
}

vincent_func = function(z){ 
  nx = length(z)
  f = 0
  for (i in 1:nx){
    if (z[i] < 0.25) {
      f = f + (0.25-z[i])^2.0 + sin(10.0*log(2.5)) + 0.1
    }
    else if (z[i] <= 10.0) {
      f = f + sin(10.0*log(z[i])) + 0.1
    }
    else {
      f = f + (z[i]-10)^2.0 + sin(10.0*log(10.0)) + 0.1
    }
  }
  f = f/nx
  return (f)
}

sphere_func = function(z){ 
  nx = length(z)
  f = 0
  for (i in 1:nx){  
    f = f + z[i]^2.0
  }
  return (f)
}

ellips_func = function(z){ 
  nx = length(z)
  f = 0
  for (i in 1:nx){  
    f = f + (1000000^((i-1)/(nx-1)))*(z[i]^2.0)
  }
  return (f)
}

bent_cigar_func = function(z){ 
  nx = length(z)
  f = 0
  for (i in 2:nx){  
    f = f + z[i]^2.0
  }
  f = z[1]^2.0 + 1000000*f
  return (f)
}

discus_func = function(z){ 
  nx = length(z)
  f = 0
  for (i in 2:nx){  
    f = f + z[i]^2.0
  }
  f = f + 1000000*(z[1]^2.0)
  return (f)
}

dif_powers_func = function(z){ 
  nx = length(z)
  f = 0
  for (i in 1:nx){  
    f = f + abs(z[i])^(2.0+4.0*((i-1.0)/(nx-1.0)))
  }
  f = sqrt(f)
  return (f)
}

rosenbrock_func = function(z){ 
  nx = length(z)
  f = 0
  i = 1
  while (i <= nx - 1) { 
    f = f + 100.0*((z[i]^2.0-z[i+1])^2.0) + (z[i]-1.0)^2.0
    i = i + 1
  }
  return (f)
}

ackley_func = function(z){ 
  nx = length(z)
  sum1 = 0
  sum2 = 0
  for (i in 1:nx){  
    sum1 = sum1 + z[i]^2.0
    sum2 = sum2 + cos(2.0*pi*z[i])
  }
  sum1 = -exp(-0.2*sqrt(sum1/nx))
  sum2 = -exp(sum2/nx)
  f = 20.0*sum1 + sum2 + 20.0 + exp(1)
  return (f)
}

weierstrass_func = function(z){ 
  nx = length(z)
  f = 0
  a = 0.5
  b = 3.0
  k_max = 20
  for (i in 1:nx){ 
    sum = 0
    for (k in 0:k_max){  
      sum = sum + (a^k)*cos(2.0*pi*(b^k)*(z[i]+0.5))
    }
    f = f + sum
  }
  sum = 0
  for (k in 0:k_max){  
    sum = sum + (a^k)*cos(pi*(b^k))
  }
  f = f - nx*sum
  return (f)
}

griewank_func = function(z){ 
  nx = length(z)
  f = 0
  s = 0.0
  p = 1.0
  for (i in 1:nx){ 
    s = s + z[i]^2.0
    p = p * cos(z[i]/sqrt(i))
  }
  f = s/4000.0 - p + 1
  return (f)
}

rastrigin_func = function(z){ 
  nx = length(z)
  f = 0
  for (i in 1:nx){ 
    f = f + (z[i]^2.0 - 10.0*cos(2.0*pi*z[i]) + 10.0)
  }
  return (f)
}

f6_func = function(x, y){ 
  temp = x^2.0 + y^2.0
  f = 0.5 + (sin(sqrt(temp))^2 - 0.5)/ ((1 + 0.001 * temp)^2.0)
  return (f)
}

escaffer6_func = function(z){ 
  nx = length(z)
  f = 0
  i = 1
  while (i <= nx - 1) { 
     f = f + f6_func(z[i],z[i+1])
     i = i + 1
    }
  f = f + f6_func(z[nx],z[1])
  return (f)
}

schwefel_func = function(z){ 
  nx = length(z)
  f = 0
  temp = 0
  for (i in 1:nx){ 
    z[i] = z[i] + 4.209687462275036e+002
    if (z[i] > 500) {
      f = f - (500.0-(z[i]%%500))*sin(sqrt(abs(500.0-(z[i]%%500))))
      tmp = (z[i]-500.0)/100.0
      f = f + tmp*tmp/nx
    }
    else if (z[i] < -500)
    {
      f = f - (-500.0 + (abs(z[i])%%500))*sin(sqrt(abs(abs(z[i])%%500-500.0)))
      tmp = (z[i]+500.0)/100.0
      f = f + tmp*tmp/nx
    }
    else {
      f = f - z[i]*sin(abs(z[i])^0.5)
    }
  }
  f = f + 4.189828872724338e+002*nx
  return (f)
}

katsuura_func = function(z){ 
  nx = length(z)
  f = 1.0
  temp3 = 10/(nx^1.2)
  for (i in 1:nx){ 
    temp = 0.0
    for (j in 1:32){ 
      temp1 = 2.0^j
      temp2 = temp1*z[i]
      temp = temp + abs(temp2-round(temp2))/temp1
    }
    f = f * ((1.0 + i*temp)^temp3)
  }
  f = 10.0*(f - 1)/(nx^2.0)
  return (f)
}

happycat_func = function(z){ 
  nx = length(z)
  r2 = 0.0
  sum_z = 0.0
  for (i in 1:nx){ 
    r2 = r2 + z[i]^2.0
    sum_z = sum_z + z[i]
  }
  f = abs(r2-nx)^0.25 + (0.5*r2 + sum_z)/nx + 0.5
  return (f)
}

hgbat_func = function(z){ 
  nx = length(z)
  f = 0.0
  r2 = 0.0
  sum_z = 0.0
  for (i in 1:nx){ 
    r2 = r2 + z[i]^2.0
    sum_z = sum_z + z[i]
  }
  f = abs(r2^2.0 - sum_z^2.0)^0.5 + (0.5*r2 + sum_z)/nx + 0.5
  return (f)
}

grie_rosen_func = function(z){ 
  nx = length(z)
  f = 0
  i = 1
  while (i <= nx - 1) { 
    temp = 100.0*((z[i]^2.0-z[i+1])^2.0) + (z[i]-1.0)^2.0
    temp1 = temp^2/4000.0 - cos(temp) + 1
    f = f + temp1
    i = i + 1
  }
  temp = 100.0*((z[nx]^2.0-z[1])^2.0) + (z[nx]-1.0)^2.0
  temp1 = temp^2/4000.0 - cos(temp) + 1
  f = f + temp1
                         
  return (f)
}

cf01 = function(z){ 
  nx = length(z)
  cf_num = 10
  sigma = c(10,20,10,20,10,20,10,20,10,20)
  lamnda = c(1, 1, 1e-6, 1e-6, 1e-6, 1e-6, 1e-4, 1e-4, 1e-5, 1e-5)
  bias = c(0,0,0,0,0,0,0,0,0,0)
  g = rep(0, times = cf_num)
  g[1] = sphere_func(z)
  g[2] = sphere_func(z)
  g[3] = ellips_func(z)
  g[4] = ellips_func(z)
  g[5] = bent_cigar_func(z)
  g[6] = bent_cigar_func(z)
  g[7] = discus_func(z)
  g[8] = discus_func(z)  
  g[9] = dif_powers_func(z)
  g[10] = dif_powers_func(z)
  return(cf_cal(z, nx, cf_num, sigma, lamnda, bias, g))
}

cf02 = function(z){ 
  nx = length(z)
  cf_num = 10
  sigma = c(10,20,30,40,50,60,70,80,90,100)
  lamnda = c(1e-5, 1e-5, 1e-6, 1e-6, 1e-6, 1e-6, 1e-4, 1e-4, 1, 1)
  bias = c(0,10,20,30,40,50,60,70,80,90)
  g = rep(0, times = cf_num)
  g[1] = ellips_func(z)
  g[2] = ellips_func(z)
  g[3] = dif_powers_func(z)
  g[4] = dif_powers_func(z)
  g[5] = bent_cigar_func(z)
  g[6] = bent_cigar_func(z)
  g[7] = discus_func(z)
  g[8] = discus_func(z)  
  g[9] = sphere_func(z)
  g[10] = sphere_func(z)
  return(cf_cal(z, nx, cf_num, sigma, lamnda, bias, g) )
}

cf03 = function(z){ 
  nx = length(z)
  cf_num = 10
  sigma = c(10,10,10,10,10,10,10,10,10,10)
  lamnda = c(0.1, 0.1, 10, 10, 10, 10, 100, 100, 1, 1)
  bias = c(0,0,0,0,0,0,0,0,0,0)
  g = rep(0, times = cf_num)
  g[1] = rosenbrock_func(z)
  g[2] = rosenbrock_func(z)
  g[3] = rastrigin_func(z)
  g[4] = rastrigin_func(z)
  g[5] = happycat_func(z)
  g[6] = happycat_func(z)
  g[7] = escaffer6_func(z)
  g[8] = escaffer6_func(z)  
  g[9] = schwefel_func(z)
  g[10] = schwefel_func(z)
  return(cf_cal(z, nx, cf_num, sigma, lamnda, bias, g) )
}

cf04 = function(z){ 
  nx = length(z)
  cf_num = 10
  sigma = c(10,10,20,20,30,30,40,40,50,50)
  lamnda = c(0.1, 0.1, 10, 10, 10, 10, 100, 100, 1, 1)
  bias = c(0,0,0,0,0,0,0,0,0,0)
  g = rep(0, times = cf_num)
  g[1] = rosenbrock_func(z)
  g[2] = rosenbrock_func(z)
  g[3] = rastrigin_func(z)
  g[4] = rastrigin_func(z)
  g[5] = happycat_func(z)
  g[6] = happycat_func(z)
  g[7] = escaffer6_func(z)
  g[8] = escaffer6_func(z)  
  g[9] = schwefel_func(z)
  g[10] = schwefel_func(z)
  return(cf_cal(z, nx, cf_num, sigma, lamnda, bias, g) )
}

cf05 = function(z){ 
  nx = length(z)
  cf_num = 10
  sigma = c(10,20,30,40,50,60,70,80,90,100)
  lamnda = c(0.1, 0.1, 10, 0.1, 2.5, 1e-3, 100, 2,5, 10, 1)
  bias = c(0,0,0,0,0,0,0,0,0,0)
  g = rep(0, times = cf_num)
  g[1] = rosenbrock_func(z)
  g[2] = hgbat_func(z)
  g[3] = rastrigin_func(z)
  g[4] = ackley_func(z)
  g[5] = weierstrass_func(z)
  g[6] = katsuura_func(z)
  g[7] = escaffer6_func(z)
  g[8] = grie_rosen_func(z)  
  g[9] = happycat_func(z)
  g[10] = schwefel_func(z)
  return(cf_cal(z, nx, cf_num, sigma, lamnda, bias, g) )
}

cf06 = function(z){ 
  nx = length(z)
  cf_num = 10
  sigma = c(10,10, 20, 20, 30, 30, 40, 40, 50, 50)
  lamnda = c(10, 1, 10, 1, 10, 1, 10, 1, 10, 1)
  bias = c(0, 20, 40, 60, 80, 100, 120, 140, 160, 180)
  g = rep(0, times = cf_num)
  g[1] = rastrigin_func(z)
  g[2] = schwefel_func(z)
  g[3] = rastrigin_func(z)
  g[4] = schwefel_func(z)
  g[5] = rastrigin_func(z)
  g[6] = schwefel_func(z)
  g[7] = rastrigin_func(z)
  g[8] = schwefel_func(z)  
  g[9] = rastrigin_func(z)
  g[10] = schwefel_func(z)
  return(cf_cal(z, nx, cf_num, sigma, lamnda, bias, g))
}

cf07 = function(z){ 
  nx = length(z)
  cf_num = 10
  sigma = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
  lamnda = c(0.1, 10, 10, 0.1, 2.5, 1e-3, 100, 2.5, 10, 1)
  bias = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  g = rep(0, times = cf_num)
  g[1] = rosenbrock_func(z)
  g[2] = hgbat_func(z)
  g[3] = rastrigin_func(z)
  g[4] = ackley_func(z)
  g[5] = weierstrass_func(z)
  g[6] = katsuura_func(z)
  g[7] = escaffer6_func(z)
  g[8] = grie_rosen_func(z)  
  g[9] = happycat_func(z)
  g[10] = schwefel_func(z)
  return(cf_cal(z, nx, cf_num, sigma, lamnda, bias, g) )
}

cf_cal = function(z, nx, cf_num, sigma, lamnda, bias, g){
  w_max=0
  w_sum=0
  w = rep(0, times = cf_num)
  o = sample(-80:80,cf_num*nx,replace=T)
  for (i in 1:cf_num) {
    w[i]=0
    for (j in 1:nx) {
      w[i]= w[i] + (z[j]-o[(i-1)*nx+j])^2
    }
    if (w[i] != 0) {
      w[i]=(1.0/w[i]^0.5)*exp(-w[i]/(2.0*nx*(sigma[i]^2.0)))
    }
    else {
      w[i] = 1e+80
    }
    if (w[i] > w_max) {
      w_max = w[i]
    }
    w_sum = w_sum + w[i]
  }
  if (w_max == 0) {
    w = rep(1,cf_num)
    w_sum = cf_num
  }
  w = w/w_sum
  f = 0
  for (i in 1:cf_num) {
    f = f + w[i]*(lamnda[i]*g[i]+bias[i])
  }
  return (f)
}