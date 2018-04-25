
twopeaks_func = function(z){
	nx = length(z)
	f = 0
	for (i in 1:nx){			
		if ((z[i]<15.0)&(z[i]>=0.0))
		{
			f = f - (160.0/15.0)*(15.0-z[i])
		}
		else if ((z[i]<=20.0)&(z[i]>=15.0))
		{
			f = f -40.0*(z[i]-15.0);
		}
		else if (z[i]<0.0)
		{
			f = f -160.0+ z[i]^2
		}
		else
		{
			f = f -200.0+ (z[i]-20.0)^2
		}
	}
	f = f + (200.0*nx);
	return (f)	
}

fiveuneven_func = function(z){
	nx = length(z)
	f = 0
	for (i in 1:nx){	
		if (z[i]<0)
		{
			f = f -200.0+(z[i]^2.0);
		}
		else if (z[i]<2.5)
		{
			f = f -80.0*(2.5-z[i]);
		}
		else if (z[i]<5.0)
		{
			f = f -64.0*(z[i]-2.5);
		}
		else if (z[i]<7.5)
		{
			f = f -160.0+(z[i]^2.0);
		}
		else if (z[i]<12.5)
		{
			f = f -28.0*(z[i]-7.5);
		}
		else if (z[i]<17.5)
		{
			f = f -28.0*(17.5-z[i]);
		}
		else if (z[i]<22.5)
		{
			f = f -32.0*(z[i]-17.5);
		}
		else if (z[i]<27.5)
		{
			f = f -32.0*(27.5-z[i]);
		}
		else if (z[i]<=30.0)
		{
			f = f -80.0*(z[i]-27.5);
		}
		else
		{
			f = f -200.0+(z[i]-30.0)^2.0;
		}
	}
	f = f + 200.0*nx;
	return (f)
}