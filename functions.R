
get_wait = function(dat_vax){
	pos = 1
	dist = dat_vax[pos, 'total_vaccines_distributed']
	dist_date = dat_vax[pos, 'date']
	i = 1
	counted = 0
	wait_df = data.frame(Time = NA, Doses = NA, DistDate = NA,
						AdminDate = NA)
	for (i in 1:nrow(dat_vax)){
		i
		pos
		# Set up the variables to put in the data frame
		vaxxed = dat_vax[i, 'total_vaccinations']
		new_vaxxed = dat_vax[i, 'change_vaccinations']
		date = dat_vax[i, 'date']

		# Check if we are in a nonsense state FIRST
		dist < vaxxed
		while (dist < vaxxed){
			counted = sum(wait_df$Doses, na.rm = TRUE)
			leftover = dist - counted
			pos = pos + 1
			dist = dat_vax[pos, 'total_vaccines_distributed']
			dist_date = dat_vax[pos, 'date']
			dist < vaxxed
		}

		pos > i
		if (pos > i){
			wait_df = rbind(wait_df,
						   data.frame(Time = NA,
						   		   Doses = new_vaxxed,
						   		   DistDate = NA,
						   		   AdminDate = date))
			wait_df
			dat_vax[1:i,]
			# Go back to the top of the for loop without checking anything else
			next
		}

		# If the number of vaxxed does not exceed the number distributed, just
		# add them
		wait_df = rbind(wait_df,
					   data.frame(Time = date - dist_date,
					   		   Doses = new_vaxxed,
					   		   DistDate = dist_date,
					   		   AdminDate = date))
		wait_df
		dat_vax[1:i,]
	}
	wait_df = wait_df[-1,]
	return(wait_df)
}

get_avg = function(wait_df, start = 1, end = nrow(wait_df)){
	sub_df = wait_df[start:end,]
	prd = sub_df$Time * sub_df$Doses
	avg = sum(prd, na.rm = TRUE)/sum(sub_df$Doses, na.rm = TRUE)
}

roll_avg = function(x, n){
	if (length(x) <= n){
		stop('vector needs to be longer than window')
	}
	v_avg = rep(NA, n-1)
	for (i in n:length(x)){
		avg = mean(x[(i-n+1):i], na.rm = TRUE)
		v_avg = c(v_avg, avg)
	}
	return(v_avg)
}