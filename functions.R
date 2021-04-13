
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
	if (n == 'all'){
		v_avg = c()
		for (i in 1:length(x)){
			avg = mean(x[1:i], na.rm = TRUE)
			v_avg = c(v_avg, avg)
		}
	} else if (length(x) <= n){
		stop('vector needs to be longer than window')
	} else {
		v_avg = rep(NA, n-1)
		for (i in n:length(x)){
			avg = mean(x[(i-n+1):i], na.rm = TRUE)
			v_avg = c(v_avg, avg)
		}
	}
	return(v_avg)
}

what_if = function(dat_vax){
	vax_adm = data.frame(Date = NA, Doses = NA, OneDay = NA, TwoDay = NA)
	max_adm = 0
	new_d = 0
	dist_1 = 0
	dist_2 = 0
	avail = 0
	i = min(dat_vax$date) -1
	for (i in min(dat_vax$date, na.rm = TRUE):max(dat_vax$date, na.rm = TRUE)){
		# How many vaccines were distributed today
		new_d = dat_vax[dat_vax[,'date'] == i, 'change_vaccines_distributed']
		new_d
		# Add this to the number available
		avail = avail + new_d
		avail

		# Our current capacity is max_adm. Administer at least that many, if they
		# are available
		max_adm
		adm = min(max_adm, avail)
		adm

		# Did we use up all the dist_2?
		dist_2 = dist_2 - adm
		dist_2

		if (dist_2 <= 0) {
			# if we used more than were in dist_2, take them out of dist_1
			dist_1 = dist_1 + dist_2
			dist_1
			if (dist_1 < 0){
				# if we used more than were in dist_1, take them out of new_d
				new_d = new_d + dist_1
				new_d # this should never be less than 0
			}
		} else {
			# if we didn't use them all up, it's time to expand capacity
			adm = adm + dist_2
			adm
			max_adm = max(max_adm, adm)
			max_adm
		}

		# Push dist_1 back to dist_2
		dist_2 = max(dist_1, 0)
		dist_2
		# Push new vax back to dist_1
		dist_1 = new_d
		dist_1
		# Count all available
		avail = dist_1 + dist_2
		avail

		# Update the data.frame
		vax_adm = rbind(vax_adm, data.frame(Date = dat_vax[dat_vax[,'date'] == i,'date'],
											Doses = adm, OneDay = dist_1,
											TwoDay = dist_2))
		vax_adm
	}
	vax_adm = vax_adm[-1,]
	return(vax_adm)
}