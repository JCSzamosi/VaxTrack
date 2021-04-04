Vax Track
=========

Tracking how quickly Canada is moving its vaccine supply into human beings. All
data are taken from [covid19tracker.ca](https://covid19tracker.ca). This 
analysis is at the dose level and does not distinguish between first and second
doses, or vaccine types.

## Setup

<details><summary>Click here to see the setup code</summary>

Load packages and import data:


```r
knitr::opts_chunk$set(message = FALSE)
setwd('~/MyProjects/COVID19/VaxTrack/')
library(jsonlite)
library(tidyverse)
theme_set(theme_bw())
library(lubridate)
url = 'https://api.covid19tracker.ca/reports?fill_dates=true'
upd = fromJSON(url)
dat_can = upd$data
# head(dat_can)
# tail(dat_can)
summary(dat_can)
```

```
##      date            change_cases  change_fatalities  change_tests   
##  Length:435         Min.   :   1   Min.   :  0.00    Min.   : -5768  
##  Class :character   1st Qu.: 457   1st Qu.:  8.00    1st Qu.: 28952  
##  Mode  :character   Median :1536   Median : 34.00    Median : 58903  
##                     Mean   :2441   Mean   : 55.77    Mean   : 65306  
##                     3rd Qu.:3922   3rd Qu.: 96.00    3rd Qu.: 99710  
##                     Max.   :9481   Max.   :228.00    Max.   :393941  
##                     NA's   :22     NA's   :22                        
##  change_hospitalizations change_criticals  change_recoveries
##  Min.   :-218.00         Min.   :-76.000   Min.   :    0.0  
##  1st Qu.: -19.00         1st Qu.: -4.000   1st Qu.:  345.5  
##  Median :   0.00         Median :  0.000   Median :  847.0  
##  Mean   :   5.97         Mean   :  1.841   Mean   : 2133.5  
##  3rd Qu.:  28.00         3rd Qu.:  6.500   3rd Qu.: 3210.5  
##  Max.   : 467.00         Max.   :188.000   Max.   :23999.0  
##                                                             
##  change_vaccinations change_vaccinated  change_vaccines_distributed
##  Min.   :     0.0    Min.   :-32257.0   Min.   :     0             
##  1st Qu.:     0.0    1st Qu.:   455.5   1st Qu.:     0             
##  Median :     0.0    Median :  4818.0   Median :  3900             
##  Mean   : 14386.2    Mean   :  6339.3   Mean   : 70571             
##  3rd Qu.:   361.5    3rd Qu.: 10145.0   3rd Qu.: 50575             
##  Max.   :219496.0    Max.   : 32068.0   Max.   :817830             
##                      NA's   :324        NA's   :315                
##   total_cases      total_fatalities  total_tests       total_hospitalizations
##  Min.   :      1   Min.   :    0    Min.   :       2   Min.   :   0.0        
##  1st Qu.:  72844   1st Qu.: 5344    1st Qu.: 1290244   1st Qu.: 328.5        
##  Median : 129701   Median : 9151    Median : 6327940   Median :1370.0        
##  Mean   : 287298   Mean   : 9829    Mean   : 9358803   Mean   :1602.6        
##  3rd Qu.: 482466   3rd Qu.:13772    3rd Qu.:16682481   3rd Qu.:2524.0        
##  Max.   :1008144   Max.   :23031    Max.   :28408246   Max.   :4879.0        
##                                                                              
##  total_criticals total_recoveries total_vaccinations total_vaccinated
##  Min.   :  0.0   Min.   :     0   Min.   :      0    Min.   :  2173  
##  1st Qu.: 69.5   1st Qu.: 34600   1st Qu.:      0    1st Qu.:105338  
##  Median :249.0   Median :115054   Median :      0    Median :387694  
##  Mean   :329.4   Mean   :247999   Mean   : 415034    Mean   :356598  
##  3rd Qu.:559.5   3rd Qu.:394286   3rd Qu.:   1873    3rd Qu.:592002  
##  Max.   :900.0   Max.   :928081   Max.   :6258003    Max.   :703660  
##                                                      NA's   :348     
##  total_vaccines_distributed
##  Min.   :   3900           
##  1st Qu.: 548955           
##  Median :1253140           
##  Mean   :2084313           
##  3rd Qu.:2933320           
##  Max.   :8468570           
##  NA's   :323
```

```r
dat_can$date = ymd(dat_can$date)
# summary(dat_can)

dat_vax = (dat_can
		   %>% filter(!is.na(total_vaccines_distributed))
		   %>% select(date,change_vaccinations,
		   		   change_vaccines_distributed,total_vaccinations,
		   		   total_vaccines_distributed))
# dim(dat_vax)
# head(dat_vax)
```

Function definitions:


```r
# This is the least graceful way I could have done this, but it works.
avg_wait = function(dat_vax){
	pos = 1
	dist = 0
	vaxxed = 0
	i = 0
	counted = 0
	avg_df = data.frame(Time = NA, Doses = NA, .vaxxed = NA,
						.counted = NA, .leftover = NA,
						.dist = NA, .pos = NA)
	for (i in 1:nrow(dat_vax)){
		leftover = dist - vaxxed
		dist = dat_vax[pos,'total_vaccines_distributed']
		vaxxed = dat_vax[i,'total_vaccinations']
		new_vaxxed = dat_vax[i,'change_vaccinations']
		if (vaxxed <= dist){
			timediff = dat_vax[i, 'date'] - dat_vax[pos, 'date']
			avg_df = rbind(avg_df, data.frame(Time = timediff, 
											  Doses = new_vaxxed,
											  .vaxxed = vaxxed, 
											  .counted = counted,
											  .leftover = leftover,
											  .dist = dist, .pos = pos))
			counted = counted + new_vaxxed
		} else {
			timediff = dat_vax[i, 'date'] - dat_vax[pos, 'date']
			avg_df = rbind(avg_df, data.frame(Time = timediff, Doses = leftover,
											  .vaxxed = vaxxed, 
											  .counted = counted,
											  .leftover = leftover,
											  .dist = dist, .pos = pos))
			counted = counted + leftover
			new_vaxxed = new_vaxxed - leftover
			pos = pos + 1
			dist = dat_vax[pos, 'total_vaccines_distributed']
			while (dist < vaxxed){
				avail = dist - counted
				timediff = dat_vax[i, 'date'] - dat_vax[pos, 'date']
				avg_df = rbind(avg_df, data.frame(Time = timediff,
												  Doses = avail,
											  .vaxxed = vaxxed, 
											  .counted = counted,
											  .leftover = leftover,
											  .dist = dist, .pos = pos))
				counted = counted + avail
				new_vaxxed = new_vaxxed - avail
				pos = pos + 1
				dist = dat_vax[pos, 'total_vaccines_distributed']
				if (pos > i){
					stop('pos > i')
				}
			}
			timediff = dat_vax[i, 'date'] - dat_vax[pos, 'date']
			avg_df = rbind(avg_df, data.frame(Time = timediff, Doses = new_vaxxed,
											  .vaxxed = vaxxed, 
											  .counted = counted,
											  .leftover = leftover,
											  .dist = dist, .pos = pos))
			counted = counted + new_vaxxed
		}
	}
	avg_df = na.omit(avg_df)
	return(avg_df)
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

get_wait = function(df){
	wait = sum(df$Time*df$Doses)/sum(df$Doses)
}
```

</details>

## Vaccine Gap

<details><summary>code</summary>

```r
dist_tot = dat_vax[nrow(dat_vax),'total_vaccines_distributed']
admn_tot = dat_vax[nrow(dat_vax),'total_vaccinations']
msg = paste('So far Canada has distributed ', 
			format(dist_tot, nsmall = 1, big.mark = ','), 
			' vaccine doses, and ',
			format(admn_tot, nsmall = 1, big.mark = ','), 
			' of those have been administered.', sep = '')
msg1 = paste('This is a cumulative administration rate of ', 
			100*round(admn_tot/dist_tot, 4), '%', sep = '')
```
</details>


```
## [1] "So far Canada has distributed 8,468,570 vaccine doses, and 6,258,003 of those have been administered."
```

```
## [1] "This is a cumulative administration rate of 73.9%"
```

<details><summary>code</summary>

```r
# head(dat_vax)
dat_tot_long = (dat_vax
				%>% select(-change_vaccinations, 
						   -change_vaccines_distributed)
				%>% gather(Variable, Count, -date)
				%>% mutate(Variable = factor(Variable, 
											 levels = c('total_vaccines_distributed',
											 		   'total_vaccinations'))))
# head(dat_tot_long)
abs_adm = ggplot(dat_tot_long, aes(date, Count, fill = Variable)) +
	geom_area(alpha = 0.5, position = 'identity') +
	scale_fill_manual(values = c('sienna4', 'sienna1'), 
					  labels = c('doses distributed','doses administered'),
					  name = NULL) +
	xlab('Date') +
	ggtitle('Total cumulative doses distributed\nand administered over time')
abs_adm
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

```r
dat_vax = (dat_vax
		   %>% mutate(prop_dist = 
		   		   	total_vaccinations/total_vaccines_distributed,
		   		   prop_7 = roll_avg(prop_dist, 7)))
prop_adm = ggplot(dat_vax, aes(date, prop_dist)) +
	geom_line() +
	xlab('Date') +
	ylab('Proportion Administered') +
	ggtitle('Instantaneous proportion of delivered doses\nthat have been administered')

prop_adm_7 = ggplot(dat_vax, aes(date, prop_7)) +
	geom_line() +
	xlab('Date') +
	ylab('Proportion Administered') +
	ggtitle('Rolling 7-day average of proportion of delivered doses\nthat have been administered')
```
</details>

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)
![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-2.png)


## Wait Times

<details><summary>code</summary>

```r
wt_df = avg_wait(dat_vax)
wait = get_wait(wt_df)
msg = paste('Vaccine doses have waited an average of', round(wait,1), 'days between being distributed and being administered.')
```
</details>


```
## [1] "Vaccine doses have waited an average of 7.4 days between being distributed and being administered."
```

<details><summary>code</summary>

```r
wait_df = data.frame(Wait = NA)

for(i in 2:nrow(dat_vax)){
	wait_df = rbind(wait_df, 
					data.frame(Wait = get_wait(avg_wait(dat_vax[1:i,]))))
}
wait_df = na.omit(wait_df)
# head(wait_df)
wait_df$Date = dat_vax[-1,'date']

wait_plt = ggplot(wait_df, aes(Date, Wait)) +
	geom_line() +
	ylab('Wait Time (Days)') +
	ggtitle('Cumulative average time it takes a vaccine dose\nto go from distribution to administration\nin Canada')
```
</details>

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png)

<details><summary>code</summary>

```r
n = nrow(wt_df)
dist = wt_df[n,'.dist']
vaxxed = wt_df[n,'.vaxxed']
leftover = dist - vaxxed
pos = wt_df[n,'.pos']
timediff = today() - dat_vax[pos, 'date']

avg_wt = data.frame(Time = timediff, Doses = leftover)
# avg_wt

for (i in (pos+1):nrow(dat_vax)){
	i = i + 1
	i
	dat_vax[i,]
	timediff = today() - dat_vax[i, 'date']
	timediff
	avg_wt = rbind(avg_wt,
				   data.frame(Time = timediff,
				   		   Doses = dat_vax[i, 'change_vaccines_distributed']))
	avg_wt
}

avg_wt = na.omit(avg_wt)

new_wt = sum(avg_wt$Time * avg_wt$Doses)/sum(avg_wt$Doses)

msg = paste('Current unadministerd doses have been waiting an average of',
			round(new_wt,1), 'days.')
```
</details>


```
## [1] "Current unadministerd doses have been waiting an average of 1.9 days."
```

---

<details><summary>code</summary>

```r
msg = paste('This website was last updated', today())
msg1 = paste('The most recent data are from', max(dat_vax$date))
```
</details>


```
## [1] "This website was last updated 2021-04-03"
```

```
## [1] "The most recent data are from 2021-04-03"
```

