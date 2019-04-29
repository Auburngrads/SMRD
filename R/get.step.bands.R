get.step.bands <-
function(cdfest.out, band.type, conf.level = GetSMRDDefault("SMRD.ConfLevel")/100., a.limit = 0.001, b.limit
	 = 0.999, mono.tran = T)
{
	#
	#for cdfplots, these are points and segments
	#
	#get indices of non zero sd's
	#
	#
	#now allowing lower = upper = fhat when sd=0 (because p=0 or 1)
	#
	#	fhat <- cdfest.out$prob[ok]
	if(is.null(cdfest.out$sd)) return(warning("No standard errors available from cdfest"))
	ok <- cdfest.out$sd > 0.
	fhat <- cdfest.out$prob
	lower <- fhat
	upper <- fhat
	at.point <- (cdfest.out$p == cdfest.out$q) & ok
	over.interval <- !(cdfest.out$p == cdfest.out$q) & ok
	timesp <- cdfest.out$p[ok]
	timesq <- cdfest.out$q[ok]
	nux.squared <- cdfest.out$number.observations * (cdfest.out$sd[ok]/fhat[ok])^2.
	kx <- nux.squared/(1. + nux.squared)
	zvalue <- 0.
	if(is.null(band.type) || band.type == "")
		band.type <- "none"
	switch(casefold(band.type),
		s = ,
		simultaneous = {
			band.type <- "Simultaneous"
			bands.over <- kx > a.limit & kx < b.limit
			zvalue <- evalue(a = a.limit, b = b.limit, conf.level = conf.level)
		}
		,
		"Point-wise" = ,
		pointwise = ,
		p = ,
		"point-wise" = {
			band.type <- "Pointwise"
			bands.over <- kx > 0. & kx < 1.
			zvalue <- qnorm(1. - (1. - conf.level)/2.)
		}
		
		,
		none = {
			band.type <- "none"
		}
		,
		{
			warning("band.type not recognized")
			band.type <- "none"
		}
		)
	if(band.type == "none") {
		return(list(cdfest.out = cdfest.out, cdfest.out, lower = NULL, upper = NULL, at.point = NULL,
			over.interval = NULL, band.type))
	}
	#	lower <- lower[ok]
	#	upper <- upper[ok]
	if(any(at.point)) {
		dist.probs <- cdfest.out$prob[at.point]
		stderrq <- cdfest.out$sd[at.point]/(dist.probs * (1. - dist.probs))
		lower[at.point] <- plogis(qlogis(dist.probs) - zvalue * stderrq)
		upper[at.point] <- plogis(qlogis(dist.probs) + zvalue * stderrq)
	}
	if(any(over.interval)) {
		dist.probs <- cdfest.out$prob[over.interval]
		stderrq <- cdfest.out$sd[over.interval]/(dist.probs * (1. - dist.probs))
		lower[over.interval] <- plogis(qlogis(dist.probs) - zvalue * stderrq)
		upper[over.interval] <- plogis(qlogis(dist.probs) + zvalue * stderrq)
	}
	if(mono.tran) {
		if(any(!is.na(lower)))
			lower[!is.na(lower)] <- mono.lower(lower[!is.na(lower)])
		if(any(!is.na(upper)))
			upper[!is.na(upper)] <- mono.upper(upper[!is.na(upper)])
	}
	return(list(cdfest.out = cdfest.out, fhat = fhat, lower = lower, upper = upper, at.point = at.point,
		over.interval = over.interval, band.type = band.type))
}
