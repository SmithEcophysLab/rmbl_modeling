# calculate optimal vcmax as in Smith et al. Photosynthetic capacity is optimized to the 
# environment. Ecology Letters. 

########################
## variable key
########################

# tg_c: acclimated temperature (degC)
# z: elevation (m)
# vpdo: vapor pressure deficit at sea level (kPa)
# cao: atmospheric CO2 at sea level (umol mol-1)
# paro: photosynthetically active radiation at sea level (µmol m-2 s-1)
# q0: quantum efficiency of photosynthetic electron transport (mol/mol)
# theta: curvature of the light response of electron transport (unitless)
# R: universal gas constant (J mol-1 K-1)
# patm: atmospheric pressure (Pa)
# ca: atmospheric CO2 at z (Pa)
# km: Michaelis-Menten constant for Rubisco (Pa)
# gammastar: CO2 compensation point (Pa)
# chi: leaf intercellular to atmospheric CO2 ratio (ci/ca) (unitless)
# vpd: vapor pressure deficit at z (kPa)
# par: photosynthetically active radiation at z (µmol m-2 s-1)
# ci: leaf intercellular CO2 concentation (Pa)
# m: CO2 limiation of electron transport rate limited photosynthesis (Pa)
# mc: CO2 limiation of Rubisco carboxylation rate limited photosynthesis (Pa)
# c: constant describing cost of maintaining electron transport (unitless)
# omega: omega term from Smith et al.
# omega_star: omega_star term from Smith et al.
# vcmax_star: maximum rate of Rubisco carboxylation without temperature correction (µmol m-2 s-1) 
# vcmax_prime: optimal maximum rate of Rubisco carboxylation at tg (µmol m-2 s-1)
# jvrat: ratio of the maximum rate of electron transport to the maximum rate of Rubisco carbocylation at tg (unitless)
# jmax_prime: optimal maximum rate of electron transport at tg (µmol m-2 s-1)

# All equation numbers refer to equations presented in Smith et al., 2019 

# libraries
# install.packages('R.utils')
library(R.utils)

# load necessary functions
sourceDirectory('functions')

calc_optimal_vcmax <- function(tg_c = 25, z = 0, vpdo = 1, cao = 400, paro = 800, q0 = 0.257, theta = 0.85,
                               f = 0.5){
	
	# constants
	R <- 8.314
	c <- 0.05336251

	# environmental terms
	patm <- calc_patm(z)
	par <- paro ## not using calc function based on elevation
	vpd <- vpdo ## not using calc function based on elevation
	ca <- cao * 1e-6 * patm

	# K and Gamma* model terms
  km <- calc_km_pa(tg_c, z)                
	gammastar <- calc_gammastar_pa(tg_c, z)   

	# Coordination and least-cost hypothesis model terms
	chi <- calc_chi(tg_c, z, vpdo, cao)              
	ci <- chi * ca # Pa
	mc <- ((ci - gammastar) / (ci + km))             
	m <- ((ci - gammastar)/(ci + (2 * gammastar)))   
	omega <- calc_omega(theta = theta, c = c, m = m) 
	omega_star <- (1 + (omega) - sqrt((1 + (omega))^2 - (4 * theta * omega))) 
	
	# calculate q0 using Bernacchi et al. (2003) temperature response (set to 0.257 at 25C)
	# q0 = -0.0805 + (0.022 * tg_c) - (0.00034 * tg_c * tg_c)
	q0 = 0.25 # Bernacchi only goes down to 10C and these are much colder >> probably shouldnt use!
	
	# calculate vcmax and jmax	
	vcmax <- ((q0 * par * m) / mc) * (omega_star / (8 * theta))	          
	jvrat <- ((8 * theta * mc * omega) / (m * omega_star))     
	jmax <- jvrat * vcmax
	
	vcmax25 <- vcmax / calc_tresp_mult(tg_c, tg_c, 25)
	vpmax25 <- 0
	jmax25 <- jmax / calc_jmax_tresp_mult(tg_c, tg_c, 25)
	
	# estimate LMA
	lma <- calc_lma(f = f, par = par*0.0864*0.1, # convert to absorbed mol m-2 day-1 assuming 10% absorbance
	                temperature = tg_c, vpd_kpa = vpd, z = z, co2 = ca)
	
	# calculate gross photosynthesis
	grossphoto <- vcmax * mc
	
	# calculate respiration
	resp <- 0.15 * vcmax
	
	# calculate net photosynthesis
	netphoto <- grossphoto - resp
	
	# calculate leaf N in rubisco from predicted vcmax
	nrubisco <- fvcmax25_nrubisco(vcmax25)
	
	# calculate leaf N in bioenergetics from predicted jmax
  nbioe <- fjmax25_nbioe(jmax25)
	
	# calculate leaf N in rubisco from predicted vpmax with PEP-specific constants
	npep <- fvpmax25_npep(vpmax25)
	
	# calculate nitrogen in structural tissue from lma 
	nstructure <- flma_nstructure(lma)
	
	# sum all leaf N predictions
	nall <- nrubisco + nbioe + nstructure + npep
	
	# calculate leaf N used for photosynthesis
	nphoto <- nrubisco + nbioe + npep
	
	# calculate the fraction of leaf N in rubisco out of all leaf N
	nrubisco_frac <- nrubisco / nall
	
	# calculate the fraction of leaf N for photosynthesis out of all leaf N
	nphoto_frac <- nphoto / nall

	# output
	results <- as.data.frame(cbind(tg_c, z, vpdo, cao, paro, q0, theta, c, par, patm, ca, vpd, chi, ci, km, 
	                               gammastar, omega, m, mc, omega_star, vcmax, jvrat, jmax, 
	                               vcmax25, jmax25,
	                               lma,
	                               grossphoto, resp, netphoto, nrubisco, nbioe, npep, nstructure,
	                               nall, nphoto, nrubisco_frac, nphoto_frac))
	
	colnames(results) <- c('tg_c', 'z', 'vpdo', 'cao', 'paro', 'q0', 'theta', 'c', 'par', 'patm', 'ca', 'vpd', 'chi', 'ci', 'km', 
	                       'gammastar', 'omega', 'm', 'mc', 'omega_star', 'vcmax', 'jvrat', 'jmax', 
	                       'vcmax25', 'jmax25',
	                       'lma',
	                       'grossphoto', 'resp', 'netphoto', 'nrubisco', 'nbioe', 'npep', 'nstructure',
	                       'nall', 'nphoto', 'nrubisco_frac', 'nphoto_frac')
	
	results	
	
}

