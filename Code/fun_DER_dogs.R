# This function estimates the DER and the percent of DER that
# is covered with the daily amount of tortillas fed to dogs

# The user defines dogs'
#	BW  --- body weight
#	wah --- number of tortillas fed
#	at  --- activity type
# according to his knowledge or sample

# Table with activity factors to multiply by RER in order to get DER from:
#	Thatcher, C., Hand, M. S., & Remillard, R. L. (2010). Small animalclinical nutrition:
#	an iterative process. In C. Thatcher, M. S. Hand, & R. L. Remillard (Eds.), Smallanimal
#	clinical nutrition (pp. 3-21). Topeka: Mark Morris Institute.
activity <- data.frame(cbind(
c("Maintenance", "Neutered adult", "Intact adult", "Inactive/obese prone", "Weight loss", "Critical care", "Weight gain", "Light work", "Moderate work", "Heavy work", "Gestation first 42 days", "Gestation last 21 days", "Lactation 1 puppy", "Lactation 2 puppies", "Lactation 3-4 puppies", "Lactation 5-6 puppies", "Lactation 7-8 puppies", "Lactation 9 puppies", "Growth weaning-4mo age", "Growth 4mo-adult size"),
c(1, 1.6, 1.8, 1.2, 1, 1, 1.2, 1.6, 2, 5, 1.8, 3, 3, 3.5, 4, 5, 5.5, 6, 3, 2)
))
names(activity) <- c("type","factor")
#activity$factor <- as.numeric(levels(activity$factor)[activity$factor])
activity$factor <- as.numeric(activity$factor)

# Kilocalories per gram of tortilla from:
#	USDA Branded Food Products Database Release October 2017 Software v.3.8.6.4 2017-10-02
#	Nutrient data for: 45135964, GONZALEZ, TORTILLAS DE MAIZ CORN TORTILLAS, UPC: 748703281103
#	according to these sources 1 tortilla (25.8 g) has 80 kcal/g

# Finally, the function
dogs_DER <- function(BW,wah,at){
	RER <- 70*(BW^0.75)
	daily_kcal <- 80*wah
	af <- activity$factor[activity$type==at]
	DER <- RER*af
	pDER_by_wah <- (daily_kcal*100)/DER
	return(cbind(DER, pDER_by_wah))
	}
