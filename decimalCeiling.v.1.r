# This is a function for performing ceiling to a value at a particular number of decimal places
decimalCeiling <- function(func_value, func_level=0){ 
	# Frist step is to find out how many decimal places exist within this value
	func_value <- strsplit(as.character(func_value),'.',fixed=TRUE)[[1]]
	# If there are no values after the decimal we simply return it, otherwise we find the number
	# of places after the decimal, and then round as appropriate to the func_level
	if(length(func_value) > 1){
		# We split apart the second value into individual characters and make them into elements concatenated with the integers
		func_deciList <- c(as.numeric(func_value[1]),as.numeric(strsplit(func_value[2],"")[[1]]))
		# we now increase the level value by one since our vector includes one additional element
		func_level <- func_level + 1
		# If the user is trying to ceiling at a level below the current 
		# number of decimal values contained then we perform no operation!
		if(length(func_deciList) > func_level){
			# We check if there is anything to be rounded, then do so
			if(any(func_deciList[func_level : length(func_deciList)] > 0)){
				func_deciList[func_level] <- func_deciList[func_level] + 1
				if(func_deciList[func_level] >= 10){
					func_deciList[func_level] = func_deciList[func_level] - 10
					func_deciList[func_level - 1] = func_deciList[func_level - 1] +1
				}
			}
			# we now return the value ceilinged to our level
			return( as.numeric(paste(func_deciList[1],'.',paste(func_deciList[2:func_level],collapse=""),sep="")) )
		# If we're trying to round at a number of decimal places smaller than the number of decimal values that exist
		# then we simply return the value as given
		} else {
			return( as.numeric(paste(func_value,sep='',collapse='.')) )
		}
	# If there are noa decimal values then we simply return that first portion 	
	} else {
		return( as.numeric(func_value) )	
	}
}

