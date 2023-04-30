# This function converts degree.minutes.seconds to degree.minutes be it latitude or longitude
conversion <- function (x)
{
	deg.val. <- as.numeric(unlist(strsplit(x, split = ".", fixed = TRUE)))
	deg. <- deg.val.[1]
	mins <- deg.val.[2]/60
	sec. <- (as.numeric(paste(deg.val.[3], deg.val.[4], sep = ".")))/3600
	return(deg. + mins + sec.)
}


# Outlier deletion

out.del <- function(x){
  lo.bound <- quantile(x)[2] - (IQR(x) * 1.5)
  up.bound <- quantile(x)[4] + (IQR(x) * 1.5)
  x <- x[x >= lo.bound & x <= up.bound]
  return(x)
}

# Matching unique values in two data frames

discrepancy <- function(x, y, xfield, yfield) 
{ 
	if (!class(x) == "data.frame" & !class(y) == "data.frame"){
		stop("Please provide data frame as input")
	}
	check.1way <- (x[, xfield] %in% y[, yfield])
	check.2way <- (y[, yfield] %in% x[, xfield])
	if (sum(check.1way) == sum(check.2way)) {
		return("NO Discrepancy!! Go Ahead!!")
	} else {
		return("Whoa! there is discrepancy. Look for culprit duplicate values!!!")
	}
}

# A useful function to find values 
find.it <- function(x, y, d = d)
{
    x.find<-agrep(x, y, ignore.case = TRUE, value = TRUE, max.distance = d)
    return(x.find)
}

# remove cells with no values
# field should have character class
rm.novalue <- function(x, field)
{
	x <- x[!x[, field] == "",]
	return(x)
}


# This function provides fuzzy matching of unique values.
# Sometimes it is possible the a data entery operator writes same name with two different spellings, this function will be helpful
# in such cases but might require bit modifictions.

get.fvalues <- function(x, y, z, xfield, yfield, zfield, d = 0.1)
{	
	if (class(x) == "SpatialPointsDataFrame"){
		x <- x@data
	} else if (class(y) == "SpatialPointsDataFrame"){
		y <- y@data
	} else if (class(z) == "SpatialPointsDataFrame"){
		z <- z@data
	}
	if (any(x[, xfield] == "")){
		x <- rm.novalue(x, xfield)
	}
	library(gtools)
	dat.table <- smartbind(data.frame(a = x[, xfield][1]),
						   data.frame(a.y = find.it(x[, xfield][1], y[, yfield], d = d)),
						   data.frame(a.z = find.it(x[, xfield][1], z[, zfield], d = d)))
	for (i in 2:length(x[, xfield])){
		dat.table <- rbind(dat.table, smartbind(data.frame(a = x[, xfield][i]),
					   data.frame(a.y = find.it(x[, xfield][i], y[, yfield], d = d)),
					   data.frame(a.z = find.it(x[, xfield][i], z[, zfield], d = d))))
	}
	names(dat.table)[1] <- deparse(substitute(x))
	names(dat.table)[2] <- deparse(substitute(y))
	names(dat.table)[3] <- deparse(substitute(z))
#print(length(names(dat.table)))
	return(dat.table)
}
