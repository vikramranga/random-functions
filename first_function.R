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
