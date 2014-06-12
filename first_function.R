# This function converts degree.minutes.seconds to degree.minutes be it latitude or longitude
conversion <- function (x)
{
	deg.val. <- as.numeric(unlist(strsplit(x, split = ".", fixed = TRUE)))
	deg. <- deg.val.[1]
	mins <- deg.val.[2]/60
	sec. <- (as.numeric(paste(deg.val.[3], deg.val.[4], sep = ".")))/3600
	return(deg. + mins + sec.)
}
