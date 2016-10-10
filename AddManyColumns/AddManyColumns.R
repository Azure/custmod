AddManyColumns <- function(dataset1=NULL, dataset2=NULL, dataset3=NULL, dataset4=NULL, dataset5=NULL)
{
	x = cbind(dataset1, dataset2);
	if (!is.null(dataset3)) x = cbind(x, dataset3)
	if (!is.null(dataset4)) x = cbind(x, dataset4)
	if (!is.null(dataset5)) x = cbind(x, dataset5)
	return (x)
}