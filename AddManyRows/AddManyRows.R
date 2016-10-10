# CustomAddRows.R - A sample R script to add the rows from two datasets, with a swap parameter that determines the order in which the datasets are appended.
# © 2015 Microsoft Corporation. All rights reserved. 
# Sample scripts in this guide are not supported under any Microsoft standard support program or service. 
# The sample scripts are provided AS IS without warranty of any kind. 
# Microsoft disclaims all implied warranties including, without limitation, any implied warranties of merchantability or of fitness for a particular purpose. 
# The entire risk arising out of the use or performance of the sample scripts and documentation remains with you. 
# In no event shall Microsoft, its authors, or anyone else involved in the creation, production, or delivery of the scripts be liable for any damages whatsoever (including, without limitation, damages for loss of business profits, business interruption, loss of business information, or other pecuniary loss) arising out of the use of or inability to use the sample scripts or documentation, even if Microsoft has been advised of the possibility of such damages.

AddManyRows <- function(dataset1=NULL, dataset2=NULL, dataset3=NULL, dataset4=NULL, dataset5=NULL) 
{ 
  return (rbind(dataset1, dataset2, dataset3, dataset4, dataset5)); 
} 
