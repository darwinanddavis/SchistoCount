# Schistosome counter 
## Script for pulling counts from objects in .jpg images 

The script counts objects in a standard, flattened image file (.jpg, .png, etc) with no metadata by separating the gamma layers of the file so that R can treat objects as a separate layer to the background, then count the number of those objects.    

Files:  
.R
