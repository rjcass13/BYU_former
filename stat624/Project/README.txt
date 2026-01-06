The code is broken into 2 files: 

functions.R - This contains all the functions used in the simulation, and should not need to be accessed in order to execute the simulation (obviously open for review, but nothing should need to be edited for it to function)

base.R - This is the file containing the actual exectuable code. It sources the functions script which also contains the necessary libraries (although I do source a package in this script to generate .gifs as I didn't want to try to make that a function). I have a few example scripts in there that should be able to be run as is. 

The 'write_up' directory contains all documents for generating the slides and the report. In particular, there are subdirectories which contain all images used in each, as well as the .bib file. 
