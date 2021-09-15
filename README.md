# T-maze-drosophila
This is a script for plotting the results from T-maze experiments in Drosophila melanogaster tested twice

For running the results from the T-maze with red light illumination you just need to:

	1. Open the file Tmaze_red_evaluation.R in R Studio.
	
	2. Click on the Source button in R Studio to run the file.
	
	3. It will display a prompt where you can look for the file to open. Within the repository there is a csv file 		to test the script with the path ./data/Tmaze_screen.csv. In case you want to use your own csv please use the 		same format as shown for this template or follow the instructions below.
	It just needs as input a csv file with a with the data ordered in this way:
		- 1st column with the name Fly.line
		- 2nd, 3rd and 4th columns with the number of flies in the tube illuminated by red, in the elevator 			and in the dark tube respectively
		- 5th to 7th columns with the repetition of the experiments of the same group of flies in the same 			order (red, elevator, dark).
	The name of the columns is only relevant in the case of Fly.line.

	4. A message with "Enter the name of the output file" will prompt in the command line in R Studio: enter the 		name for the pdf file that is going to contain all the graphs.
	5. Look in your current directory for the pdf file (if you do not know where, you can type getwd() in the 		command line).

The pdf will contain boxplots for the PIs and weighted PI calculations for:
	-Both experiments together
	-First test
	-Second test
