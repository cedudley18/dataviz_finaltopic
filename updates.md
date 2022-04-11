## March 21

  Downloaded the dataset for Vegas odds for NHL season 2017-18 and started cleaning.
  
## March 28 beginning of class

  I set up my dataset so that I have a full dataset of all of the coefficients I will need to create geom_segments of lines from my models for the 2017-18 season. Still need to find a way to include both the dataset with the odds and the dataset with the coefficients.
  
## March 28 end of class

  I created a Shinyapp that allows you to select a team and shows the scatterplot of their predicted probability of winning a game throughout the 2017-18 season. 
  
## March 30 beginning of class

  I have a full shinyApp where you select a team and you see both a scatterplot and geom_segments of my linear model.
  
## March 30 end of class

  I have been cleaning up another season's data to get it ready to add to the shinyApp.
  
## April 4 beginning of class

  I added another year's data to the Shiny and you can now select a season and team.
  
## April 4 end of class

  I added another year's data to the Shiny and also created a table output that I will continue to edit.
  
# April 6 beginning of class

  I added another year's data to the Shiny and data file.
  
# April 6 end of class

  I added the final season of data and was able to clean up the errors with my Shiny. Now thinking about making my plot interactive.
  
<<<<<<< HEAD
# April 11 beginning of class

  I created a new variable that will allow us to see the opposing team for each game - for now it is just a color, but I am looking to make this interactive/reactive where you hover over a point and see the opposing team.

=======
MH: It's probably a little late for this to be worth it, but you should be able to do what you're doing with the year data sets a bit more programatically with `purrr`. Using `map()` from `purrr` would allow you to read in the year data sets at the same time, perform a set of functions on those data sets, and then combine them at the end.
>>>>>>> cbd95dae1417ab2a01ceb723f27a9e0ba8271846
  
  
