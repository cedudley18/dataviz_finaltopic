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

# April 13 beginning of class

  I turned my shiny into a shiny Plotly where you can hover over the data points.
  
# April 18 beginning of class

  I have a good template for a shiny dashboard and fixed my plotly so you only have one variable labeled.
  
# April 18 end of class

  Added a caption to my table in shiny. Working to add another variable to my model and my app that shows  whether a team played back to back games. I want to eventually color the points that are the second game of a back to back series.

# April 20 beginning of class

  Have my new back to back variable in the shinyApp and the points are colored by this variable. Can isolate one color by clicking on it.
  
# April 20 end of class
  Added a theme, tried to make app look cleaner. Changed table to a data table and will continue to play around with that.
  
# April 25 beginning of class

  I started to add radio buttons where you can choose what variable you want colored on the scatterplot.
  
# April 25 end of class

  Still trying to figure out the second set of radio buttons, now using updateradiobuttons but still not able to get it to work yet.

# April 27 beginning of class

  I added this year's season of data (2021-2022 up until April 26) and got it in the Shiny.
  
# April 27 end of class

  I turned my table back into a regular table, set the color to just be one variable, and added an error message to be displayed when the user chooses a season and team combo that did not exist (for new teams, a season where they weren't formed yet)