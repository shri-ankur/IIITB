 
#	Import the Bollywood data set in Rstudio in a variable named bollywood


  bollywood <- read.csv("bollywood.csv")
  View(bollywood)


#	When you import a data set, R stores character vectors as factors (by default)
# You can check the structure of the data frame by using str()
  str(bollywood)

# You can change the attribute 'Movie' from factor to character type using the given command
  bollywood$Movie <- as.character(bollywood$Movie)
	 

#Q1.
#	Access the last 10 movies (from the bottom of the Bollywood data frame) using column bollywood$Movie
# Store the names of those movies in last_10 vector (in the same order)

# tail function with n=10 will return the last 10 on column movie
# would give the names the last 10 movies in the dataset.  
	last_10 <- tail(bollywood$Movie, 10)
	 
	  
#Q2.
#	Find out the total number of  missing values (NA) in the bollywood data frame.
# Store the result in na_bollywood vector
 
#Applying is.na on bollywod and summing gives the total number
#of missing values in bollywood dataframe
	na_bollywood <- sum(is.na(bollywood))

	#Q3
#	Write the command to find out which movie tops the list in terms of Total Collections
# Store the movie name in variable named top_movie
 
#Applying which.max function Tcollection column gives the row number
#of the movie with the highest Total Collection, Subsetting on the row number and
#Movie column gives the name of the top movie. 	
  top_movie <- bollywood$Movie[which.max(bollywood$Tcollection)]
  
  
#Q4
#	Write the command to find out which movie comes second on the list in terms of Total Collections
# Store the movie name in variable named top_2_movie

# Use order function to arrange bollywood dataset in reverse order of
# TCollection, then subset on the Movie column and 2nd row.  
  top_2_movie <- bollywood[with(bollywood, order(-Tcollection)),]$Movie[2]
	  
	
# Now let's find out the movies shot by Shahrukh, Akshay and Amitabh separately.
# subset() function is used for that. The code has already been written for you. 
	
	shahrukh <- subset(bollywood, Lead == "Shahrukh")
	akshay <- subset(bollywood, Lead == "Akshay")
	amitabh <- subset(bollywood, Lead  == "Amitabh")

# You can view what the above data frames look like

		   
#Q5
#	What is the total collection of Shahrukh, Akshay and Amitabh movies individually?
# You can use	a column named 'Tcollection' for this
# Use sum function on each of the actor's Tcollection column.
 
  shahrukh_collection <- sum(shahrukh$Tcollection)
    
	akshay_collection <- sum(akshay$Tcollection)
    
	amitabh_collection <- sum(amitabh$Tcollection)
    
	
#Q6  
# Write command/s to find out how many movies are in Flop, Average, Hit and Superhit categories in the entire Bollywood data set.
# Summary command can be used to find out the number of movies in the various categories.
	summary(bollywood$Verdict)

   
#You can use SAPPLY function if you want to apply a function specific columns in a data frame 
#You can write a command to find the maximum value of Ocollection, Wcollection, Fwcollecion and Tcollection using sapply
  sapply(bollywood[,4:7], max, na.rm=TRUE) 

 
#Q7 
# Write a command to find the names of the movies which have the maximum Ocollection, Wcollection, Fwcollecion & Tcollection
# Store the names of 4 movies in same sequence in movie_result vector
# Use sapply to apply which.max function on 4th to 7th column of bollywood,
# then subset on these rows and the movie column.
  movie_result <- bollywood[sapply(bollywood[,4:7],which.max),]$Movie

	

   
    


    
    
    