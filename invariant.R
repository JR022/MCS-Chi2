invariant = function(matrix, initial, N) {
  print("Starting.")
  start_time <- Sys.time()
  
  #Computes the proportion of time spent in each state.  Requires the transition matrix, initial state, and the number of steps 'N'.
  visits <- c(0,0,0) #Stores the number of times each state is visited.
  state <- initial
  for (trial in 1:N) {
    visits[state] <- visits[state] + 1 #Increment the number of visits array.  Performing this at the start of the loop simplifies the code.
    val <- runif(1) #Generate a random number in Unif[0,1] to decide state to enter.
    #print(val)
    #The line [0,1] can be partitioned into [0,p_1], [p_1,p_1+p_2], [p_1+p_2,1].  These strips have width p_1, p_2, 1-(p_1+p_2)=p_3 respectively.
    if (val < matrix[state,1]) {
      #Move to state 1.
      state <- 1
      #print("Moving to 1.")
      next
    }
    if (val > matrix[state,1]+matrix[state,2]) {
      #Move to state 3.
      state <- 3
      #print("Moving to 3.")
      next
    }
    # Here, p_1 < val < p_1+p_2 must be true - move to state 2.
    state <- 2
    #print("Moving to 2.")
    next
  }
  visits[state] <- visits[state] + 1 #Needed to add the last visit.
  print(sprintf("Duration %f seconds.",difftime(Sys.time(), start_time, units = "secs")))
  return(visits/N) #Calculate the proportions by dividing the number of visits by the total number of trials.
}