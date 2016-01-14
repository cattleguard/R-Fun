library(markovchain)

# Playing Rock, Paper, Scissors, Lizard, Spock with markov chains.

# The rules to Rock, Paper, Scissors, Spock (V), Lizard
#     https://en.wikipedia.org/wiki/Rock-paper-scissors

state.names = c("R", "P", "S", "V", "L")
sequence <- vector(mode = 'character') 
bot.wins <- vector(mode = 'integer') 

SheldorsPlay <- function(sequence)
{
  play <- vector(mode = 'character')
  if (length(sequence) < 1)
  {
    play <- sample(state.names, 1)
  }
  else
  {
    mcFitter <- markovchainFit(data = sequence)
    #play <- predict(mcFitter$estimate,newdata=tail(sequence,n=1),n.ahead=1)
    tryCatch({
    play <- markovchainSequence(n = 1, markovchain = mcFitter$estimate, t0 = tail(sequence, n = 1))
    },
    error = function (condition) {
      print(paste("  Couldn't locate transition:",conditionMessage(condition)))
    }
  )
    # If we didn't have a way to estimate a transition, we'll pick a play at random.
    if(length(play) == 0)
    {
      play <- sample(state.names, 1)
    }
  }
  
  if (play == "R")
    response <- c("P", "V")
  else if (play == "P")
    response <- c("S", "L")
  else if (play == "S")
    response <- c("V", "R")
  else if (play == "V")
    response <- c("L", "P")
  else if (play == "L")
    response <- c("S", "R")
  
  return(sample(response, 1))
}

TranslatePlay <- function(play)
{
  numero <- 0
  if (play == "R")
    numero <- 1
  else if (play == "P")
    numero <- 2
  else if (play == "S")
    numero <- 3
  else if (play == "V")
    numero <- 4
  else if (play == "L")
    numero <- 5
  
  return(numero)
}

DetermineWinner <- function(sheldor, player)
{
  shelnum <- TranslatePlay(sheldor)
  playnum <- TranslatePlay(player)
  
  calc.win <- (playnum - shelnum)%%5
  
  if(calc.win == 1 | calc.win == 3)
    sheldon.wins <- 0
  else if (calc.win == 2 | calc.win == 4)
    sheldon.wins <- 1
  else
    sheldon.wins <- 2  # Tie.
  
  return(sheldon.wins)  
}

# Exit when a valid selection is not made.
while(1){
  userinput <- toupper(readline("Sheldor challenges you to a duel. 'R','P','S','V','L' :"))
  userinput <- ifelse(grepl("[RPSLV]", userinput), userinput, -1)
  if(userinput < 1){break}
  
  sheldor <- SheldorsPlay(sequence)
  print(paste("Sheldor played", sheldor))
  sequence <- append(sequence, userinput)
  winner <- DetermineWinner(sheldor, userinput)
  
  if (winner != 2)
    bot.wins <- append(bot.wins, winner)
  
  if (winner == 1)
    print("Sheldor wins!")
  else if (winner == 2)
    print("Tied")
  else
    print("You win!")
  
  print(paste("Sheldor's win percentage: ", mean(bot.wins)))
}




