#ROCK PAPER SCISSOR GAME
cus_name <- readline("What would you like us to call you?")
print(paste("hello",cus_name))
print("Welcome to PAO YING SHUP GAME")

# rule detail
print("--------------------")
print("Let's start the game rule")
print("We have >> Rock, Paper, Scissors ")
print("Rock beats Scissors")
print("Scissors beats Paper")
print("Paper beats Rock")
print("If both players throw the same hand signal, it is considered a tie")

sign <- c("R","P","S")
player_win <- 0
player_lose <- 0
player_tie <- 0
i <- 1

# game start
print("--------------------")
print("Let's start the game")
print("Rock = R, Scissors = S, Paper = P or Q to quit the game")

player <- "Z"
while(player != "Q"){
  print(paste("Round",i))
  player <- toupper(readline("your sign is:"))
  bot <- sample(sign,1)
  print(paste("Bot sign is:", bot))
  i=i+1
  if(player==bot){
    player_tie <- player_tie +1
    print("IT's tie")
  }
  else if (player == "R"){
    if(bot == "S") {
      player_win <- player_win+1
      print("you win")
    } 
    else {
      player_lose <- player_lose+1
      print("you lose")
    } 
  }
  else if (player == "P"){
    if(bot == "R") {
      player_win <- player_win+1
      print("you win")
    } 
    else {
      player_lose <- player_lose+1 
      print("you lose")
    } 
  }
  else if (player == "S"){
    if(bot == "P") {
      player_win <- player_win+1
      print("you win")
    } 
    else{
      player_lose <- player_lose+1
      print("you lose")
    } 
  }
  else print("Try again")
  
}

# endding
print("--------------------")
print("thank you for join the game")
print(paste("you win:",player_win,"times"))
print(paste("you lose:",player_lose,"times"))
print(paste("you tie:",player_tie,"times"))








