#command to deploy app: deployApp("C:\\Users\\derek.funk\\Desktop\\MSDS\\Shiny\\yahtzee\\yahtzee")

library(shinyjs)
library(data.table)
library(shinythemes)
library(V8)
library(shinyWidgets)

ui <- fluidPage(
  useShinyjs(),
  # extendShinyjs(script = "sounds.js"),
  theme = shinytheme("paper"),
  titlePanel("Yahtzee"),
  fluidRow(
    #column 1: basics
    column(width = 3,
           # actionButton("beep", "Beep"),
           br(), br(), br(),
           #instructions
           uiOutput(outputId = "instructions"),
           #link to game rules
           uiOutput(outputId = "linkToGameRules"),
           br(), br(), br(),
           #forfeit game
           disabled(actionButton(
             inputId = "forfeitGame",
             label = "START NEW GAME (i.e. forfeit)",
             width = 300,
             icon = icon("frown-open"),
             class = "btn-danger"
           )),
           br(), br(), br(),
           #game number
           {textOutput(
             outputId = "gameNumber"
           )},
           #round number
           {textOutput(
             outputId = "roundNumber"
           )},
           #roll number
           {textOutput(
             outputId = "rollNumber"
           )},
           br(),br(),br(),br(),br(),
           print("Please email bugs to dfunk0923@gmail.com :)")
    ),
    #column 2: dice values and roll button
    column(width = 1,
           #die values
           
           {uiOutput(
             outputId = "die1Value"
           )},
           
           {uiOutput(
             outputId = "die2Value"
           )},
           
           {uiOutput(
             outputId = "die3Value"
           )},
           
           {uiOutput(
             outputId = "die4Value"
           )},
           
           {uiOutput(
             outputId = "die5Value"
           )},
           
           br(),
           
           #roll button
           actionButton(
             inputId = "nextRoll",
             label = "BEGIN PLAYING",
             width = 300,
             icon = icon("play-circle"),
             class = "btn-primary"
           )
    ),
    #column 3: dice locks
    column(width = 3,
           #lock dice
           # {disabled(radioButtons(
           #   inputId = "lockDie1",
           #   label = "Lock die 1",
           #   choices = c(
           #     "Keep Rolling Die 1" = FALSE,
           #     "Lock Die 1" = TRUE
           #   ),
           #   inline = TRUE
           # ))},
           switchInput(
             inputId = "lockDie1",
             label = "DIE 1",
             onLabel = HTML("Toggle to <br> Keep Rolling<br> Die 1"),
             offLabel = HTML("Toggle to <br> Stop Rolling <br> Die 1"),
             disabled = TRUE,
             inline = TRUE
           ),
           switchInput(
             inputId = "lockDie2",
             label = "DIE 2",
             onLabel = HTML("Toggle to <br> Keep Rolling<br> Die 2"),
             offLabel = HTML("Toggle to <br> Stop Rolling <br> Die 2"),
             disabled = TRUE,
             inline = TRUE
           ),
           switchInput(
             inputId = "lockDie3",
             label = "DIE 3",
             onLabel = HTML("Toggle to <br> Keep Rolling<br> Die 3"),
             offLabel = HTML("Toggle to <br> Stop Rolling <br> Die 3"),
             disabled = TRUE,
             inline = TRUE
           ),
           switchInput(
             inputId = "lockDie4",
             label = "DIE 4",
             onLabel = HTML("Toggle to <br> Keep Rolling<br> Die 4"),
             offLabel = HTML("Toggle to <br> Stop Rolling <br> Die 4"),
             disabled = TRUE,
             inline = TRUE
           ),
           switchInput(
             inputId = "lockDie5",
             label = "DIE 5",
             onLabel = HTML("Toggle to <br> Keep Rolling<br> Die 5"),
             offLabel = HTML("Toggle to <br> Stop Rolling <br> Die 5"),
             disabled = TRUE,
             inline = TRUE
           )
    ),
    #column 4: pick score buttons
    column(width = 1,
           disabled(actionButton(
             inputId = "pickOnes",
             label = "Ones",
             width = 135,
             class = "btn-success"
           )),
           disabled(actionButton(
             inputId = "pickTwos",
             label = "Twos",
             width = 135,
             class = "btn-success"
           )),
           disabled(actionButton(
             inputId = "pickThrees",
             label = "Threes",
             width = 135,
             class = "btn-success"
           )),
           disabled(actionButton(
             inputId = "pickFours",
             label = "Fours",
             width = 135,
             class = "btn-success"
           )),
           disabled(actionButton(
             inputId = "pickFives",
             label = "Fives",
             width = 135,
             class = "btn-success"
           )),
           disabled(actionButton(
             inputId = "pickSixes",
             label = "Sixes",
             width = 135,
             class = "btn-success"
           )),
           disabled(actionButton(
             inputId = "pick3OfAKind",
             label = "3 of a Kind",
             width = 135,
             class = "btn-success"
           )),
           disabled(actionButton(
             inputId = "pick4OfAKind",
             label = "4 of a Kind",
             width = 135,
             class = "btn-success"
           )),
           disabled(actionButton(
             inputId = "pickFullHouse",
             label = "Full House",
             width = 135,
             class = "btn-success"
           )),
           disabled(actionButton(
             inputId = "pickSmallStraight",
             label = "Small Straight",
             width = 135,
             class = "btn-success"
           )),
           disabled(actionButton(
             inputId = "pickLargeStraight",
             label = "Large Straight",
             width = 135,
             class = "btn-success"
           )),
           disabled(actionButton(
             inputId = "pickChance",
             label = "Chance",
             width = 135,
             class = "btn-success"
           )),
           disabled(actionButton(
             inputId = "pickYahtzee",
             label = "YAHTZEE",
             width = 135,
             class = "btn-success"
           ))  
    ),
    #column 5: score table
    column(width = 3, offset = 1,
           tableOutput(
             outputId = "pointsTable"
           )
    )
    # last column: hidden dev notes
    # ,
    # column(width = 3,
    #    verbatimTextOutput(outputId = "debugBox"),
    #    verbatimTextOutput(outputId = "devNotes")
    # )
  )
)

server <- function(input, output, session) {
  #initialize values
  counters <- reactiveValues(
    game = 1,
    round = 1,
    roll = 0,
    dieValues = c(0,0,0,0,0),
    pointsTable = data.frame(
      ones = 0,
      twos = 0,
      threes = 0,
      fours = 0,
      fives = 0,
      sixes = 0,
      upperSum = 0,
      bonus = 0,
      threeOfAKind = 0,
      fourOfAKind = 0,
      fullHouse = 0,
      smallStraight = 0,
      largeStraight = 0,
      chance = 0,
      yahtzee = 0,
      grandTotal = 0
    ),
    pointsUsed = data.frame(
      ones = FALSE,
      twos = FALSE,
      threes = FALSE,
      fours = FALSE,
      fives = FALSE,
      sixes = FALSE,
      threeOfAKind = FALSE,
      fourOfAKind = FALSE,
      fullHouse = FALSE,
      smallStraight = FALSE,
      largeStraight = FALSE,
      chance = FALSE,
      yahtzee = FALSE
    ),
    upperComplete = FALSE,
    allComplete = FALSE
  )
  
  updateUpperSum <- function() {
    counters$pointsTable$upperSum <- counters$pointsTable$ones +
      counters$pointsTable$twos +
      counters$pointsTable$threes +
      counters$pointsTable$fours +
      counters$pointsTable$fives +
      counters$pointsTable$sixes
    
    if(counters$pointsTable$upperSum >= 63) {
      counters$pointsTable$bonus <- 35  
    }
  }
  
  updateGrandTotal <- function() {
    counters$pointsTable$grandTotal <- counters$pointsTable$upperSum +
      counters$pointsTable$bonus +
      counters$pointsTable$threeOfAKind +
      counters$pointsTable$fourOfAKind +
      counters$pointsTable$fullHouse +
      counters$pointsTable$smallStraight +
      counters$pointsTable$largeStraight +
      counters$pointsTable$chance +
      counters$pointsTable$yahtzee
  }
  
  checkIfUpperComplete <- function() {
    if(!counters$upperComplete) {
      if(counters$pointsUsed$ones & counters$pointsUsed$twos & counters$pointsUsed$threes &
         counters$pointsUsed$fours & counters$pointsUsed$fives & counters$pointsUsed$sixes) {
        counters$upperComplete <- TRUE
      }
    }
  }
  
  checkIfAllComplete <- function() {
    if(counters$upperComplete) {
      if(counters$pointsUsed$threeOfAKind & counters$pointsUsed$fourOfAKind &
         counters$pointsUsed$fullHouse & counters$pointsUsed$smallStraight &
         counters$pointsUsed$largeStraight & counters$pointsUsed$chance &
         counters$pointsUsed$yahtzee) {
        counters$allComplete <- TRUE
        showModal(
          modalDialog(
            title = "Game Complete!",
            HTML(
              "<b>Final Score = ", counters$pointsTable$grandTotal, "</b> <br><br>",
              if(counters$pointsTable$grandTotal < 150) {
                "<i>Perhaps you should try a different game....</i> <br><br>"
              } else if(counters$pointsTable$grandTotal < 200) {
                "<i>Better luck next time!</i> <br><br>"
              } else if(counters$pointsTable$grandTotal < 250) {
                "<i>Not bad.</i> <br><br>"
              } else if(counters$pointsTable$grandTotal < 300) {
                "<i>Nice score!</i> <br><br>"
              } else if(counters$pointsTable$grandTotal < 350) {
                "<i>You killed it!</i> <br><br>"
              } else {
                "<i>If I had a leaderboard, you would be up there.</i> <br><br>"
              },
              "<u>Game Summary</u> <br>",
              "Ones: ", counters$pointsTable$ones, "</b> <br>",
              "Twos: ", counters$pointsTable$twos, "</b> <br>",
              "Threes: ", counters$pointsTable$threes, "</b> <br>",
              "Fours: ", counters$pointsTable$fours, "</b> <br>",
              "Fives: ", counters$pointsTable$fives, "</b> <br>",
              "Sixes: ", counters$pointsTable$sixes, "</b> <br>",
              "Upper Sum Bonus: ", counters$pointsTable$bonus, "</b> <br>",
              "3 of a Kind: ", counters$pointsTable$threeOfAKind, "</b> <br>",
              "4 of a Kind: ", counters$pointsTable$fourOfAKind, "</b> <br>",
              "Full House: ", counters$pointsTable$fullHouse, "</b> <br>",
              "Small Straight: ", counters$pointsTable$smallStraight, "</b> <br>",
              "Large Straight: ", counters$pointsTable$largeStraight, "</b> <br>",
              "Chance: ", counters$pointsTable$chance, "</b> <br>",
              "Yahtzee: ", counters$pointsTable$yahtzee
            ),
            footer = modalButton("Play New Game")
          )
        )
        startNewGame()
      }
    }
  }
  
  showGameStatus <- function() {
    if(!counters$allComplete & !(counters$round == 1 & counters$roll == 0)) {
      # showModal(
      #   modalDialog(
      #     title = "Game Status...",
      #     paste0(
      #       "Rounds Completed = ",
      #       if(counters$roll == 1) {
      #         counters$round
      #       } else {
      #         counters$round - 1  
      #       }
      #     ),
      #     br(),
      #     paste0("Current Score = ", counters$pointsTable$grandTotal)
      #   )
      # )
      showNotification(
        ui = HTML(
          "<b>Game Status...</b> <br> Rounds Completed = ",
          if(counters$roll == 1) {
            counters$round
          } else {
            counters$round - 1
          },
          "<br> Current Score = ",
          counters$pointsTable$grandTotal
        ),
        duration = 3,
        closeButton = FALSE
      )
      updateActionButton(
        session = session,
        inputId = "nextRoll",
        label = paste("Begin next round: Round", counters$round),
        icon = icon("play-circle")
      )
    }
    for(i in 1:5) {
      updateSwitchInput(
        session = session,
        inputId = switchInputIds[i],
        value = FALSE,
        disabled = TRUE
      )
    }
  }
  
  startNewGame <- function() {
    counters$game <- counters$game + 1
    counters$round <- 1
    counters$roll <- 0
    counters$dieValues <- c(0,0,0,0,0)
    counters$pointsTable <- data.frame(
      ones = 0,
      twos = 0,
      threes = 0,
      fours = 0,
      fives = 0,
      sixes = 0,
      upperSum = 0,
      bonus = 0,
      threeOfAKind = 0,
      fourOfAKind = 0,
      fullHouse = 0,
      smallStraight = 0,
      largeStraight = 0,
      chance = 0,
      yahtzee = 0,
      grandTotal = 0
    )
    counters$pointsUsed <- data.frame(
      ones = FALSE,
      twos = FALSE,
      threes = FALSE,
      fours = FALSE,
      fives = FALSE,
      sixes = FALSE,
      threeOfAKind = FALSE,
      fourOfAKind = FALSE,
      fullHouse = FALSE,
      smallStraight = FALSE,
      largeStraight = FALSE,
      chance = FALSE,
      yahtzee = FALSE
    )
    counters$upperComplete <- FALSE
    counters$allComplete <- FALSE
    updateActionButton(
      session = session,
      inputId = "nextRoll",
      label = paste("START GAME ", counters$game),
      icon = icon("play-circle")
    )
    enable("nextRoll")
    for(scoreButton in scoreActionButtonsInputIds) {
      show(scoreButton)
      disable(scoreButton)
    }
    for(i in 1:5) {
      updateSwitchInput(
        session = session,
        inputId = switchInputIds[i],
        value = FALSE,
        disabled = TRUE
      )
    }
    disable("forfeitGame")
  }
  
  switchInputIds <- c()
  for(i in 1:5) {
    switchInputIds <- c(switchInputIds, paste0("lockDie", toString(i)))
  }
  
  scoreActionButtonsInputIds <- c("pickOnes", "pickTwos","pickThrees","pickFours",
                                  "pickFives","pickSixes","pick3OfAKind","pick4OfAKind",
                                  "pickFullHouse","pickSmallStraight","pickLargeStraight",
                                  "pickChance","pickYahtzee")
  endRoundEarly <- function() {
    if(counters$roll %in% c(1,2)) {
      counters$round <- counters$round + 1
      counters$roll <- 0
      updateActionButton(
        session = session,
        inputId = "nextRoll",
        label = paste("Begin next round: Round", counters$round),
        icon = icon("play-circle")
      )
      for(i in 1:5) {disable(switchInputIds[i])}
      for(scoreButton in scoreActionButtonsInputIds) {disable(scoreButton)}
    }
  }
  
  observeEvent(input$beep, {
    js$beep()
  })
  
  observeEvent(input$nextRoll, {
    #the forfeit option should be available at all times except at beginning of game
    if(counters$round == 1 & counters$roll == 0) {enable("forfeitGame")}
    
    if(counters$roll == 0) {
      #right after scoring and ready for new round
      for(i in 1:5) {
        updateSwitchInput(
          session = session,
          inputId = switchInputIds[i],
          disabled = FALSE
        )
      }
      for(scoreButton in scoreActionButtonsInputIds) {enable(scoreButton)}
      counters$roll <- counters$roll + 1
      updateActionButton(
        session = session,
        inputId = "nextRoll",
        label = paste0("Take Roll #", counters$roll + 1),
        icon = icon("dice")
      )
    } else if(counters$roll == 2) {
      #right after 2nd roll
      counters$round <- counters$round + 1
      counters$roll <- 0
      updateActionButton(
        session = session,
        inputId = "nextRoll",
        label = "Pick your score before proceeding....",
        icon = icon("pause-circle")
      )
      disable("nextRoll")
      for(i in 1:5) {disable(switchInputIds[i])}
      # for(scoreButton in scoreActionButtonsInputIds) {disable(scoreButton)}
    } else {
      #right after 1st roll
      counters$roll <- counters$roll + 1
      updateActionButton(
        session = session,
        inputId = "nextRoll",
        label = paste0("Take Roll #", counters$roll + 1)
      )
    }
    
    if(input$lockDie1 == TRUE) {
      counters$dieValues[1] <- counters$dieValues[1]
    } else {
      counters$dieValues[1] <- sample(x = 1:6, size = 1)
    }
    
    if(input$lockDie2 == TRUE) {
      counters$dieValues[2] <- counters$dieValues[2]
    } else {
      counters$dieValues[2] <- sample(x = 1:6, size = 1)
    }
    
    if(input$lockDie3 == TRUE) {
      counters$dieValues[3] <- counters$dieValues[3]
    } else {
      counters$dieValues[3] <- sample(x = 1:6, size = 1)
    }
    
    if(input$lockDie4 == TRUE) {
      counters$dieValues[4] <- counters$dieValues[4]
    } else {
      counters$dieValues[4] <- sample(x = 1:6, size = 1)
    }
    
    if(input$lockDie5 == TRUE) {
      counters$dieValues[5] <- counters$dieValues[5]
    } else {
      counters$dieValues[5] <- sample(x = 1:6, size = 1)
    }
    
    # insertUI(selector = "#nextRoll",
    #          where = "afterEnd",
    #          ui = tags$audio(src = "diceRoll.mp3", type = "audio/mp3", autoplay = NA, controls = NA, style="display:none;")  
    # )
    
    # tags$audio(src = "diceRoll.mp3", type = "audio/mp3", autoplay = NA, controls = NA)
  })
  
  observeEvent(input$pickOnes, {
    counters$pointsTable$ones <- (
      (if(counters$dieValues[1] == 1) {1} else {0}) +
        (if(counters$dieValues[2] == 1) {1} else {0}) +
        (if(counters$dieValues[3] == 1) {1} else {0}) +
        (if(counters$dieValues[4] == 1) {1} else {0}) +
        (if(counters$dieValues[5] == 1) {1} else {0})
    )
    counters$pointsUsed$ones <- TRUE
    hide("pickOnes")
    updateUpperSum()
    updateGrandTotal()
    checkIfUpperComplete()
    checkIfAllComplete()
    endRoundEarly()
    showGameStatus()
    enable("nextRoll")
    for(scoreButton in scoreActionButtonsInputIds) {disable(scoreButton)}
  })
  
  observeEvent(input$pickTwos, {
    counters$pointsTable$twos <- (
      (if(counters$dieValues[1] == 2) {2} else {0}) +
        (if(counters$dieValues[2] == 2) {2} else {0}) +
        (if(counters$dieValues[3] == 2) {2} else {0}) +
        (if(counters$dieValues[4] == 2) {2} else {0}) +
        (if(counters$dieValues[5] == 2) {2} else {0})
    )
    counters$pointsUsed$twos <- TRUE
    hide("pickTwos")
    updateUpperSum()
    updateGrandTotal()
    checkIfUpperComplete()
    checkIfAllComplete()
    endRoundEarly()
    showGameStatus()
    enable("nextRoll")
    for(scoreButton in scoreActionButtonsInputIds) {disable(scoreButton)}
  })
  
  observeEvent(input$pickThrees, {
    counters$pointsTable$threes <- (
      (if(counters$dieValues[1] == 3) {3} else {0}) +
        (if(counters$dieValues[2] == 3) {3} else {0}) +
        (if(counters$dieValues[3] == 3) {3} else {0}) +
        (if(counters$dieValues[4] == 3) {3} else {0}) +
        (if(counters$dieValues[5] == 3) {3} else {0})
    )
    counters$pointsUsed$threes <- TRUE
    hide("pickThrees")
    updateUpperSum()
    updateGrandTotal()
    checkIfUpperComplete()
    checkIfAllComplete()
    showGameStatus()
    endRoundEarly()
    showGameStatus()
    enable("nextRoll")
    for(scoreButton in scoreActionButtonsInputIds) {disable(scoreButton)}
  })
  
  observeEvent(input$pickFours, {
    counters$pointsTable$fours <- (
      (if(counters$dieValues[1] == 4) {4} else {0}) +
        (if(counters$dieValues[2] == 4) {4} else {0}) +
        (if(counters$dieValues[3] == 4) {4} else {0}) +
        (if(counters$dieValues[4] == 4) {4} else {0}) +
        (if(counters$dieValues[5] == 4) {4} else {0})
    )
    counters$pointsUsed$fours <- TRUE
    hide("pickFours")
    updateUpperSum()
    updateGrandTotal()
    checkIfUpperComplete()
    checkIfAllComplete()
    endRoundEarly()
    showGameStatus()
    enable("nextRoll")
    for(scoreButton in scoreActionButtonsInputIds) {disable(scoreButton)}
  })
  
  observeEvent(input$pickFives, {
    counters$pointsTable$fives <- (
      (if(counters$dieValues[1] == 5) {5} else {0}) +
        (if(counters$dieValues[2] == 5) {5} else {0}) +
        (if(counters$dieValues[3] == 5) {5} else {0}) +
        (if(counters$dieValues[4] == 5) {5} else {0}) +
        (if(counters$dieValues[5] == 5) {5} else {0})
    )
    counters$pointsUsed$fives <- TRUE
    hide("pickFives")
    updateUpperSum()
    updateGrandTotal()
    checkIfUpperComplete()
    checkIfAllComplete()
    endRoundEarly()
    showGameStatus()
    enable("nextRoll")
    for(scoreButton in scoreActionButtonsInputIds) {disable(scoreButton)}
  })
  
  observeEvent(input$pickSixes, {
    counters$pointsTable$sixes <- (
      (if(counters$dieValues[1] == 6) {6} else {0}) +
        (if(counters$dieValues[2] == 6) {6} else {0}) +
        (if(counters$dieValues[3] == 6) {6} else {0}) +
        (if(counters$dieValues[4] == 6) {6} else {0}) +
        (if(counters$dieValues[5] == 6) {6} else {0})
    )
    counters$pointsUsed$sixes <- TRUE
    hide("pickSixes")
    updateUpperSum()
    updateGrandTotal()
    checkIfUpperComplete()
    checkIfAllComplete()
    endRoundEarly()
    showGameStatus()
    enable("nextRoll")
    for(scoreButton in scoreActionButtonsInputIds) {disable(scoreButton)}
  })
  
  observeEvent(input$pick3OfAKind, {
    counters$pointsTable$threeOfAKind <- (
      (if(max(table(counters$dieValues)) >= 3) {sum(counters$dieValues)} else {0})
    )
    counters$pointsUsed$threeOfAKind <- TRUE
    hide("pick3OfAKind")
    updateGrandTotal()
    checkIfAllComplete()
    endRoundEarly()
    showGameStatus()
    enable("nextRoll")
    for(scoreButton in scoreActionButtonsInputIds) {disable(scoreButton)}
  })
  
  observeEvent(input$pick4OfAKind, {
    counters$pointsTable$fourOfAKind <- (
      (if(max(table(counters$dieValues)) >= 4) {sum(counters$dieValues)} else {0})
    )
    counters$pointsUsed$fourOfAKind <- TRUE
    hide("pick4OfAKind")
    updateGrandTotal()
    checkIfAllComplete()
    endRoundEarly()
    showGameStatus()
    enable("nextRoll")
    for(scoreButton in scoreActionButtonsInputIds) {disable(scoreButton)}
  })
  
  observeEvent(input$pickFullHouse, {
    counters$pointsTable$fullHouse <- (
      if(
        2 %in% table(counters$dieValues) & 3 %in% table(counters$dieValues)
      ) {
        25
      } else {
        0
      }
    )
    counters$pointsUsed$fullHouse <- TRUE
    hide("pickFullHouse")
    updateGrandTotal()
    checkIfAllComplete()
    endRoundEarly()
    showGameStatus()
    enable("nextRoll")
    for(scoreButton in scoreActionButtonsInputIds) {disable(scoreButton)}
  })
  
  observeEvent(input$pickSmallStraight, {
    counters$pointsTable$smallStraight <- (
      if(
        (1%in%counters$dieValues & 2%in%counters$dieValues & 3%in%counters$dieValues & 4%in%counters$dieValues) |
        (2%in%counters$dieValues & 3%in%counters$dieValues & 4%in%counters$dieValues & 5%in%counters$dieValues) |
        (3%in%counters$dieValues & 4%in%counters$dieValues & 5%in%counters$dieValues & 6%in%counters$dieValues)
      ) {
        30
      } else {
        0
      }
    )
    counters$pointsUsed$smallStraight <- TRUE
    hide("pickSmallStraight")
    updateGrandTotal()
    checkIfAllComplete()
    endRoundEarly()
    showGameStatus()
    enable("nextRoll")
    for(scoreButton in scoreActionButtonsInputIds) {disable(scoreButton)}
  })
  
  observeEvent(input$pickLargeStraight, {
    counters$pointsTable$largeStraight <- (
      if(
        (1%in%counters$dieValues & 2%in%counters$dieValues & 3%in%counters$dieValues & 4%in%counters$dieValues & 5%in%counters$dieValues) |
        (2%in%counters$dieValues & 3%in%counters$dieValues & 4%in%counters$dieValues & 5%in%counters$dieValues & 6%in%counters$dieValues)
      ) {
        40
      } else {
        0
      }
    )
    counters$pointsUsed$largeStraight <- TRUE
    hide("pickLargeStraight")
    updateGrandTotal()
    checkIfAllComplete()
    endRoundEarly()
    showGameStatus()
    enable("nextRoll")
    for(scoreButton in scoreActionButtonsInputIds) {disable(scoreButton)}
  })
  
  observeEvent(input$pickChance, {
    counters$pointsTable$chance <- (
      sum(counters$dieValues)
    )
    counters$pointsUsed$chance <- TRUE
    hide("pickChance")
    updateGrandTotal()
    checkIfAllComplete()
    endRoundEarly()
    showGameStatus()
    enable("nextRoll")
    for(scoreButton in scoreActionButtonsInputIds) {disable(scoreButton)}
  })
  
  #provided you haven't yet zeroed out yahtzee option...
  observeEvent(input$pickYahtzee, {
    if(max(table(counters$dieValues)) < 5) {
      #zero out this option
      counters$pointsUsed$yahtzee <- TRUE
      hide("pickYahtzee")
    } else if(counters$pointsTable$yahtzee == 0) {
      #first time getting yahtzee
      counters$pointsTable$yahtzee <- 50
      counters$pointsUsed$yahtzee <- TRUE
    } else {
      #not first time getting yahtzee
      counters$pointsTable$yahtzee <- counters$pointsTable$yahtzee + 100
    }
    updateGrandTotal()
    checkIfAllComplete()
    endRoundEarly()
    showGameStatus()
    enable("nextRoll")
    for(scoreButton in scoreActionButtonsInputIds) {disable(scoreButton)}
  })
  
  #forfeit game
  observeEvent(input$forfeitGame, {
    startNewGame()
  })
  
  output$gameNumber <- renderText({
    paste("Game", counters$game)
  })
  
  output$roundNumber <- renderText({
    paste("Round", counters$round)
  })
  
  output$rollNumber <- renderText({
    paste("Roll", counters$roll)
  })
  
  output$die1Value <- renderUI({
    if(counters$dieValues[1] == 1) {
      icon(name = "dice-one", class = "fa fa-dice-one fa-7x")
    } else if(counters$dieValues[1] == 2) {
      icon(name = "dice-two", class = "fa fa-dice-two fa-7x")
    } else if(counters$dieValues[1] == 3) {
      icon(name = "dice-three", class = "fa fa-dice-three fa-7x")
    } else if(counters$dieValues[1] == 4) {
      icon(name = "dice-four", class = "fa fa-dice-four fa-7x")
    } else if(counters$dieValues[1] == 5) {
      icon(name = "dice-five", class = "fa fa-dice-five fa-7x")
    } else if(counters$dieValues[1] == 6) {
      icon(name = "dice-six", class = "fa fa-dice-six fa-7x")
    } else {
      icon(name = "dice", class = "fa fa-dice fa-7x")
    }
  })
  
  output$die2Value <- renderUI({
    if(counters$dieValues[2] == 1) {
      icon(name = "dice-one", class = "fa fa-dice-one fa-7x")  
    } else if(counters$dieValues[2] == 2) {
      icon(name = "dice-two", class = "fa fa-dice-two fa-7x")  
    } else if(counters$dieValues[2] == 3) {
      icon(name = "dice-three", class = "fa fa-dice-three fa-7x")  
    } else if(counters$dieValues[2] == 4) {
      icon(name = "dice-four", class = "fa fa-dice-four fa-7x")  
    } else if(counters$dieValues[2] == 5) {
      icon(name = "dice-five", class = "fa fa-dice-five fa-7x")  
    } else if(counters$dieValues[2] == 6) {
      icon(name = "dice-six", class = "fa fa-dice-six fa-7x")  
    } else {
      icon(name = "dice", class = "fa fa-dice fa-7x")
    }
  })
  
  output$die3Value <- renderUI({
    if(counters$dieValues[3] == 1) {
      icon(name = "dice-one", class = "fa fa-dice-one fa-7x")  
    } else if(counters$dieValues[3] == 2) {
      icon(name = "dice-two", class = "fa fa-dice-two fa-7x")  
    } else if(counters$dieValues[3] == 3) {
      icon(name = "dice-three", class = "fa fa-dice-three fa-7x")  
    } else if(counters$dieValues[3] == 4) {
      icon(name = "dice-four", class = "fa fa-dice-four fa-7x")  
    } else if(counters$dieValues[3] == 5) {
      icon(name = "dice-five", class = "fa fa-dice-five fa-7x")  
    } else if(counters$dieValues[3] == 6) {
      icon(name = "dice-six", class = "fa fa-dice-six fa-7x")  
    } else {
      icon(name = "dice", class = "fa fa-dice fa-7x")
    }
  })
  
  output$die4Value <- renderUI({
    if(counters$dieValues[4] == 1) {
      icon(name = "dice-one", class = "fa fa-dice-one fa-7x")  
    } else if(counters$dieValues[4] == 2) {
      icon(name = "dice-two", class = "fa fa-dice-two fa-7x")  
    } else if(counters$dieValues[4] == 3) {
      icon(name = "dice-three", class = "fa fa-dice-three fa-7x")  
    } else if(counters$dieValues[4] == 4) {
      icon(name = "dice-four", class = "fa fa-dice-four fa-7x")  
    } else if(counters$dieValues[4] == 5) {
      icon(name = "dice-five", class = "fa fa-dice-five fa-7x")  
    } else if(counters$dieValues[4] == 6) {
      icon(name = "dice-six", class = "fa fa-dice-six fa-7x")  
    } else {
      icon(name = "dice", class = "fa fa-dice fa-7x")
    }
  })
  
  output$die5Value <- renderUI({
    if(counters$dieValues[5] == 1) {
      icon(name = "dice-one", class = "fa fa-dice-one fa-7x")  
    } else if(counters$dieValues[5] == 2) {
      icon(name = "dice-two", class = "fa fa-dice-two fa-7x")  
    } else if(counters$dieValues[5] == 3) {
      icon(name = "dice-three", class = "fa fa-dice-three fa-7x")  
    } else if(counters$dieValues[5] == 4) {
      icon(name = "dice-four", class = "fa fa-dice-four fa-7x")  
    } else if(counters$dieValues[5] == 5) {
      icon(name = "dice-five", class = "fa fa-dice-five fa-7x")  
    } else if(counters$dieValues[5] == 6) {
      icon(name = "dice-six", class = "fa fa-dice-six fa-7x")
    } else {
      icon(name = "dice", class = "fa fa-dice fa-7x")
    }
  })
  
  output$pointsTable <- renderTable({
    pointsTable <- data.frame(t(counters$pointsTable))
    pointsTable <- setDT(pointsTable, keep.rownames = TRUE)
    colnames(pointsTable) <- c("Points Type", "Points Value")
    pointsTable[,2] <- lapply(pointsTable[,2], as.integer)
    print(pointsTable)
  })
  
  gameRulesWebsite <- a(
    "Yahtzee Game Rules",
    href = "http://grail.sourceforge.net/demo/yahtzee/rules.html",
    target="_blank"
  )
  
  output$linkToGameRules <- renderUI({
    tagList(gameRulesWebsite)
  })
  
  output$instructions <- renderText({
    HTML(
      "
      <font size=3>Game Instructions</font>
      <ol>
        <li>Use the blue button in the middle to roll the dice.</li>
        <li>Use the toggles next to the dice to lock/unlock them.</li>
        <li>Use the green buttons to the right to pick your score. You can select these early within a round (i.e. before you roll 3 times). Selecting a score option that is not possible is the same as zeroing out that option.</li>
        <li>Use the red button below to start the game all over.</li>
      </ol>
      <font size=2>For detailed game rules and scoring options, go to this link:</font>
      "
    )
  })
  
  output$devNotes <- renderPrint({
    print("bug: double check some of the summing")
    print("bug: toggle dice black vs white")
    print("use hide for when move is used; use disable to toggle button")
    print("Abby: yahtzee hand held simulator")
    print("music")
    print("BD: two players")
    print("me: show probabilities of future strategies")
  })
  
  output$debugBox <- renderPrint({
    print("debug")
    # print(paste("round", counters$round))
    # print(paste("roll", counters$roll))
    # print(counters$upperComplete)
    # print(counters$allComplete)
    # print(input$lockDie1)
    print(counters$dieValues)
  })
}

shinyApp(ui, server)