# Load R packages
library(shiny)
library(shinythemes)
library(ggplot2)
data<-valo
guess=0

  # Define UI
  ui <- fluidPage(theme = shinytheme("yeti"),
    navbarPage(
      # theme = "cerulean",  # <--- To use a theme, uncomment this
      "Valorant Data Set",
      tags$style(HTML("
    body {
            background-color: #F4AF1B;
            color: black;
            }")),
      navbarMenu("Home",
                 tabPanel("Home",
                            h1("Valorant LeaderBoard Statistics", align="Center"),
                            h2("Episode 4: Act 3",align = "Center"),
                 
              
               mainPanel(
                           h3("Analysis By: The Marauders"),
                           h3("Team Memebers:"),
                           h4("Rao Mueez        20F-0315"),
                           h4("Huzaif Bin Tariq 20F-0272"),
                           h4("Abdul Rehman     20F-0238"),
                           h4("Muhammad Shayan  20F-0348"),

               ),
                          div( imageOutput("pics"),style="text-align:center"),
               # mainPanel
               h2("Intro"),
               h4("The data set we have chosen consist of over 85,000 entries and we have used this data to predict any given player`s rank."),
               h4("We have also shown graphs for various statistics of all leaderboard players globally and regionally."),
      ), # Navbar 1, tabPanel
      tabPanel("Graphs",
               h1("Graphs",align="center",style="Bold"),
               h3("Below are the comparsions of statistics for average win and headshot percentage between all six regions"),
               h3("Regions Inculde:"),
               h4("Asia Pacific - AP"),
               h4("Brazil - BR"),
               h4("Europe - EU"),
               h4("Korea - KR"),
               h4("Latin America - LAT"),
               h4("North America - NAM"),
               plotOutput("bar1"),
               plotOutput("bar2"),
               h3("Now we are comparing Radiant(Peak Rank in the game) with all the other ranked players on the leaderboard"),
               h3("We can clearly see that each statistic for the non-radiants is greater in comparison to the radiant players,
                  this is because of an influx of smurfers(a player in an online game that creates a new account to play against lower-ranked players) and 
                  rank boosters(high rank players that play for others to increase their rank while getting paid)."),
               plotOutput("gplot1"),
               plotOutput("gplot2"),
               plotOutput("gplot3"),
               plotOutput("gplot4"),
               plotOutput("gplot5"),
               h4("We can see from the win percentage graphs below that the majority population for each region has a win percentage 46% - 60%"),
               plotOutput("barp1"),
               plotOutput("barp2"),
               plotOutput("barp3"),
               plotOutput("barp4"),
               plotOutput("barp5"),
               plotOutput("barp6"),
               plotOutput("barp7")),
      tabPanel("Summary analysis",
               h1("Summary Analysis",align="Center"),
               
               verbatimTextOutput("summ"),
               
               ),
      
      
     
      tabPanel("Perdiction",
               h1("Prediction",align="center"),
               h3("We can Predict based on a player`s input which rank category they fall in.
                  If they are closer to the mean, they compare to immortal 1-3 and if they greater than mean, they are
                  either radiant or smurf/booster"),
               sidebarPanel(
                 tags$h3("Search Data:"),
                 numericInput("hp", "Head percentage:", ""),
                 numericInput("wp", "Win percentage:", ""),
                 numericInput("dpr", "Damage Per Round:", ""),
                 numericInput("acs", "Average Combat Score:", ""),
                 numericInput("kd", "Kill Death Ratio:", ""),
                 actionButton("submitbutton", 
                              "Submit", 
                              class = "btn btn-primary"),
               ),
               mainPanel(
                 h1("Result"),
                 plotOutput("norm",width = "800px",height = "500px"),

                 verbatimTextOutput('contents'),
                 tableOutput('tabledata'), # Results table
                 h1(""),
                 h1(""),
                 h1(""),
               ),
               h3(""),
               h3("The Normal distribution graph above is based on the histogram below when we do not sort our data by rank."),
                 plotOutput("wingatibba"),
              
               ),
    ),
                 
    # mainPanel
     
  
    ) # navbarPage
   )# )fluidPage

 
  # Define server function  
  server <- function(input, output) {
    
    
    output$pics<-renderImage(
      {
        list(src="C:/Users/ACER/OneDrive/Desktop/Prob/pic/logo.png",height="300px",width="250px",alt="something went wrong")
      },deleteFile=FALSE
    )
    
    datasetInput <- reactive({
     guess = (-7.457e-13) + (0.3*input$hp)+(0.3*input$wp)+(4.290*input$kd)+(0.005460*input$acs)+(0.08340*input$dpr)
      
  names(guess)<-"Score"
      print(guess)
      
    })
    output$contents <- renderPrint({
      if (input$submitbutton>0) { 
        isolate("Calculation complete.") 
      } else {
        return("Server is ready for calculation.")
      }
    })
    output$tabledata <- renderTable({
      if (input$submitbutton>0) { 
        
        isolate(datasetInput()) 
      } 
    })
      output$guess<-renderText({
        guess = -7.457e-13 + (0.3*head_per)+(0.3*win_per)+(4.290*kd)+(0.005460*acs)+(0.08340*dpr)
        
      })
      output$bar1 <- renderPlot({
        barplot(by(valo$win_percent,valo$region,mean),main="Avg Win% Comparison",ylim=c(0,60))
        
    })
      output$bar2 <- renderPlot({
        barplot(by(valo$headshot_percent,valo$region,mean),main="Avg Headshot% Comparison",ylim=c(0,30))
        
      })
      
      output$gplot1 <- renderPlot({
        ggplot(valo, aes(Rank=="Radiant", headshot_percent, fill = region, stack=TRUE)) + geom_bar(stat="identity", position = "dodge")+labs(title="Radiant Comparison To Rest",x="Radiant Rank Reached",y="HeadShot%")
        
      })
      output$gplot2 <- renderPlot({
        ggplot(valo, aes(Rank=="Radiant", win_percent, fill = region, stack=TRUE)) + geom_bar(stat="identity", position = "dodge")+labs(title="Radiant Comparison To Rest",x="Radiant Rank Reached",y="Win%")
        
      })
      output$gplot3 <- renderPlot({
        ggplot(valo, aes(Rank=="Radiant", damage_round, fill = region, stack=TRUE)) + geom_bar(stat="identity", position = "dodge")+labs(title="Radiant Comparison To Rest",x="Radiant Rank Reached",y="Mean Damage Per Round")
        
      }) 
      output$gplot4 <- renderPlot({
        ggplot(valo, aes(Rank=="Radiant", ACS_round, fill = region, stack=TRUE)) + geom_bar(stat="identity", position = "dodge")+labs(title="Radiant Comparison To Rest",x="Radiant Rank Reached",y="Mean Combat Score Per Round")
        
      })
      output$gplot5 <- renderPlot({
        ggplot(valo, aes(Rank=="Radiant", kd_ratio, fill = region, stack=TRUE)) + geom_bar(stat="identity", position = "dodge")+labs(title="Radiant Comparison To Rest",x="Radiant Rank Reached",y="Mean Kills Per Deaths%")
        
      })
      
      output$barp1 <- renderPlot({
        barplot(all_region,names.arg=c("0%-15%","16%-30%","31%-45%","46%-60%","61%-75%","76%-90%","91%-100%"),xlab="Win Percentage",ylab="Players",ylim=c(0,90000),main="Global")
        
      })
      output$barp2 <- renderPlot({
        barplot(asiaPacific,names.arg=c("0%-15%","16%-30%","31%-45%","46%-60%","61%-75%","76%-90%","91%-100%"),xlab="Win Percentage",ylab="Players",ylim=c(0,20000),main="Asia Pacific")
        
      })
      output$barp3 <- renderPlot({
        barplot(europe,names.arg=c("0%-15%","16%-30%","31%-45%","46%-60%","61%-75%","76%-90%","91%-100%"),xlab="Win Percentage",ylab="Players",ylim=c(0,33000),main="Europe")
        
      })
      output$barp4 <- renderPlot({
        barplot(northAmerica,names.arg=c("0%-15%","16%-30%","31%-45%","46%-60%","61%-75%","76%-90%","91%-100%"),xlab="Win Percentage",ylab="Players",ylim=c(0,21000),main="North America")
        
      })
      output$barp5 <- renderPlot({
        barplot(korea,names.arg=c("0%-15%","16%-30%","31%-45%","46%-60%","61%-75%","76%-90%","91%-100%"),xlab="Win Percentage",ylab="Players",ylim=c(0,2500),main="Korea")
        
      })
      output$barp6 <- renderPlot({
        barplot(latinAmerica,names.arg=c("0%-15%","16%-30%","31%-45%","46%-60%","61%-75%","76%-90%","91%-100%"),xlab="Win Percentage",ylab="Players",ylim=c(0,3500),main="Latin America")
        
      })
      output$barp7 <- renderPlot({
        barplot(brazil,names.arg=c("0%-15%","16%-30%","31%-45%","46%-60%","61%-75%","76%-90%","91%-100%"),xlab="Win Percentage",ylab="Players",ylim=c(0,7000),main="Brazil")
        
      })
      
      output$norm<-renderPlot(
        {
          chances<-((0.2*valo$win_percent+0.00364*valo$ACS_round+2.86*valo$kd_ratio+0.0556*valo$damage_round+
                       0.2*valo$headshot_percent)*1.5)
          R<-c("Asia","Brazil","Europe","Korea","Latin-America","North-America")
          m<-mean(chances)
          s<-sd(chances)
          
          range=seq.default((m-(4*s)),(m+(4*s)),0.01)
          
          y<-dnorm(range,m,s)
          plot(range,y,type='l',ylim=c(0,max(y)+0.01),axes=FALSE)
          abline(v=guess,col="red")
          axis(1,at = seq(m-3*s,m+3*s,s))
        }
      )
      
      output$wingatibba<-renderPlot(
        {
          ggplot(valoo, aes(Rank, chances, fill = region,stack=TRUE)) + geom_bar(stat="identity", position = "dodge")
          
        }
      )
      
      output$summ<-renderPrint(
        {
          summary(valo)
        }
      )
  } # server
  

  # Create Shiny object
  shinyApp(ui = ui, server = server)
