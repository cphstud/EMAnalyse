library(rsconnect)
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggsoccer)
library(stringr)
library(jsonlite)
library(grid)


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("matches",
                  label = "vælg kamp",
                  choices = c("Denmark-Slovenia", "Denmark-England", "Denmark-Serbia", "Denmark-Germany"),
                  #choices <- c("dkSlov", "dkUK", "dkSerb"),
                  selected = "Denmark-England"
                  ),
      selectInput("team",
                  label = "vælg hold",
                  choices = c("Denmark","Slovenia","UK"),
                  selected = "Denmark"
                  ),
      numericInput("minutes",
                  label = "Vælg kampperiode",
                  value = 1, step = 1
                  ),
      numericInput("mincount",
                   label = "Vælg minimum antal afleveringer",
                   value = 1, step = 1
      ),
      checkboxInput("tick",
                  label = "Vælg minimum antal afleveringer",
                  value = 1 )
    ),
      mainPanel(
        tableOutput("result"),
        plotOutput("passplot"),
        plotOutput("passstat"),
        plotOutput("passstatII"),
        plotOutput("goalstat")
      )
    )
  )

server <- function(input, output, session) {
  dkSlov = fromJSON("data/dkslov.json", flatten = T)
  dkUK = fromJSON("data/dkgb.json", flatten = T)
  dkSerb = fromJSON("data/dkserb.json", flatten = T)
  dkGerm = fromJSON("data/dkgerm.json", flatten = T)
  gc <- c("Denmark-Slovenia"="dkSlov", "Denmark-Germany"="dkGerm", "Denmark-England"="dkUK", "Denmark-Serbia"="dkSerb")
  
  #getT <- reactive({
  #  dkm <- input$matches
  #  paste(dkm)
  #})
    
  
  output$result <- renderTable({
    dkm <- input$matches
    mtest=gc[dkm]
    myMatch=get(mtest)
    myMatch <- myMatch %>% mutate(period=(minute %/% 10)+1)
    dkMShots=myMatch %>% filter(type.name=="Shot")
    myGoals = dkMShots %>% filter(shot.outcome.name == "Goal") %>% select(team.name,shot.outcome.name, period)
  })
  
  observe({
    dkm <- input$matches
    ttin <- unlist(str_split(dkm,"-"))
    t1 <-ttin[1]
    t2 <-ttin[2]
    updateSelectInput(session, "team", choices = c(t1,t2))
  })
  
  output$goalstat <- renderPlot({
    dkm <- input$matches
    mtest=gc[dkm]
    myMatch=get(mtest)
    myMatch <- myMatch %>% mutate(period=(minute %/% 10)+1)
    dkMShots=myMatch %>% filter(type.name=="Shot")
    myTotShots = dkMShots %>% group_by(team.name) %>% mutate(cnt=n()) %>% select(team.name,cnt) %>% unique() %>% ungroup()
    myShotP = dkMShots %>% group_by(team.name, period) %>% mutate(cnt=n()) %>% 
      select(team.name,period,cnt) %>% unique() %>% ungroup()
    myGoals = dkMShots %>% filter(shot.outcome.name == "Goal") %>% select(team.name,shot.outcome.name, period)
    
    ggplot(myShotP, aes(x = period, y = cnt, color = team.name)) +
      #geom_bar(stat = "identity", position = "dodge")+
      geom_point()+
      geom_line()+
      scale_x_continuous(breaks = seq(min(myShotP$period), max(myShotP$period), by = 1)) +
      ylim(0, max(myShotP$cnt))+
      labs(title = "Team Shots Over Periods",
           x = "Period",
           y = "Count",
           color = "Team")
    
  })
  
  output$passstatII <- renderPlot({
    dkm <- input$matches
    mtest=gc[dkm]
    myMatch=get(mtest)
    myMatch <- myMatch %>% mutate(period=(minute %/% 10)+1)
    dkMPasses=myMatch %>% filter(type.name=="Pass")
    totPass = dkMPasses %>% group_by(team.name) %>% mutate(cnt=n()) %>% select(team.name,cnt) %>% unique() %>% ungroup()
    periodPass = dkMPasses %>% group_by(team.name, period) %>% mutate(cnt=n()) %>% select(team.name,period,cnt) %>% unique() %>% ungroup()
    ggplot(periodPass, aes(x = period, y = cnt, color = team.name, group = team.name)) +
      geom_line() +
      geom_point() +
      scale_x_continuous(breaks = seq(min(myPassP$period), max(myPassP$period), by = 1)) +
      labs(title = "Team Passes Over Periods",
           x = "Period (10 minutes)",
           y = "Count",
           color = "Team") +
      theme_minimal()
  })
  
  output$passstat <- renderPlot({
    dkm <- input$matches
    mtest=gc[dkm]
    myMatch=get(mtest)
    myMatch <- myMatch %>% mutate(period=(minute %/% 10)+1)
    dkMPasses=myMatch %>% filter(type.name=="Pass")
    totPass = dkMPasses %>% group_by(team.name) %>% mutate(cnt=n()) %>% select(team.name,cnt) %>% unique() %>% ungroup()
    periodPass = dkMPasses %>% group_by(team.name, period) %>% mutate(cnt=n()) %>% select(team.name,period,cnt) %>% unique() %>% ungroup()
    
    dkMPassesFW=dkMPasses %>% mutate(fw=ifelse(pass.angle < 1,0,1))
    dkMPassesFWCt=dkMPassesFW %>% filter(fw==1) %>%  group_by(team.name,period) %>% mutate(fwct=n()) %>% select(team.name,period,fwct) %>% unique() %>% ungroup()
    dkMPassesFWCtre=dkMPassesFW %>% filter(fw==0) %>%  group_by(team.name,period) %>% mutate(rwct=n()) %>% select(team.name,period,rwct) %>% unique() %>% ungroup()
    
    dkTot=merge(dkMPassesFWCt,dkMPassesFWCtre) 
    data_long <- dkTot %>%
    pivot_longer(cols = c("fwct", "rwct"), names_to = "type", values_to = "count")
    
  ggplot(data_long, aes(x = period, y = count, color = interaction(team.name, type), group = interaction(team.name, type))) +
    geom_line() +
    geom_point() +
    scale_x_continuous(breaks = seq(min(data_long$period), max(data_long$period), by = 1)) +
    labs(title = "Team Passes (forward and backwards (fwct and rwct)) Over Periods",
         x = "Period",
         y = "Count",
         color = "Team and Type") +
    theme_minimal()
    
    
#    ggplot(dkMPassesFWCtre, aes(x = period, y = fwct, color = team.name, group = team.name)) +
#      geom_line() +
#      geom_point() +
#      scale_x_continuous(breaks = seq(min(dkMPassesFWCtre$period), max(dkMPassesFWCtre$period), by = 1)) +
#      labs(title = "Team backPasses Over Periods",
#           x = "Period (10 minutes)",
#           y = "Count",
#           color = "Team") +
#      theme_minimal()
#    
#    ggplot(dkMPassesFWCt, aes(x = period, y = fwct, color = team.name, group = team.name)) +
#      geom_line() +
#      geom_point() +
#      scale_x_continuous(breaks = seq(min(dkMPassesFWCtre$period), max(dkMPassesFWCtre$period), by = 1)) +
#      labs(title = "Team Forward Passes Over Periods",
#           x = "Period (10 minutes)",
#           y = "Count",
#           color = "Team") +
#      theme_minimal()
#    
    #ggplot(myPassP, aes(x = period, y = cnt, fill = team.name, group = team.name)) +
    #  geom_bar(stat = "identity", position = "dodge") +
    #  labs(title = "Team Passes Over Periods",
    #       x = "Period",
    #       y = "Count",
    #       color = "Team") +
    #  theme_minimal()
    #ggplot(myShotP, aes(x = period, y = cnt, color = team.name)) +
    #  geom_line()+
    #  labs(title = "Team Shots Over Periods",
    #       x = "Period",
    #       y = "Count",
    #       color = "Team")
   # 
   # 
  #  #print(dkMPassesBU)
   # print("2dkMPassesBU")
    
  })

  output$passplot <- renderPlot({
    #get input vars
    tval <- input$minutes
    team <- input$team
    minpass <- input$mincount
    test=input$matches
    print(minpass)
    mtest=gc[test]
    
    # get the chosen match
    dkMatch=get(mtest)
    dkMatch=dkSlov
    dkMPassesBU=dkMatch %>% filter(type.name=="Pass")
    dkMPasses=dkMPassesBU
    dkMPasses$location.x=unlist(lapply(dkMPasses$location, function(x) x[1]))
    dkMPasses$location.y=unlist(lapply(dkMPasses$location, function(x) x[2]))
    dkMPasses$pass.endLocation.x=unlist(lapply(dkMPasses$pass.end_location, function(x) x[1]))
    dkMPasses$pass.endLocation.y=unlist(lapply(dkMPasses$pass.end_location, function(x) x[2]))
    dkMPasses$pass.endLocation.y=unlist(lapply(dkMPasses$pass.end_location, function(x) x[2]))
    dkMPasses <- dkMPasses %>% mutate(period=(minute %/% 10)+1)
    print(minpass)
    
    dkMPassesG <- dkMPasses %>% filter(period==tval)
    allPM <- dkMPassesG
    allPM1 <- allPM %>% filter(team.name==team)
    allPM1gr <- allPM1 %>% group_by(player.name) %>% 
      mutate(mx=mean(location.x),
             my=mean(location.y),
      )  %>% select(player.name,
                    period,
                    pass.recipient.name,
                    mx,
                    my,
                    pass.endLocation.x,
                    pass.endLocation.y,
                    team.name
      ) %>% unique() 
    allPM1gr <- allPM1gr %>% group_by(pass.recipient.name) %>% mutate(
      mrx=mean(pass.endLocation.x),
      mry=mean(pass.endLocation.y),
    ) %>% ungroup()
    # now count the number of passes
    df2=allPM1gr %>% group_by(player.name,pass.recipient.name) %>% 
      mutate(cnt=n()) %>% arrange(desc(cnt)) %>% filter(cnt>minpass) %>% unique() %>% 
      select(player.name,cnt,pass.recipient.name,mx,my,mrx,mry,team.name) %>%  unique()
    
    custom_colors <- c("A" = "blue", "B" = "red")
    custom_labels <- c("A" = "Group A", "B" = "Group B")
    
    ggplot(df2) +
      annotate_pitch() +
      geom_segment(aes(x = mx, y = my, xend = mrx, yend = mry),size=df2$cnt/8,
                   arrow = arrow(length = unit(0.3, "cm"), type = "closed"))+
      geom_point(aes(x=mx,y=my,color=df2$team.name), size=2)+
      geom_point(aes(x=mrx,y=mry,color=df2$team.name), size=2)+
      geom_text(aes(x=mx,y=my,label=player.name),color="blue",size=2,vjust=-2)+
      geom_text(aes(x=mrx,y=mry,label=pass.recipient.name),color="red",size=2,vjust=2)+
      theme_pitch() +
      theme(legend.position = "none")+
      direction_label() +
      ggtitle("passmap", df2$team.name )
  })
}

shinyApp(ui, server)
