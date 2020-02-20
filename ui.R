library(shiny)
library(shinythemes)
shinyUI(navbarPage(
  theme = shinytheme("yeti"),
  strong("한국 외래관광객 수요예측 모형 연습 - Korea Tourism Inbound TSForecasting Shinyapp (2010 to 2019)"),
  tabPanel("Data Summary",
           sidebarLayout(
             sidebarPanel(
               radioButtons("RD1",label=h3(strong("Data Type")),choices = list("국가별"=1,"Your Data"=2),selected = 1),
               
               conditionalPanel(condition="input.RD1==2",fileInput('file1', h4(strong('Choose csv File')),
                                                                   accept=c('text/csv', 
                                                                            'text/comma-separated-values,text/plain', 
                                                                            '.csv')),
                                checkboxInput('header', 'Header', TRUE),
                                radioButtons('sep', 'Separator',
                                             c(Comma=',',
                                               Semicolon=';',
                                               Tab='\t'),
                                             ','),
                                radioButtons('quote', 'Quote',
                                             c(None='',
                                               'Double Quote'='"',
                                               'Single Quote'="'"),
                                             '"'))
               ,
               numericInput("Col",strong("국가선택 - 열(Column)"),value = 2)
               
               ,
               
               h4(strong("Time Series Setting")),
               numericInput("Start",strong("Start Period"),value = 2010),
               
               numericInput("freq",strong("Frequency"),value=12),
               h4(strong("Holdout Sample Setting")),
               numericInput("Starth",strong("Start Period"),value = 2019),
               numericInput("Endh",strong("End Period"),value = 2020)
             ),
             
             
             mainPanel(fluidRow(
               
               column(10,h3("Summary"),verbatimTextOutput("summary")),
               column(10,h3("Table"),dataTableOutput("table")),
               column(10,h3("Plot"),plotOutput("PlotG"))
               
             )
             
             )
             
           )),
  
  tabPanel("Naive Method",
           sidebarLayout(
             sidebarPanel(
               
               'Naive Method Does not Need any Parameter.'
             ),
             
             mainPanel(
               fluidRow(
                 h3('Model Introduction'),
                 p('Naive forecasts are the most cost effective forecasting model. Generally, it just use the value of past to predict the near future',align='justify'),
                 
                 column(10,h4(strong('Forecasting Plot')),plotOutput("Plot0")),
                 
                 column(10,h4(strong('Accuracy Table')),tableOutput("accu0")),
                 
                 column(10,h4(strong('Accuracy Bar Plot')),plotOutput("Plot00"))
               )
             )
           ))
  ,
  navbarMenu("Smoothing Method",       
             tabPanel("Simple Exponential Smoothing",
                      sidebarLayout(
                        sidebarPanel(
                          
                          sliderInput("CI",label="Confience Interval",min=0.01,max=0.99,value=0.9)
                          ,
                          radioButtons("F2",h4(strong("Determinant or Optimal")),choices = list("Optimal"=1,"Determinant"=2),selected = 1)
                          ,
                          conditionalPanel(condition="input.F2==2",sliderInput("AlphaS","Your Alpha Value",min=0,max=1,value=0.2))
                        ),
                        mainPanel(
                          fluidRow(
                            h3('Model Introduction'),
                            p('Simple exponential smoothing. The simplest of the exponentially smoothing methods is naturally called simple exponential smoothing (SES). (In some books, it is called single exponential smoothing.) This method is suitable for forecasting data with no trend or seasonal pattern.',align='justify'),
                            
                            column(10,h4(strong('Forecasting Plot')),plotOutput("Plot1")),
                            
                            column(10,h4(strong('Accuracy Table')),tableOutput("accu1")),
                            column(10,h4(strong('Accuracy Bar Plot')),plotOutput("Plot2"))
                          )
                        )
                      )),
             tabPanel("Linear Exponential Smoothing",
                      sidebarLayout(
                        (sidebarPanel(
                          
                          
                          sliderInput("CI1",label="Confience Interval",min=0.01,max=0.99,value=0.9)
                          ,
                          radioButtons("F4",h4(strong("Determinant or Optimal")),choices = list("Optimal"=1,"Determinant"=2),selected = 1)
                          ,
                          conditionalPanel(condition="input.F4==2",sliderInput("AlphaL","Your Alpha Value",min=0,max=1,value=0.2),sliderInput("BetaL","Your Beta Value",min = 0,max = 1,value = 0.2))
                        )
                        ),
                        mainPanel(
                          fluidRow(
                            h3('Model Introduction'),
                            p('Holt (1957) extended simple exponential smoothing to allow forecasting of data with a trend. This method involves a forecast equation and two smoothing equations (one for the level and one for the trend)',align='justify'),
                            
                            column(10,h4(strong('Forecasting Plot')),plotOutput("Plot3")),
                            
                            column(10,h4(strong('Accuracy Table')),tableOutput("accu2")),
                            column(10,h4(strong('Accuracy Bar Plot')),plotOutput("Plot4"))
                          )
                        )
                      )),
             tabPanel("Holt Winter Method",
                      sidebarLayout(
                        sidebarPanel(
                          
                          sliderInput("CI2",label="Confience Interval",min=0.01,max=0.99,value=0.9)
                          ,
                          radioButtons("F6",h4(strong("Determinant or Optimal")),choices = list("Optimal"=1,"Determinant"=2),selected = 1),
                          radioButtons("AM",h4(strong("Additive or Multiplicative")),choices=list("Additive"=1,"Multiplicative"=2),selected=1)
                          ,
                          conditionalPanel(condition="input.F6==2",sliderInput("AlphaH","Your Alpha Value",min=0,max=1,value=0.2),sliderInput("BetaH","Your Beta Value",min = 0,max = 1,value = 0.2),sliderInput("GammaH","Your Gamma Value",min=0,max=1,value=0.2))
                          
                          
                        ),
                        mainPanel(
                          fluidRow(
                            h3('Model Introduction'),
                            p('Holt (1957) and Winters (1960) extended Holt method to capture seasonality. The Holt Winters seasonal method comprises the forecast equation and three smoothing equations, one for the level, one for trend, and one for the seasonal component',align='Justify'),
                            column(10,h4(strong('Forecasting Plot')),plotOutput("Plot5")),
                            
                            column(10,h4(strong('Accuracy Table')),tableOutput("accu3")),
                            column(10,h4(strong('Accuracy Bar Plot')),plotOutput("Plot6"))
                          )
                        )
                      ))),
  tabPanel("Regression Method ",
           sidebarLayout(
             sidebarPanel(
               radioButtons("Trans",h4(strong("Data Transformation")),choices = list("Normal"=1,"Logarithm"=2,"Powered"=3),selected = 1),
               
               sliderInput("CI3","Confidence Interval",min = 0.01,max = 0.99,value = 0.9)
               
             ),
             mainPanel(
               fluidRow(
                 h3('Model Introduction'),
                 p('In statistical modeling, regression analysis is a statistical process for estimating the relationships among variables. It includes many techniques for modeling and analyzing several variables, when the focus is on the relationship between a dependent variable and one or more independent variables (or predictors).',align='Justify'),
                 column(10,h4(strong('Forecasting Plot')),plotOutput("Plot7")),
                 
                 column(10,h4(strong('Accuracy Table')),tableOutput("accu4")),
                 column(10,h4(strong('Accuracy Bar Plot')),plotOutput("plot8"))
               )
             )
           )),
  
  tabPanel("Neural Network",
           sidebarLayout(
             sidebarPanel(
               
               
               sliderInput("CI4",label="Confience Interval",min=0.01,max=0.99,value=0.9)
               
             ),
             
             mainPanel(
               fluidRow(
                 h3('Model Introduction'),
                 p('A neural network usually involves a large number of processors operating in parallel and arranged in tiers. The first tier receives the raw input information -- analogous to optic nerves in human visual processing. Each successive tier receives the output from the tier preceding it, rather than from the raw input -- in the same way neurons further from the optic nerve receive signals from those closer to it. The last tier produces the output of the system.',align='Justify'),
                 column(10,h4(strong('Forecasting Plot')),plotOutput("Plot9")),
                 column(10,h4(strong('Accuracy Table')),tableOutput("accu5")),
                 column(10,h4(strong('Accuracy Bar Plot')),plotOutput("Plot10"))
               )
             )
           )),
  tabPanel("ARIMA Method",
           sidebarLayout(
             sidebarPanel(
               
               
               sliderInput("CI5",label="Confience Interval",min=0.01,max=0.99,value=0.9)
               
             ),
             
             mainPanel(
               fluidRow(
                 h3('Model Introduction'),
                 p('In statistics and econometrics, and in particular in time series analysis, an autoregressive integrated moving average (ARIMA) model is a generalization of an autoregressive moving average (ARMA) model. These models are fitted to time series data either to better understand the data or to predict future points in the series (forecasting). They are applied in some cases where data show evidence of non-stationarity, where an initial differencing step (corresponding to the "integrated" part of the model) can be applied to reduce the non-stationarity.',align='Justify'),
                 
                 column(10,h4(strong('Forecasting Plot')),plotOutput("Plot11")),
                 column(10,h4(strong('Accuracy Table')),tableOutput("accu6")),
                 column(10,h4(strong('Accuracy Bar Plot')),plotOutput("Plot12"))
               )
             )
           )),
  tabPanel("모델개요",
           h3(strong("Introduction")),
           br(),
           p("한국을 방문한 외래관광객 수요예측 연습 (한국문화관광연구원 관광지식정보시스템 2010년 1월부터 2019년 12월 인바운드(120개 월별 통계).",align="Justify"),
           p("7개 관광수요 예측 모델별 기초통계 및 모형 적합도 결과(Accuracy Table and Bar plot).",align="Justify"),
           p("7개 예측모델은 R 수요예측기법과 Shinyapp 도구 활용.",align="Justify"),
           
           p("국가별 데이터 선택(1), 개인 데이터 업로드(2). 업로드 형식: 변수명, 시계열 자료",align="Justify"),
           br(),
           h3(strong("작성자")),
           p("Henry H.J. Joun Email: hyojae.joun@gmail.com"),
           p("Researcher Story Economy and tourism industry in Korea"),
           p(""),
           a(h5("참고한 사이트 Author: Jiashen Liu => Code Here"),href="https://github.com/liujiashen9307/Forcast",target="_blank")
  )
  
)
)



