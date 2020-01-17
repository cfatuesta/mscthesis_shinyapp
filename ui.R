ui = bootstrapPage(fluidPage(
  theme = shinytheme("cyborg"),
  titlePanel(
    'Seizure outcome after beginning AED withdrawal after epilepsy surgery'
  ),
  p("This clinical tool permits individualised risk assessment of postsurgical AED withdrawal. It is meant to be used only by medical professionals to improve prognostication of seizure outcome after AED withdrawal after epilepsy surgery.", style=" padding:10px; color: lightgrey; height: 60px"),
  p("We invite you to read the provided information before using this tool:", style="padding:20px; " ),
  actionButton('showEligibility', 'Eligibility', style="color: black; background-color: lightblue; border-color: white"),
  actionButton('showLimitations', 'Limitations', style="color: black; background-color: lightblue; border-color: white"),
  actionButton('showPerformance', 'Performance', style="color: black; background-color: lightblue; border-color: white"),
  actionButton('readArticle', 'Reference Manuscript', style="color: black; background-color: lightblue; border-color: white", onclick="window.open('https://drive.google.com/file/d/132swAABQjhD7AIn7J06eO3WiZSxk6UQT/view?usp=sharing', '_blank')"),
  p("Instructions on how to use this tool can be found here:", style=" padding:20px; color: lightgrey;"),
  actionButton('showHowtouse', "How to use",style="color: #49484B; background-color:#D0CBDC; border-color: white"),
  br(),
  br(), 
  sidebarLayout(
    sidebarPanel( 
      uiOutput('manySliders.f'),
      uiOutput('manySliders.n'),
      checkboxInput('trans', 'Alpha blending (transparency)', value = TRUE),
      actionButton('add', 'Predict', style="color: white; background-color: green; border-color: white"),
      style="color: black; background-color: 	#298AAE ; border-color: black"),
    mainPanel(
      tabsetPanel(
        id = 'tabs',
        tabPanel('Predicted survival over time', plotOutput('plot')),
        tabPanel('95% confidence interval', plotlyOutput('plot2')),
        tabPanel('Numerical Summary', verbatimTextOutput('data.pred')),
        tabPanel('Model Summary', verbatimTextOutput('summary'))
        
      )
    )
  )
))
