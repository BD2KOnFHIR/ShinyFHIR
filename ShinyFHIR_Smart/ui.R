library(shiny)
library(dygraphs)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

	# Application title
	titlePanel("Shiny FHIR!"),

	# Sidebar with controls to select a dataset and specify the number
	# of observations to view
	sidebarPanel(
		tabsetPanel(id="InputPanel",
			tabPanel(title = "PatientCentered",value="panel1", uiOutput('PatientUI'), uiOutput('Analysis'), uiOutput('Dataset'), uiOutput('AnalysisSub')),
			tabPanel(title = "CohortBased",value="panel2", uiOutput('Coho_category'), uiOutput('Coho_Analysis'), uiOutput('Coho_Dataset'), uiOutput('Coho_AnalysisSub'))
		)
	),

	# Show a summary of the dataset and an HTML table with the requested
	# number of observations
	mainPanel(
		tabsetPanel(id="OutputPanel",
			tabPanel(title = "QueryResult",value="panel1",uiOutput('table')),
			tabPanel(title = "Plot",value="panel2",plotOutput('plot')),
			tabPanel(title = "Graph",value="panel3",dygraphOutput('dygraph'))
		)
	) 	

))
					
