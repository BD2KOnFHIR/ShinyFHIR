library(shiny)
library(jsonlite)
library(dygraphs)
library(xts)

session_data<-data.frame()
session_final_id<-c()


#Get FHIR server data
HAPI_observation<-data.frame()
HAPI_patient<-data.frame()
HAPI_condition<-data.frame()
HAPI_procedure<-data.frame()

#connect to Fhirbase
library(RPostgreSQL)
drv=dbDriver("PostgreSQL")
#test: Ovarian Cancer Data; fhirbase_pubtest: smart_on_fhir Data
con=dbConnect(drv,host="localhost",port="5432",user="postgres",dbname="test",password="postgres")

#Get Patient
{
   length=dbGetQuery(con,"SELECT count(content)  FROM patient")
  len=as.numeric(length)
  out=dbGetQuery(con,"SELECT content  FROM patient")
   i=1

  while(i<len+1)  
   {
     myobj<-fromJSON(out[i,1])
     print(i);
    # print(myobj)
      id<-as.vector(myobj$id)
      print(id)
      gender<-as.vector(myobj$gender)
      bod<-as.character(myobj$birthDate)
      print(gender)
      print(bod)
  
      isdeceased<-as.character(myobj$deceasedBoolean)
      if(is.null(myobj$deceasedBoolean)){
        isdeceased="alive";
      }
     # print(isdeceased)
      bod<-substr(bod,0,4)
      bod<-as.numeric(bod)
      age<-2016-bod
      
    #  print(age)
      HAPI_patient<-rbind(HAPI_patient,cbind(id,gender,age,isdeceased))
    #  HAPI_patient<-rbind(HAPI_patient,cbind(id,gender,age))
    #  print(HAPI_patient)
      i=i+1
   }  
    
    HAPI_patient[HAPI_patient=="NULL"]<-NA
    colnames(HAPI_patient)<-c("patientid","gender","age","status")
  #  print(class(HAPI_patient))
    HAPI_patient<-as.data.frame(HAPI_patient)
    HAPI_patient<-HAPI_patient[!duplicated(HAPI_patient),]
    HAPI_patient<-HAPI_patient[which(!is.na(HAPI_patient$patientid)),]
    
  }
  
 


#Get Observation

{
 # length=dbGetQuery(con,"SELECT count(content)  FROM observation")
  length=dbGetQuery(con,"SELECT count(content) FROM observation WHERE content @> '{\"code\":{\"coding\":[{\"system\":\"http://loinc.org\",\"code\":\"2339-0\",\"display\":\"Glucose Bld-mCnc\"}],\"text\":\"Glucose Bld-mCnc\"}}'");
  len=as.numeric(length)
  print(len)
# out=dbGetQuery(con,"SELECT content  FROM observation")
  out=dbGetQuery(con,"SELECT content FROM observation WHERE content @> '{\"code\":{\"coding\":[{\"system\":\"http://loinc.org\",\"code\":\"2339-0\",\"display\":\"Glucose Bld-mCnc\"}],\"text\":\"Glucose Bld-mCnc\"}}'");
  
  print(out)
  i=1
  while(i<len+1)  
 # while(i<11)
  {
    
    myobj<-fromJSON(out[i,1])
   
          id<-as.vector(myobj$id)
       #   print(id)
          pid<-as.vector(myobj$subject$reference)
        # print(pid)
          pid=substr(pid,9,45)
        #  print(pid)
         # pid<-substr(pid, 9,length(pid))
         
          value<-as.vector(myobj$valueQuantity$value)
          print(value)
          system<-as.vector(myobj$valueQuantity$system)
          print(system)
          unit<-as.vector(myobj$valueQuantity$unit)
          print(unit)
          valuecode<-as.vector(myobj$valueQuantity$code)
          if(is.null(myobj$valueQuantity$code)){
            valuecode="none";
          }
          print(valuecode)
          display<-as.vector(myobj$code$coding$display)
          print(display)
        # obcode<-as.vector(lapply(myobj$code$coding, FUN = `[[`, "code"))
          obcode<-as.vector(myobj$code$coding$code)
          print(obcode)
          sys<-as.vector(myobj$code$coding$system)
          print(sys)
        #  effectivePeriod_start<-as.vector(myobj$effectivePeriod$start)
          effectivePeriod_start<-as.vector(myobj$effectiveDateTime)
          print(effectivePeriod_start)
          HAPI_observation<-rbind(HAPI_observation,cbind(id,pid,display,obcode,sys,system,valuecode,value,unit,effectivePeriod_start))
      

    i=i+1
  }  
 
      HAPI_observation[HAPI_observation=="NULL"]<-NA
      colnames(HAPI_observation)<-c("observationid","patientid","codedisplay","code","codesystem","valueQuantitysystem","valueQuantitycode","valueQuantityvalue","valueQuantityunit","effectivedatetime")
      HAPI_observation<-as.data.frame(HAPI_observation)
      HAPI_observation<-HAPI_observation[!duplicated(HAPI_observation),]
      HAPI_observation<-HAPI_observation[which(!is.na(HAPI_observation$codedisplay)),]

}



# #Get Condition
# {
#   length=dbGetQuery(con,"SELECT count(content)  FROM condition")
#   len=as.numeric(length)
#   print(len)
#   out=dbGetQuery(con,"SELECT content  FROM condition")
#   #print(out)
#   i=1
#   while(i<len+1)  
#     # while(i<11)
#   {
#     
#     myobj<-fromJSON(out[i,1])
#     
#     id<-as.vector(myobj$id)
#   
#     pid<-as.vector(myobj$patient$reference)
#   
#     pid=substr(pid,9,45)
#     #pid<-substr(pid, 9,length(pid))
#    
#     display<-as.vector(myobj$code$coding$display)
#     # obcode<-as.vector(lapply(myobj$code$coding, FUN = `[[`, "code"))
#     obcode<-as.vector(myobj$code$coding$code)
#     sys<-as.vector(myobj$code$coding$system)
# 
#     onsetdatetime<-as.vector(myobj$onsetDateTime)
#     HAPI_condition<-rbind(HAPI_condition,cbind(id,pid,display,obcode,sys,onsetdatetime))
#     
#     
#     i=i+1
#   }  
#   
#   HAPI_condition[HAPI_condition=="NULL"]<-NA
#   colnames(HAPI_condition)<-c("conditionid","patientid","codedisplay","code","codesystem","onsetdatetime")
#   HAPI_condition<-as.data.frame(HAPI_condition)
#   HAPI_condition<-HAPI_condition[!duplicated(HAPI_condition),]
#   HAPI_condition<-HAPI_condition[which(!is.na(HAPI_condition$codedisplay)),]
#   
# }


 #Get Procedure
# {
#   
#   length=dbGetQuery(con,"SELECT count(content)  FROM procedure")
#   len=as.numeric(length)
#   print(len)
#   out=dbGetQuery(con,"SELECT content  FROM procedure")
#   #print(out)
#   i=1
#   while(i<len+1)  
#     # while(i<11)
#   {
#     
#     myobj<-fromJSON(out[i,1])
#     
#     id<-as.vector(myobj$id)
#    
#     pid<-as.vector(myobj$subject$reference)
# 
#     pid=substr(pid,9,45)
#     #pid<-substr(pid, 9,length(pid))
#    
#     valuecode<-as.vector(myobj$valueQuantity$code)
#     display<-as.vector(myobj$code$coding$display)
#     # obcode<-as.vector(lapply(myobj$code$coding, FUN = `[[`, "code"))
#     obcode<-as.vector(myobj$code$coding$code)
#     sys<-as.vector(myobj$code$coding$system)
#     
#     performeddatetime<-as.vector(myobj$performedDateTime)
#     HAPI_procedure<-rbind(HAPI_procedure,cbind(id,pid,display,obcode,sys,performeddatetime))
#     
#     
#     i=i+1
#   }  
#   
#   HAPI_procedure[HAPI_procedure=="NULL"]<-NA
#   colnames(HAPI_procedure)<-c("procedureid","patientid","codedisplay","code","codesystem","performeddatetime")
#   HAPI_procedure<-as.data.frame(HAPI_procedure)
#   HAPI_procedure<-HAPI_procedure[!duplicated(HAPI_procedure),]
#   HAPI_procedure<-HAPI_procedure[which(!is.na(HAPI_procedure$codedisplay)),]
#   
#   
# }




HAPI_patient <- as.data.frame(lapply(HAPI_patient, function(X) unname(unlist(X))))
HAPI_observation <- as.data.frame(lapply(HAPI_observation, function(X) unname(unlist(X))))
# HAPI_condition <- as.data.frame(lapply(HAPI_condition, function(X) unname(unlist(X))))
# HAPI_procedure <- as.data.frame(lapply(HAPI_procedure, function(X) unname(unlist(X))))
# print(HAPI_patient)
# print(HAPI_observation)
# print(HAPI_condition)
# print(HAPI_procedure)

#####################################################################################################################
#load("data.rda")
shinyServer(function(input, output, session) {
  
  
  #updateSelectizeInput(session, 'patientid', choices = HAPI_patient$patientid, server = TRUE)
  #updateSelectInput(session, 'patientid', choices = HAPI_patient$patientid)
  outSuboptions <- reactive({
    if(input$InputPanel == "panel1")
    {
      
      HAPI_patient$patientid <- as.character(HAPI_patient$patientid)
     # HAPI_patient$patientid <- as.integer(HAPI_patient$patientid)
      return(selectizeInput('patientid','PATIENT',  choices = c("Select",HAPI_patient$patientid),multiple = FALSE))
    }
  })
  
  output$PatientUI = renderUI({
    outSuboptions()
  })
  
  outAnalysis <- reactive({
    if( input$InputPanel == "panel1" && !is.null(input$patientid) && input$patientid != "Select")
    {
      return(selectInput('result_analysis', 'Choose a Category',as.list(c("Select","Observation","Procedure","Condition"))))
    }
  })
  
  output$Analysis = renderUI({
    outAnalysis()
  })
  
  outDataset <- reactive({
    if(input$InputPanel == "panel1" && !is.null(input$patientid) && input$patientid > 0 && !is.null(input$result_analysis) && input$result_analysis =="Observation")
    {
      tmp<-unique(HAPI_observation[which(HAPI_observation$patientid == input$patientid),"codedisplay"])
      return(selectInput('obs_analysis', 'Choose a Observation', as.list(c("Select",as.character(tmp)))))
    }
    if(input$InputPanel == "panel1" && !is.null(input$patientid) && input$patientid > 0 && !is.null(input$result_analysis) && input$result_analysis =="Condition")
    {
      tmp<-unique(HAPI_condition[which(HAPI_condition$patientid ==input$patientid),"codedisplay"])
      return(selectInput('cat_analysis', 'Choose a Category', as.list(c("Select",as.character(tmp)))))
    }
    if(input$InputPanel == "panel1" && !is.null(input$patientid) && input$patientid > 0 && !is.null(input$result_analysis) && input$result_analysis =="Procedure")
    {
      tmp<-unique(HAPI_procedure[which(HAPI_procedure$patientid ==input$patientid),"codedisplay"])
      return(selectInput('pro_analysis', 'Choose a Procedure', as.list(c("Select",as.character(tmp)))))
    }
  })
  
  output$Dataset = renderUI({
    outDataset()
  })
  
  outQueryTable <- reactive({
    if(input$InputPanel == "panel1" && !is.null(input$patientid) && input$patientid > 0 && !is.null(input$result_analysis) && input$result_analysis =="Observation" && !is.null(input$obs_analysis) && input$obs_analysis !="Select" )	
    {
      tmp<-as.data.frame(HAPI_observation[which(HAPI_observation$patientid == input$patientid & HAPI_observation$codedisplay==input$obs_analysis ),])
      return(tmp)
    }
    if(input$InputPanel == "panel1" && !is.null(input$patientid) && input$patientid > 0 && !is.null(input$result_analysis) && input$result_analysis =="Condition" && !is.null(input$cat_analysis) && input$cat_analysis !="Select" )	
    {
      tmp<-as.data.frame(HAPI_condition[which(HAPI_condition$patientid == input$patientid & HAPI_condition$codedisplay==input$cat_analysis ),])
      return(tmp)
    }
    if(input$InputPanel == "panel1" && !is.null(input$patientid) && input$patientid > 0 && !is.null(input$result_analysis) && input$result_analysis =="Procedure" && !is.null(input$pro_analysis) && input$pro_analysis !="Select" )	
    {
      tmp<-as.data.frame(HAPI_procedure[which(HAPI_procedure$patientid == input$patientid & HAPI_procedure$codedisplay==input$pro_analysis ),])
      return(tmp)
    }
   # print(input$coho_category)
    #print(input$coho_obs_analysis)
    if(input$InputPanel == "panel2" && !is.null(input$coho_category) && input$coho_category =="Observation" && !is.null(input$coho_obs_analysis) && input$coho_obs_analysis !="Select"  && !is.null(input$coho_pat_char) && input$coho_pat_char =="age")
    {
      tmp<-unique(HAPI_observation[which(HAPI_observation$codedisplay==input$coho_obs_analysis),])
      print(tmp)
      print(HAPI_patient)
      print(typeof(tmp))
      print(typeof(HAPI_patient))
      tmp1<-merge(tmp,HAPI_patient,by="patientid",all.x=TRUE)
      print(tmp1)
      col<-c(colnames(tmp),input$coho_pat_char)
      tmp1<-as.data.frame(tmp1[,col])
      return(tmp1)
    }
    if(input$InputPanel == "panel2" && !is.null(input$coho_category) && input$coho_category =="Observation" && !is.null(input$coho_obs_analysis) && input$coho_obs_analysis !="Select"  && !is.null(input$coho_pat_char) && input$coho_pat_char =="gender")
    {
      tmp<-unique(HAPI_observation[which(HAPI_observation$codedisplay==input$coho_obs_analysis),])
      print(tmp)
      print(HAPI_patient)
      print(typeof(tmp))
      print(typeof(HAPI_patient))
      tmp1<-merge(tmp,HAPI_patient,by="patientid",all.x=TRUE)
      print(tmp1)
      col<-c(colnames(tmp),input$coho_pat_char)
      tmp1<-as.data.frame(tmp1[,col])
      return(tmp1)
    }
    if(input$InputPanel == "panel2" && !is.null(input$coho_category) && input$coho_category =="Observation" && !is.null(input$coho_obs_analysis) && input$coho_obs_analysis !="Select"  && !is.null(input$coho_pat_char) && input$coho_pat_char =="survival analysis")
    {
      tmp<-unique(HAPI_observation[which(HAPI_observation$codedisplay==input$coho_obs_analysis),])
      print(tmp)
      print(HAPI_patient)
      print(typeof(tmp))
      print(typeof(HAPI_patient))
      tmp1<-merge(tmp,HAPI_patient,by="patientid",all.x=TRUE)
      print(tmp1)
      col<-c(colnames(tmp),"gender","age","status")
      tmp1<-as.data.frame(tmp1[,col])
      return(tmp1)
    }
    if(input$InputPanel == "panel2" && !is.null(input$coho_category) && input$coho_category =="Procedure" && !is.null(input$coho_pro_analysis) && input$coho_pro_analysis !="Select"  && !is.null(input$coho_pat_char) && input$coho_pat_char !="Select")
    {
      tmp<-unique(HAPI_procedure[which(HAPI_procedure$codedisplay==input$coho_pro_analysis),])
      tmp1<-merge(tmp,HAPI_patient,by="patientid",all.x=TRUE)
      col<-c(colnames(tmp),input$coho_pat_char)
      tmp1<-as.data.frame(tmp1[,col])
      return(tmp1)
    }
    if(input$InputPanel == "panel2" && !is.null(input$coho_category) && input$coho_category =="Condition" && !is.null(input$coho_con_analysis) && input$coho_con_analysis !="Select"  && !is.null(input$coho_pat_char) && input$coho_pat_char !="Select")
    {
      tmp<-unique(HAPI_condition[which(HAPI_condition$codedisplay==input$coho_con_analysis),])
      tmp1<-merge(tmp,HAPI_patient,by="patientid",all.x=TRUE)
      col<-c(colnames(tmp),input$coho_pat_char)
      tmp1<-as.data.frame(tmp1[,col])
      return(tmp1)
    }
  })
  output$table <- renderTable({
    outQueryTable()
  })
  outPlot <- reactive({
    if(input$InputPanel == "panel1" && !is.null(input$patientid) && input$patientid > 0 && !is.null(input$result_analysis) && input$result_analysis =="Observation" && !is.null(input$obs_analysis) && input$obs_analysis !="Select" )	
    {
       print(input$patientid)
       print(input$obs_analysis)
       print(HAPI_observation)
      tmp<-as.vector(HAPI_observation[which(HAPI_observation$patientid == input$patientid & HAPI_observation$codedisplay==input$obs_analysis ),"valueQuantityvalue"])
      tmp1<-as.vector(HAPI_observation[which(HAPI_observation$patientid == input$patientid & HAPI_observation$codedisplay==input$obs_analysis ),"effectivedatetime"])
       print(tmp)
      tmp<-as.numeric(as.character(tmp))
      tmp1<-as.character(tmp1)
      # print(tmp)
      # print(tmp1)
      if(length(tmp)>1)
      {
        par(mfrow = c(2,1))
        hist(tmp,main=paste("Observation: ",input$obs_analysis ," Distribution for Patient ",input$patientid,sep=""),xlab=input$obs_analysis,ylab="Number of Observations")
        tmp1 <- as.Date(tmp1, "%Y-%m-%d")
        print(tmp1)
        ord<-order(tmp1)
        tmp1<-tmp1[ord]
        tmp<-tmp[ord]
       
        plot(tmp ~ tmp1,  xaxt = "n", type = "l",xlab="Date",ylab="Value")
     
        axis.Date(side = 1, tmp1, format = "%d/%m/%Y")
      }
      
      
    }
    if(input$InputPanel == "panel2" && !is.null(input$coho_category) && input$coho_category =="Observation" && !is.null(input$coho_obs_analysis) && input$coho_obs_analysis !="Select"  && !is.null(input$coho_pat_char) && input$coho_pat_char =="age")
    {
      tmp<-unique(HAPI_observation[which(HAPI_observation$codedisplay==input$coho_obs_analysis),])
      tmp1<-merge(tmp,HAPI_patient,by="patientid",all.x=TRUE)
      col<-c(colnames(tmp),input$coho_pat_char)
      tmp1<-as.data.frame(tmp1[,col])
      tmp1<-tmp1[,c("valueQuantityvalue",input$coho_pat_char)]
      colnames(tmp1)<-c("value","char")
      tmp1<-tmp1[with(tmp1, order(char)), ]
      #print(tmp1)
      plot(tmp1[,1] ~ tmp1[,2],   type = "l",xlab=input$coho_pat_char,ylab=paste("Observation Value ",input$coho_obs_analysis,sep=""),main=paste(input$coho_obs_analysis," VS ",input$coho_pat_char,sep=""))
      #print(dim(tmp1))
      #boxplot(tmp1[,1]~tmp1[,2], main=paste("Box plot of ",input$coho_obs_analysis," Characterstic ",input$coho_pat_char,sep=""), xlab=input$coho_pat_char, ylab=paste("Observation Value ",input$coho_obs_analysis,sep=""))
    }
    if(input$InputPanel == "panel2" && !is.null(input$coho_category) && input$coho_category =="Observation" && !is.null(input$coho_obs_analysis) && input$coho_obs_analysis !="Select"  && !is.null(input$coho_pat_char) && input$coho_pat_char =="gender")
    {
      tmp<-unique(HAPI_observation[which(HAPI_observation$codedisplay==input$coho_obs_analysis),])
      tmp1<-merge(tmp,HAPI_patient,by="patientid",all.x=TRUE)
      col<-c(colnames(tmp),input$coho_pat_char)
      tmp1<-as.data.frame(tmp1[,col])
      tmp1<-tmp1[,c("valueQuantityvalue",input$coho_pat_char)]
      colnames(tmp1)<-c("valueQuantityvalue",input$coho_pat_char)
      #print(dim(tmp1))
     # boxplot(tmp1[,1]~tmp1[,2], main=paste("Box plot of ",input$coho_obs_analysis," Characterstic ",input$coho_pat_char,sep=""), xlab=input$coho_pat_char, ylab=paste("Observation Value ",input$coho_obs_analysis,sep=""))
     # boxplot(tmp1[,1]~tmp1[,2],)
      plot(tmp1[,1] ~ tmp1[,2],   type = "l",xlab=input$coho_pat_char,ylab=paste("Observation Value ",input$coho_obs_analysis,sep=""),main=paste(input$coho_obs_analysis," VS ",input$coho_pat_char,sep=""))
      
    }
    if(input$InputPanel == "panel2" && !is.null(input$coho_category) && input$coho_category =="Procedure" && !is.null(input$coho_pro_analysis) && input$coho_pro_analysis !="Select"  && !is.null(input$coho_pat_char) && input$coho_pat_char =="gender")
    {
      tmp<-unique(HAPI_procedure[which(HAPI_procedure$codedisplay==input$coho_pro_analysis),])
      tmp1<-merge(tmp,HAPI_patient,by="patientid",all.x=TRUE)
      col<-c(colnames(tmp),input$coho_pat_char)
      tmp1<-as.data.frame(tmp1[,col])
      tmp1<-unique(tmp1[,c("patientid",input$coho_pat_char)])
      slices <- as.numeric(table(tmp1[,2]))
      lbls <- names(table(tmp1[,2]))
      pct <- round(slices/sum(slices)*100)
      lbls <- paste(lbls, pct) # add percents to labels 
      lbls <- paste(lbls,"%",sep="") # ad % to labels 
      pie(slices,labels = lbls, col=rainbow(length(lbls)),main=paste("Pie Chart of ",input$coho_pro_analysis," Characterstic ",input$coho_pat_char,sep=""))
    }
    if(input$InputPanel == "panel2" && !is.null(input$coho_category) && input$coho_category =="Procedure" && !is.null(input$coho_pro_analysis) && input$coho_pro_analysis !="Select"  && !is.null(input$coho_pat_char) && input$coho_pat_char =="age")
    {
      tmp<-unique(HAPI_procedure[which(HAPI_procedure$codedisplay==input$coho_pro_analysis),])
      tmp1<-merge(tmp,HAPI_patient,by="patientid",all.x=TRUE)
      col<-c(colnames(tmp),input$coho_pat_char)
      tmp1<-as.data.frame(tmp1[,col])
      tmp1<-unique(tmp1[,c("patientid",input$coho_pat_char)])
      #print(tmp1[,2])
      hist(as.numeric(as.character(tmp1[,2])),main=paste(input$coho_pat_char," Distribution for ",input$coho_pro_analysis,sep=""),xlab=input$coho_pat_char,ylab="Number of Patients")
    }		
    if(input$InputPanel == "panel2" && !is.null(input$coho_category) && input$coho_category =="Condition" && !is.null(input$coho_con_analysis) && input$coho_con_analysis !="Select"  && !is.null(input$coho_pat_char) && input$coho_pat_char =="gender")
    {
      tmp<-unique(HAPI_condition[which(HAPI_condition$codedisplay==input$coho_con_analysis),])
      tmp1<-merge(tmp,HAPI_patient,by="patientid",all.x=TRUE)
      col<-c(colnames(tmp),input$coho_pat_char)
      tmp1<-as.data.frame(tmp1[,col])
      tmp1<-unique(tmp1[,c("patientid",input$coho_pat_char)])
      slices <- as.numeric(table(tmp1[,2]))
      lbls <- names(table(tmp1[,2]))
      pct <- round(slices/sum(slices)*100)
      lbls <- paste(lbls, pct) # add percents to labels 
      lbls <- paste(lbls,"%",sep="") # ad % to labels 
      pie(slices,labels = lbls, col=rainbow(length(lbls)),main=paste("Pie Chart of ",input$coho_con_analysis," Characterstic ",input$coho_pat_char,sep=""))
    }
    if(input$InputPanel == "panel2" && !is.null(input$coho_category) && input$coho_category =="Condition" && !is.null(input$coho_con_analysis) && input$coho_con_analysis !="Select"  && !is.null(input$coho_pat_char) && input$coho_pat_char =="age")
    {
      tmp<-unique(HAPI_condition[which(HAPI_condition$codedisplay==input$coho_con_analysis),])
      tmp1<-merge(tmp,HAPI_patient,by="patientid",all.x=TRUE)
      col<-c(colnames(tmp),input$coho_pat_char)
      tmp1<-as.data.frame(tmp1[,col])
      tmp1<-unique(tmp1[,c("patientid",input$coho_pat_char)])
      print(tmp1[,2])
      hist(as.numeric(as.character(tmp1[,2])),main=paste(input$coho_pat_char," Distribution for ",input$coho_con_analysis,sep=""),xlab=input$coho_pat_char,ylab="Number of Patients")
    }
    if(input$InputPanel == "panel2" && !is.null(input$coho_category) && input$coho_category =="Observation" && !is.null(input$coho_obs_analysis) && input$coho_obs_analysis !="Select" && !is.null(input$coho_pat_char) && input$coho_pat_char =="survival analysis")
    {
      tmp<-unique(HAPI_observation[which(HAPI_observation$codedisplay==input$coho_obs_analysis),])
      tmp2<-merge(tmp,HAPI_patient,by="patientid",all.x=TRUE)
      # print(tmp1)
      
      tmp1<-merge(tmp2,HAPI_procedure,by="patientid",all.x=TRUE)
      #print(tmp2)
      col<-c(colnames(tmp1))
      tmp2<-as.data.frame(tmp1[,col])
      print(tmp2)
      len<-length(tmp1[[1]])
      #     print(len)
      followup<-c()
      survival<-data.frame()
      i<-1
      j<-1
      while(i<len+1){
     # while(i<100){
        #print(tmp1[i])
        #print(tmp1[i,10])
        # print(tmp1[i,17])
        startchemodate<-tmp1[i,18]
        
       # print(startchemodate)
        obserdate<-tmp1[i,10]
        # print(obserdate)
        
        
        monnb <- function(d) { lt <- as.POSIXlt(as.Date(d, origin="1900-01-01")); 
        lt$year*12 + lt$mon } 
        
        mondf <- function(d1, d2) { monnb(d2) - monnb(d1) }
        followup[i]<-mondf(startchemodate,obserdate)
        # print(followup[i])
        
        folowupmonth<-followup[i]
        
        # print(folowupmonth)
        # print(tmp1[i,8])
        if( !any(is.na(folowupmonth)) && any(folowupmonth > 0)){
          
          survival[j,1]<-tmp1[i,8]
          
          survival[j,2]<-folowupmonth
          survival[j,3]<-tmp1[i,13]
          
          # print(stop)
          j<-j+1
        }
        i<-i+1
      }
      
      # print(survival)
      
      #  survival<-head(survival, 20)
        print(survival)
      library(survival)
      hem<-c()
      follow<-c()
      stat<-c()
      hem <- as.numeric(as.character(survival[,1]))
      # print(class(hem))
      # print(typeof(hem))
      # print(hem)
      follow <- as.numeric(survival[,2])
      # print(follow)
      stat <- as.character(survival[,3])
      # print(stat)
      s <- Surv(follow,stat=='TRUE')
      coxmodel <- coxph(s~ hem,data=survival)
      summary(coxmodel)
      sum1 <- summary(coxmodel)
      print(sum1)
      #hist(hem,100)
      surv.res <- survfit(s~ hem>=11)
      
      plot(surv.res, col = 1:2, xlab = "Months After Chemotherapy", ylab = "OS",main=paste(input$coho_pat_char,"on", input$coho_obs_analysis,sep=" "))
      surv.res <- survfit(s~ hem>=8)
     plot(surv.res, col = 1:2, xlab = "Months After Chemotherapy", ylab = "OS",main=paste(input$coho_pat_char,"on", input$coho_obs_analysis, sep=" "))
      legend("topright", c("hemoglobin high", "hemoglobin low"), col = 1:2, lwd = 2)
      
    }
  })
  output$plot <- renderPlot({
    outPlot()
  })
  
  outGraph <- reactive({
    if(input$InputPanel == "panel1" && !is.null(input$patientid) && input$patientid > 0 && !is.null(input$result_analysis) && input$result_analysis =="Observation" && !is.null(input$obs_analysis) && input$obs_analysis !="Select" )	
    {
       print(input$patientid)
       print(input$obs_analysis)
       print(HAPI_observation)
      tmp<-as.vector(HAPI_observation[which(HAPI_observation$patientid == input$patientid & HAPI_observation$codedisplay==input$obs_analysis ),"valueQuantityvalue"])
      tmp1<-as.vector(HAPI_observation[which(HAPI_observation$patientid == input$patientid & HAPI_observation$codedisplay==input$obs_analysis ),"effectivedatetime"])
       print(tmp)
      tmp<-as.numeric(as.character(tmp))
      tmp1<-as.character(tmp1)
       print(tmp)
       print(tmp1)
      if(length(tmp)>1)
      {
       # par(mfrow = c(2,1))
      #  hist(tmp,main=paste("Observation: ",input$obs_analysis ," Distribution for Patient ",input$patientid,sep=""),xlab=input$obs_analysis,ylab="Number of Observations")
        tmp1 <- as.Date(tmp1, "%Y-%m-%d")
       
        ord<-order(tmp1)
        tmp1<-tmp1[ord]
       
        tmp<-tmp[ord]
       
        
        print(tmp1[1])
        
        value=c(tmp[1],tmp[2],tmp[3],tmp[4],tmp[5],tmp[6],tmp[7])
        
        datetimes <- seq.POSIXt(as.POSIXct("2015-01-01", tz="GMT"), as.POSIXct("2015-01-07", tz="GMT"), by="24 hours")
        series <- xts(value, order.by = datetimes, tz="GMT")
        dygraph(series,main = paste("Observation: ",input$obs_analysis ," Timeline for Patient ",input$patientid,sep=""), xlab="Date",ylab = "Observation Value")

        
       # myts<- ts(tmp,start=c(2001,1,1), freq=12)
      #  print(myts)
      #  plot(myts)
    
      }
    }
    
  })
  output$dygraph <- renderDygraph({
    outGraph()
  })
  
  outCoho_category <- reactive({
    if(input$InputPanel == "panel2")
    {
      return(selectInput('coho_category', 'Choose a Category',as.list(c("Select","Observation","Procedure","Condition"))))
    }
  })
  
  output$Coho_category = renderUI({
    outCoho_category()
  })	
  
  outCoho_Analysis <- reactive({
    if(input$InputPanel == "panel2" && !is.null(input$coho_category) && input$coho_category =="Observation")
    {
      tmp<-unique(HAPI_observation[,"codedisplay"])
      return(selectInput('coho_obs_analysis', 'Choose a Observation', as.list(c("Select",as.character(tmp)))))
    }
    if(input$InputPanel == "panel2" && !is.null(input$coho_category) && input$coho_category =="Procedure")
    {
      tmp<-unique(HAPI_procedure[,"codedisplay"])
      return(selectInput('coho_pro_analysis', 'Choose a Observation', as.list(c("Select",as.character(tmp)))))
    }
    if(input$InputPanel == "panel2" && !is.null(input$coho_category) && input$coho_category =="Condition")
    {
      tmp<-unique(HAPI_condition[,"codedisplay"])
      return(selectInput('coho_con_analysis', 'Choose a Observation', as.list(c("Select",as.character(tmp)))))
    }
  })
  
  output$Coho_Analysis = renderUI({
    outCoho_Analysis()
  })
  
  outCohoDataset <- reactive({
    if(input$InputPanel == "panel2" && !is.null(input$coho_category) && input$coho_category =="Observation" && !is.null(input$coho_obs_analysis) && input$coho_obs_analysis != "Select")
    {
      return(selectInput('coho_pat_char', 'See Distribution or Choose Analysis Type', as.list(c("Select","age","gender","survival analysis"))))
    }
    if(input$InputPanel == "panel2" && !is.null(input$coho_category) && input$coho_category =="Procedure" && !is.null(input$coho_pro_analysis) && input$coho_pro_analysis != "Select")
    {
      return(selectInput('coho_pat_char', 'See Distribution or Choose Analysis Type', as.list(c("Select","age","gender"))))
    }
    if(input$InputPanel == "panel2" && !is.null(input$coho_category) && input$coho_category =="Condition" && !is.null(input$coho_con_analysis) && input$coho_con_analysis != "Select")
    {
      return(selectInput('coho_pat_char', 'See Distribution or Choose Analysis Type', as.list(c("Select","age","gender"))))
    }
  })
  
  output$Coho_Dataset = renderUI({
    outCohoDataset()
  }) 
  
})
