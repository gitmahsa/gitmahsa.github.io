
#1) Did people’s privacy concern change at the end of the survey
#2) What impact did context factors have on privacy concerns
#3) Which context factors did the participant care about? Why
#4) How do these concerns correlate with their Privacy Attitudes

library("devtools")
library("likert")
require(gridExtra)
library(repr)
library(cowplot)
library(dplyr)
library(ordinal)
library(FSA)
library(dplyr)
library(ggplot2)
library(tidyr)
library(likert)
library(gplots)
library(reshape2)
library(RColorBrewer)
library(grid)
library(lsmeans)
library(sjstats)
library(MASS)
library(lsr)
library(cluster)
library(compareGroups)
library(forcats)
library(lme4)
library(rcompanion)
library(car)
library(RVAideMemoire)
library(multcompView)
library(ggplot2)
library(ggrepel)
library(RVAideMemoire)

#---------------Data reading -------------------------


df <- data.frame(matrix(ncol = 52, nrow = 0))# the main dataset including all applets and all responses

labels<- c("Participant","Survey","App","App_category","Trigger_category","Action_category","Gender","Age","Education", "IFTTT", "IoT", "IUIPC1", "IUIPC2", "IUIPC3", "IUIPC4", "IUIPC5", "IUIPC6", "IUIPC7", "IUIPC8", "IUIPC9",
           "privacy", "Morning", "Afternoon","Night", "Kitchen", "Livingroom","Bathroom","Bedroom","Frontdoor","Spouse","Kids","Visitors","Outsider", "WSpouse","WKids","WVisitors","WOutsider","AKitchen", "ALivingroom","ABedroom","ABathroom", "AFrontdoor",
           "shared","time_concern","Alocation_concern","Tlocation_concern","whoaround_concern","who_concern","shared_concern","scenario_concern","scenario_reason","CF_violation")

df<-setNames(df,labels)
all_pnum<-0

concern_level<-c("Extremely concerned", "Moderately concerned", "Somewhat concerned","Slightly concerned", "Not at all concerned")
#------we read all 7 surveys and import them in to df 

# the path to final polished data: /Users/mahsa/Box Sync/Research projects/IFTTT project/60-survey responses_removing duplicates/survey-checkagain

File1<-read.csv("Data/IFTTT Survey 1.csv", header = TRUE)
File1_time<-subset(File1, 	Duration..in.seconds. > "300" & Finished =='True') 
head (File1)
  for (i in 1:7) {
  
  


  s<-(paste0("IFTTT Survey ",i,".csv" ))
  

  Survey2<-read.csv(s, header = TRUE)
  print (s)
  if (i==1){
    p_num<-67
    Survey2<-setNames(Survey2, c("Gender","Age","Education", "IFTTT", "IoT", "IUIPC1", "IUIPC2", "IUIPC3", "IUIPC4", "IUIPC5", "IUIPC6", "IUIPC7", "IUIPC8", "IUIPC9",
                                 
                                 
                                 "privacy", "Morning", "Afternoon","Night","Spouse","Kids","Visitors","Outsider","shared",
                                 "time_concern","who_concern","shared_concern","scenario_concern","scenario_reason",
                                 
                      
                                 "privacy", "Morning", "Afternoon","Night","Kitchen", "Livingroom","Bathroom","Bedroom", "Frontdoor", "Spouse","Kids","Visitors","Outsider","shared",
                                 "Tlocation_concern","time_concern","who_concern","shared_concern","scenario_concern","scenario_reason",
                                 
                                 
                                 "privacy", "Morning", "Afternoon","Night","Spouse","Kids","Visitors","Outsider","shared",
                                 "time_concern","who_concern","shared_concern","scenario_concern","scenario_reason",
                                 
                          
                                 "privacy", "Morning", "Afternoon","Night","AKitchen", "ALivingroom","ABedroom","WSpouse","WKids","WVisitors","WOutsider",
                                 "time_concern","Alocation_concern","whoaround_concern","scenario_concern","scenario_reason",
                                 
                                 "privacy", "Morning", "Afternoon","Night","Kitchen", "Livingroom","Bathroom","Bedroom","Spouse","Kids","Visitors","Outsider",
                                "AKitchen", "ALivingroom","ABathroom","ABedroom","AFrontdoor",  "WSpouse","WKids","WVisitors","WOutsider",
                                "time_concern","Tlocation_concern","who_concern","whoaround_concern","Alocation_concern","scenario_concern","scenario_reason",
                                 
                                 "privacy", "Morning", "Afternoon","Night","Kitchen", "Livingroom","Bathroom","Bedroom","Frontdoor","Spouse","Kids","Visitors","Outsider","shared",
                                 "time_concern","Tlocation_concern","who_concern","shared_concern","scenario_concern","scenario_reason",
    
                                "privacy", "Morning", "Afternoon","Night","Spouse","Kids" ,"Visitors","Outsider","WSpouse","WKids","WVisitors","WOutsider", "Kitchen", "Livingroom","Bedroom","Bathroom",
                                "AKitchen", "ALivingroom","ABedroom", 
                                "time_concern","who_concern","whoaround_concern","Tlocation_concern","Alocation_concern", "scenario_concern","scenario_reason"                 
    ))
    Survey2[] <- lapply(Survey2, as.character)
    
    cols <- c(1:14,15: 28)
    temp1<-Survey2[,cols] #applet1
    temp1[["App_category"]]<- "Location"
    temp1[["Trigger_category"]]<- "Location"
    temp1[["Action_category"]]<- "SocialNetworks"
    temp1[["CF_violation"]]<- "shared"
    
    cols <- c(1:14,29:48)
    temp2<-Survey2[,cols]#applet2
    temp2[["App_category"]]<- "Light"
    temp2[["Trigger_category"]]<- "Light"
    temp2[["Action_category"]]<- "Communication"
    temp2[["CF_violation"]]<- "shared"
    
    
    cols <- c(1:14,49:62)
    temp3<-Survey2[,cols]#applet3
    temp3[["App_category"]]<- "Location"
    temp3[["Trigger_category"]]<- "Location"
    temp3[["Action_category"]]<- "Organization"
    temp3[["CF_violation"]]<- "shared"
    
    
    cols <- c(1:14,63:78)
    temp4<-Survey2[,cols]#applet4
    temp4[["App_category"]]<- "Appliance"
    temp4[["Trigger_category"]]<- "Fitness"
    temp4[["Action_category"]]<- "Appliance"
    temp4[["CF_violation"]]<- "whoaround"
    
    
    cols <- c(1:14,79:106)
    temp5<-Survey2[,cols]#applet5
    temp5[["App_category"]]<- "Security"
    temp5[["Trigger_category"]]<- "VoiceAssistant"
    temp5[["Action_category"]]<- "Security"
    temp5[["CF_violation"]]<- "who"
    
    
    cols <- c(1:14,107:126)#applet6
    temp6<-Survey2[,cols]
    temp6[["App_category"]]<- "Security"
    temp6[["Trigger_category"]]<- "Security"
    temp6[["Action_category"]]<- "CloudStorage"
    temp6[["CF_violation"]]<- "who"
    
    
    cols <- c(1:14,127:152)#applet7
    temp7<-Survey2[,cols]
    temp7[["App_category"]]<- "VoiceAssistant"
    temp7[["Trigger_category"]]<- "VoiceAssistant"
    temp7[["Action_category"]]<- "Printer"
    temp7[["CF_violation"]]<- "whoaround"
    
    
  }#survey 1
  
  
  if (i==2){
    p_num<-56 #59
    Survey2<-setNames(Survey2, c("Gender","Age","Education", "IFTTT", "IoT", "IUIPC1", "IUIPC2", "IUIPC3", "IUIPC4", "IUIPC5", "IUIPC6", "IUIPC7", "IUIPC8", "IUIPC9",
                                 
                                 "privacy", "Morning", "Afternoon","Night", "AKitchen", "ALivingroom","ABathroom","ABedroom","WSpouse","WKids","WVisitors","WOutsider",
                                 "time_concern","Alocation_concern","whoaround_concern","scenario_concern","scenario_reason",
                                 
                                 "privacy", "Morning", "Afternoon","Night", "Kitchen", "Livingroom","Bathroom","Bedroom","Spouse","Kids","Visitors","Outsider","AKitchen", "ALivingroom","ABedroom",
                                 "WSpouse","WKids","WVisitors","WOutsider", 
                                 "time_concern","Tlocation_concern","who_concern", "whoaround_concern","Alocation_concern","scenario_concern","scenario_reason",
                                 
                                 "privacy", "Morning", "Afternoon","Night","Spouse","Kids","Visitors","Outsider", "shared", 
                                 "time_concern","who_concern", "shared_concern", "scenario_concern","scenario_reason",
                                 
                                 "privacy", "Morning", "Afternoon","Night",  "Kitchen", "Livingroom","Bathroom","Bedroom", "Spouse","Kids","Visitors","Outsider", "shared",
                                 "time_concern","Tlocation_concern","who_concern", "shared_concern", "scenario_concern","scenario_reason",
                                 
                                 "privacy", "Morning", "Afternoon","Night",  "Kitchen", "Livingroom","Bathroom","Bedroom","Frontdoor","shared",
                                 "time_concern","Tlocation_concern","shared_concern", "scenario_concern","scenario_reason",
                                 
                                 "privacy", "Morning", "Afternoon","Night" ,"Spouse","Kids","Visitors","Outsider", "shared", 
                                 "time_concern","who_concern", "shared_concern", "scenario_concern","scenario_reason",
                                 
                                 "privacy", "Morning", "Afternoon","Night",  "AKitchen", "ALivingroom","ABathroom","ABedroom", "WSpouse","WVisitors","WKids","WOutsider",
                                 "Alocation_concern", "time_concern", "whoaround_concern", "scenario_concern","scenario_reason"
                                 
                                 ))
    cols <- c(1:14,15: 31)
    temp1<-Survey2[,cols] #applet1
    temp1[["App_category"]]<- "Appliance"
    temp1[["Trigger_category"]]<- "Time"
    temp1[["Action_category"]]<- "Appliance"
    temp1[["CF_violation"]]<- "whoaround"
    
    
    
    cols <- c(1:14,32:57)
    temp2<-Survey2[,cols]#applet2
    temp2[["App_category"]]<- "Appliance"
    temp2[["Trigger_category"]]<- "VoiceAssistant"
    temp2[["Action_category"]]<- "Appliance"
    temp2[["CF_violation"]]<- "who"
    
    
    cols <- c(1:14,58:71)
    temp3<-Survey2[,cols]#applet3
    temp3[["App_category"]]<- "Location"
    temp3[["Trigger_category"]]<- "Location"
    temp3[["Action_category"]]<- "Organization"
    temp3[["CF_violation"]]<- "shared"
    
    
    cols <- c(1:14,72:90)
    temp4<-Survey2[,cols]#applet4
    temp4[["App_category"]]<- "Appliance"
    temp4[["Trigger_category"]]<- "Appliance"
    temp4[["Action_category"]]<- "Organization"
    temp4[["CF_violation"]]<- "shared"
    
    
    
    cols <- c(1:14,91:105)
    temp5<-Survey2[,cols]#applet5
    temp5[["App_category"]]<- "Light"
    temp5[["Trigger_category"]]<- "Light"
    temp5[["Action_category"]]<- "Communication"
    temp5[["CF_violation"]]<- "shared"
    
    
    cols <- c(1:14,106:119)#applet6
    temp6<-Survey2[,cols]
    temp6[["App_category"]]<- "Location"
    temp6[["Trigger_category"]]<- "Location"
    temp6[["Action_category"]]<- "SocialNetworks"
    temp6[["CF_violation"]]<- "shared"
    
    
    cols <- c(1:14,120:136)#applet7
    temp7<-Survey2[,cols]
    temp7[["App_category"]]<- "Environment"
    temp7[["Trigger_category"]]<- "Time"
    temp7[["Action_category"]]<- "Environment"
    temp7[["CF_violation"]]<- "whoaround"
    
    
  }#survey 2
  
  

if (i==3){
  p_num<-46 #51 
Survey2<-setNames(Survey2, c("Gender","Age","Education", "IFTTT", "IoT", "IUIPC1", "IUIPC2", "IUIPC3", "IUIPC4", "IUIPC5", "IUIPC6", "IUIPC7", "IUIPC8", "IUIPC9",
                            
                              "privacy", "Morning", "Afternoon","Night","Spouse","Kids","Visitors","Outsider", "shared",
                             "time_concern","who_concern","shared_concern","scenario_concern","scenario_reason",
                             
                             "privacy", "Morning", "Afternoon","Night","Spouse","Kids","Visitors","Outsider","shared",
                             "time_concern","who_concern","shared_concern","scenario_concern","scenario_reason",
                             
                             "privacy", "Morning", "Afternoon","Night",  "Kitchen", "Livingroom","Bathroom","Bedroom", "Spouse","Kids","Visitors","Outsider",
                             "WSpouse","WKids","WVisitors","WOutsider",
                             "time_concern","Tlocation_concern","whoaround_concern","who_concern","scenario_concern","scenario_reason",
                             
                             "privacy", "Morning", "Afternoon","Night","Spouse","Kids","Visitors","Outsider","shared",
                             "time_concern","who_concern","shared_concern","scenario_concern","scenario_reason",
                             
                             "privacy", "Morning", "Afternoon","Night","Spouse","Kids","Visitors","Outsider","shared",
                             "time_concern","who_concern","shared_concern","scenario_concern","scenario_reason",
                          
                             "privacy", "Morning", "Afternoon","Night",  "AKitchen", "ALivingroom","ABathroom","ABedroom","AFrontdoor", "Spouse","Kids","Visitors","Outsider",
                             "WSpouse","WKids","WVisitors","WOutsider",
                             "time_concern","Alocation_concern","who_concern","whoaround_concern","scenario_concern","scenario_reason",
                             
                             "privacy", "Morning", "Afternoon","Night",  "Kitchen", "Livingroom","Bathroom","Bedroom", "Spouse","Kids","Visitors","Outsider",
                             "WSpouse","WKids","WVisitors","WOutsider", "shared",
                             "time_concern","Tlocation_concern","who_concern","whoaround_concern","shared_concern","scenario_concern","scenario_reason"
                             
                             
                             
                             ))

cols <- c(1:14,15: 28)
temp1<-Survey2[,cols] #applet1
temp1[["App_category"]]<- "Security"
temp1[["Trigger_category"]]<- "Security"
temp1[["Action_category"]]<- "Communication"
temp1[["CF_violation"]]<- "shared"


cols <- c(1:14,29:42)
temp2<-Survey2[,cols]#applet2
temp2[["App_category"]]<- "Location"
temp2[["Trigger_category"]]<- "Location"
temp2[["Action_category"]]<- "CloudStorage"
temp2[["CF_violation"]]<- "shared"



cols <- c(1:14,43:64)
temp3<-Survey2[,cols]#applet3
temp3[["App_category"]]<- "VoiceAssistant"
temp3[["Trigger_category"]]<- "VoiceAssistant"
temp3[["Action_category"]]<- "Appliance"
temp3[["CF_violation"]]<- "who"


cols <- c(1:14,65:78)
temp4<-Survey2[,cols]#applet4 
temp4[["App_category"]]<- "Security"
temp4[["Trigger_category"]]<- "Security"
temp4[["Action_category"]]<- "Organization"
temp4[["CF_violation"]]<- "who"



cols <- c(1:14,79:92)
temp5<-Survey2[,cols]#applet5
temp5[["App_category"]]<- "Location"
temp5[["Trigger_category"]]<- "Location"
temp5[["Action_category"]]<- "Communication"
temp5[["CF_violation"]]<- "shared"


cols <- c(1:14,93:115)#applet6
temp6<-Survey2[,cols]
temp6[["App_category"]]<- "Light"
temp6[["Trigger_category"]]<- "Communication"
temp6[["Action_category"]]<- "Light"
temp6[["CF_violation"]]<- "who"


cols <- c(1:14,116:139)#applet7
temp7<-Survey2[,cols]
temp7[["App_category"]]<- "VoiceAssistant"
temp7[["Trigger_category"]]<- "VoiceAssistant"
temp7[["Action_category"]]<- "Organization"
temp7[["CF_violation"]]<- "who"

}#survey 3


if (i==4){
  p_num<-45 #47
  Survey2<-setNames(Survey2, c("Gender","Age","Education", "IFTTT", "IoT", "IUIPC1", "IUIPC2", "IUIPC3", "IUIPC4", "IUIPC5", "IUIPC6", "IUIPC7", "IUIPC8", "IUIPC9",
                               
                               "privacy", "Morning", "Afternoon","Night","Spouse","Kids","Visitors","Outsider","AKitchen", "ALivingroom","ABathroom","ABedroom","AFrontdoor",
                               "WSpouse","WKids","WVisitors","WOutsider", 
                               "time_concern","Alocation_concern","who_concern","whoaround_concern","scenario_concern","scenario_reason",
                               
                               "privacy", "Morning", "Afternoon","Night","Spouse","Kids","Visitors","Outsider","shared",
                               "time_concern","who_concern","shared_concern","scenario_concern","scenario_reason",
                               
                               "privacy", "Morning", "Afternoon","Night","Spouse","Kids","Visitors","Outsider","shared",
                               "time_concern","who_concern","shared_concern","scenario_concern","scenario_reason",
                               
                               "privacy", "Morning", "Afternoon","Night",  "AKitchen", "ALivingroom","ABathroom","ABedroom","AFrontdoor", "Spouse","Kids","Visitors","Outsider",
                               "Kitchen", "Livingroom","Bathroom","Bedroom","WSpouse","WKids","WVisitors","WOutsider",
                               "time_concern","Alocation_concern","who_concern","Tlocation_concern","whoaround_concern","scenario_concern","scenario_reason",
                               
                               
                               "privacy", "Morning", "Afternoon","Night", "Spouse","Kids","Visitors","Outsider", "WSpouse","WKids","WVisitors","WOutsider",
                               "AKitchen", "ALivingroom","ABathroom","ABedroom","AFrontdoor",
                               "time_concern","who_concern","whoaround_concern","Alocation_concern","scenario_concern","scenario_reason",
                               
                               "privacy", "Morning", "Afternoon","Night", "Spouse","Kids","Visitors","Outsider", "WSpouse","WKids","WVisitors","WOutsider",
                               "Kitchen", "Livingroom","Bathroom","Bedroom","AKitchen", "ALivingroom","ABedroom","ABathroom",
                               "time_concern","who_concern","whoaround_concern","Tlocation_concern","Alocation_concern","scenario_concern","scenario_reason",
                               
                               "privacy", "Morning", "Afternoon","Night","Spouse","Kids","Visitors","Outsider","shared",
                               "time_concern","who_concern","shared_concern","scenario_concern","scenario_reason"
                              
                               
                               
  ))
  
  cols <- c(1:14,15: 37)
  temp1<-Survey2[,cols] #applet1
  temp1[["App_category"]]<- "Light"
  temp1[["Trigger_category"]]<- "Location"
  temp1[["Action_category"]]<- "Light"
  temp1[["CF_violation"]]<- "whoaround"
  
  
  cols <- c(1:14,38:51)
  temp2<-Survey2[,cols]#applet2
  temp2[["App_category"]]<- "Appliance"
  temp2[["Trigger_category"]]<- "Appliance"
  temp2[["Action_category"]]<- "Organization"
  temp2[["CF_violation"]]<- "who"
  
  
  cols <- c(1:14,52:65)
  temp3<-Survey2[,cols]#applet3
  temp3[["App_category"]]<- "Location"
  temp3[["Trigger_category"]]<- "Location"
  temp3[["Action_category"]]<- "CloudStorage"
  temp3[["CF_violation"]]<- "shared"
  
  
  cols <- c(1:14,66:93)
  temp4<-Survey2[,cols]#applet4
  temp4[["App_category"]]<- "Light"
  temp4[["Trigger_category"]]<- "VoiceAssistant"
  temp4[["Action_category"]]<- "Light"
  temp4[["CF_violation"]]<- "who"
  
  
  cols <- c(1:14,94:116)
  temp5<-Survey2[,cols]#applet5
  temp5[["App_category"]]<- "Appliance"
  temp5[["Trigger_category"]]<- "Appliance"
  temp5[["Action_category"]]<- "Light"
  temp5[["CF_violation"]]<- "whoaround"
  
  
  cols <- c(1:14,117:143)#applet6
  temp6<-Survey2[,cols]
  temp6[["App_category"]]<- "Environment"
  temp6[["Trigger_category"]]<- "VoiceAssistant"
  temp6[["Action_category"]]<- "Environment"
  temp6[["CF_violation"]]<- "who"
  
  
  cols <- c(1:14,144:157)#applet7
  temp7<-Survey2[,cols]
  temp7[["App_category"]]<- "Security"
  temp7[["Trigger_category"]]<- "Security"
  temp7[["Action_category"]]<- "Organization"
  temp7[["CF_violation"]]<- "who"
  
  
}#survey 4


if (i==5){
  p_num<-50 #52
  Survey2<-setNames(Survey2, c("Gender","Age","Education", "IFTTT", "IoT", "IUIPC1", "IUIPC2", "IUIPC3", "IUIPC4", "IUIPC5", "IUIPC6", "IUIPC7", "IUIPC8", "IUIPC9",
                               
                               
                               "privacy", "Morning", "Afternoon","Night","Spouse","Kids","Visitors","Outsider","shared",
                               "time_concern","who_concern","shared_concern","scenario_concern","scenario_reason",
                               
                               "privacy", "Morning", "Afternoon","Night","Spouse","Kids","Visitors","Outsider",  "Kitchen", "Livingroom","Bedroom","Bathroom", "shared",
                               "time_concern","who_concern","Tlocation_concern","shared_concern","scenario_concern","scenario_reason",
                               
                               
                               "privacy", "Morning", "Afternoon","Night","Kitchen", "Livingroom","Bathroom","Bedroom", "Frontdoor", "Spouse","Kids","Visitors","Outsider","shared",
                               "time_concern","Tlocation_concern","who_concern","shared_concern","scenario_concern","scenario_reason",
                               
                               "privacy", "Morning", "Afternoon","Night","AKitchen", "ALivingroom","ABathroom","ABedroom","AFrontdoor","WSpouse","WKids","WVisitors","WOutsider", 
                               "time_concern","Alocation_concern","whoaround_concern","scenario_concern","scenario_reason",
                               
                               "privacy", "Morning", "Afternoon","Night","Kitchen", "Livingroom","Bathroom","Bedroom","Spouse","Kids","Visitors","Outsider",  
                               "WSpouse","WKids","WVisitors","WOutsider","shared",
                               "time_concern","Tlocation_concern","who_concern","whoaround_concern","shared_concern","scenario_concern","scenario_reason",
                               
                               
                               "privacy", "Morning", "Afternoon","Night","Kitchen", "Livingroom","Bathroom","Bedroom","Spouse","Kids","Visitors","Outsider",  
                               "WSpouse","WKids","WVisitors","WOutsider","AKitchen", "ALivingroom","ABathroom","ABedroom",
                               "Tlocation_concern","time_concern","who_concern","whoaround_concern","Alocation_concern","scenario_concern","scenario_reason",
  
                               "privacy", "Morning", "Afternoon","Night","Kitchen", "Livingroom","Bathroom","Bedroom","Frontdoor","Spouse","Kids","Visitors","Outsider","shared",
                               "time_concern","Tlocation_concern","who_concern","shared_concern","scenario_concern","scenario_reason"
                               
                               ))
  
  cols <- c(1:14,15: 28)
  temp1<-Survey2[,cols] #applet1
  temp1[["App_category"]]<- "Appliance"
  temp1[["Trigger_category"]]<- "Appliance"
  temp1[["Action_category"]]<- "Organization"
  temp1[["CF_violation"]]<- "shared"
  
  
  cols <- c(1:14,29:47)
  temp2<-Survey2[,cols]#applet2
  temp2[["App_category"]]<- "Environment"
  temp2[["Trigger_category"]]<- "Environment"
  temp2[["Action_category"]]<- "Communication"
  temp2[["CF_violation"]]<- "shared"
  
  
  cols <- c(1:14,48:67)
  temp3<-Survey2[,cols]#applet3
  temp3[["App_category"]]<- "Security"
  temp3[["Trigger_category"]]<- "Security"
  temp3[["Action_category"]]<- "Communication"
  temp3[["CF_violation"]]<- "shared"
  
  
  cols <- c(1:14,68:85)
  temp4<-Survey2[,cols]#applet4
  temp4[["App_category"]]<- "Light"
  temp4[["Trigger_category"]]<- "Time"
  temp4[["Action_category"]]<- "Light"
  temp4[["CF_violation"]]<- "whoaround"
  
  
  
  cols <- c(1:14,86:109)
  temp5<-Survey2[,cols]#applet5
  temp5[["App_category"]]<- "VoiceAssistant"
  temp5[["Trigger_category"]]<- "VoiceAssistant"
  temp5[["Action_category"]]<- "SocialNetworks"
  temp5[["CF_violation"]]<- "who"
  
  
  cols <- c(1:14,110:136)#applet6
  temp6<-Survey2[,cols]
  temp6[["App_category"]]<- "Environment"
  temp6[["Trigger_category"]]<- "VoiceAssistant"
  temp6[["Action_category"]]<- "Environment"
  temp6[["CF_violation"]]<- "who"
  

  
  cols <- c(1:14,137:156)#applet7
  temp7<-Survey2[,cols]
  temp7[["App_category"]]<- "Security"
  temp7[["Trigger_category"]]<- "Security"
  temp7[["Action_category"]]<- "Communication"
  temp7[["CF_violation"]]<- "shared"
  
}#survey 5

if (i==6){
  p_num<-51 #59
  Survey2<-setNames(Survey2, c("Gender","Age","Education", "IFTTT", "IoT", "IUIPC1", "IUIPC2", "IUIPC3", "IUIPC4", "IUIPC5", "IUIPC6", "IUIPC7", "IUIPC8", "IUIPC9",
                               
                               "privacy", "Morning", "Afternoon","Night","Kitchen", "Livingroom","Bathroom","Bedroom","Spouse","Kids","Visitors","Outsider",
                               "WSpouse","WKids","WVisitors","WOutsider",  "AKitchen", "ALivingroom","ABathroom","ABedroom","AFrontdoor",
                               "time_concern","Tlocation_concern","who_concern","whoaround_concern","Alocation_concern","scenario_concern","scenario_reason",
                               
                               "privacy", "Morning", "Afternoon","Night","Kitchen", "Livingroom","Bathroom","Bedroom","Frontdoor","Spouse","Kids","Visitors","Outsider","shared",
                               "time_concern","Tlocation_concern","who_concern","shared_concern","scenario_concern","scenario_reason",
                               
                               "privacy", "Morning", "Afternoon","Night","Kitchen", "Livingroom","Bathroom","Bedroom","Spouse","Kids","Visitors","Outsider","shared",
                               "time_concern","Tlocation_concern","who_concern","shared_concern","scenario_concern","scenario_reason",
                               
                               "privacy", "Morning", "Afternoon","Night",  "Kitchen", "Livingroom","Bathroom","Bedroom","Frontdoor", "Spouse","Kids","Visitors","Outsider","shared",
                               "time_concern","Tlocation_concern","who_concern","shared_concern","scenario_concern","scenario_reason",
                               
                               
                               "privacy", "Morning", "Afternoon","Night", "Kitchen", "Livingroom","Bathroom","Bedroom","Spouse","Kids","Visitors","Outsider", "WSpouse","WKids","WVisitors","WOutsider",
                               "time_concern","Tlocation_concern","who_concern","whoaround_concern","scenario_concern","scenario_reason",
                               
                               "privacy", "Morning", "Afternoon","Night", "Spouse","Kids","Visitors","Outsider","shared",
                               "time_concern","who_concern","shared_concern","scenario_concern","scenario_reason",
                               
                               "privacy", "Morning", "Afternoon","Night","AKitchen", "ALivingroom","ABathroom","ABedroom","AFrontdoor", "Spouse","Kids","Visitors","Outsider","WSpouse","WKids","WVisitors","WOutsider",
                               "time_concern","Alocation_concern","who_concern","whoaround_concern","scenario_concern","scenario_reason"
                               
                               
                                ))
  
  cols <- c(1:14,15: 42)
  temp1<-Survey2[,cols] #applet1
  temp1[["App_category"]]<- "VoiceAssistant"
  temp1[["Trigger_category"]]<- "VoiceAssistant"
  temp1[["Action_category"]]<- "Light"
  temp1[["CF_violation"]]<- "who"
  
  
  cols <- c(1:14,43:62)
  temp2<-Survey2[,cols]#applet2
  temp2[["App_category"]]<- "Hub"
  temp2[["Trigger_category"]]<- "Hub"
  temp2[["Action_category"]]<- "Communication"
  temp2[["CF_violation"]]<- "shared"
  
  
  cols <- c(1:14,63:81)
  temp3<-Survey2[,cols]#applet3 
  temp3[["App_category"]]<- "Appliance"
  temp3[["Trigger_category"]]<- "Appliance"
  temp3[["Action_category"]]<- "Communication"
  temp3[["CF_violation"]]<- "shared"
  
  
  
  cols <- c(1:14,82:101)
  temp4<-Survey2[,cols]#applet4
  temp4[["App_category"]]<- "Security"
  temp4[["Trigger_category"]]<- "Security"
  temp4[["Action_category"]]<- "Communication"
  temp4[["CF_violation"]]<- "shared"
  
  
  cols <- c(1:14,102:123)
  temp5<-Survey2[,cols]#applet5
  temp5[["App_category"]]<- "VoiceAssistant"
  temp5[["Trigger_category"]]<- "VoiceAssistant"
  temp5[["Action_category"]]<- "Spotify"
  temp5[["CF_violation"]]<- "who"
  
  
  cols <- c(1:14,124:137)#applet6
  temp6<-Survey2[,cols]
  temp6[["App_category"]]<- "Location"
  temp6[["Trigger_category"]]<- "Location"
  temp6[["Action_category"]]<- "Communication"
  temp6[["CF_violation"]]<- "shared"
  
  
  
  cols <- c(1:14,138:160)#applet7
  temp7<-Survey2[,cols]
  temp7[["App_category"]]<- "Security"
  temp7[["Trigger_category"]]<- "Security"
  temp7[["Action_category"]]<- "Light"
  temp7[["CF_violation"]]<- "who"
  
  
}#survey 6
  
if (i==7){
  p_num<-38 #51
  Survey2<-setNames(Survey2, c("Gender","Age","Education", "IFTTT", "IoT", "IUIPC1", "IUIPC2", "IUIPC3", "IUIPC4", "IUIPC5", "IUIPC6", "IUIPC7", "IUIPC8", "IUIPC9",
                               
                               "privacy", "Morning", "Afternoon","Night","Kitchen", "Livingroom","Bathroom","Bedroom","Spouse","Kids","Visitors","Outsider", 
                               "WSpouse","WKids","WVisitors","WOutsider", "AKitchen", "ALivingroom","ABathroom","ABedroom","AFrontdoor",
                               "Tlocation_concern","time_concern","who_concern","whoaround_concern","Alocation_concern","scenario_concern","scenario_reason",
                               
                               "privacy", "Morning", "Afternoon","Night","Kitchen", "Livingroom","Bathroom","Bedroom","Frontdoor","Spouse","Kids","Visitors","Outsider","shared",
                               "time_concern","Tlocation_concern","who_concern","shared_concern","scenario_concern","scenario_reason",
                               
                               "privacy", "Morning", "Afternoon","Night","Spouse","Kids","Visitors","Outsider","shared",
                               "time_concern","who_concern","shared_concern","scenario_concern","scenario_reason",
                               
                               "privacy", "Morning", "Afternoon","Night",  "Kitchen", "Livingroom","Bedroom","Bathroom", "Spouse","Kids","Visitors","Outsider","shared",
                               "time_concern","who_concern","Tlocation_concern","shared_concern","scenario_concern","scenario_reason",
                               
                               
                               "privacy", "Morning", "Afternoon","Night",  "shared",
                               "time_concern","shared_concern","scenario_concern","scenario_reason",
                               
                               "privacy", "Morning", "Afternoon","Night","Kitchen", "Livingroom","Bathroom","Bedroom", "Spouse","Kids","Visitors","Outsider","WSpouse","WKids","WVisitors","WOutsider","shared",
                               "time_concern",
                               "Tlocation_concern","who_concern","whoaround_concern","shared_concern","scenario_concern","scenario_reason",
                               
                               "privacy", "Morning", "Afternoon","Night","Spouse","Kids","Visitors","Outsider",
                               "WSpouse","WKids","WVisitors","WOutsider",
                               "time_concern","who_concern","whoaround_concern","scenario_concern","scenario_reason"
  ))
  
  cols <- c(1:14,15: 42)
  temp1<-Survey2[,cols] #applet1
  temp1[["App_category"]]<- "VoiceAssistant"
  temp1[["Trigger_category"]]<- "VoiceAssistant"
  temp1[["Action_category"]]<- "Light"
  temp1[["CF_violation"]]<- "who"
  
  
  cols <- c(1:14,43:62)
  temp2<-Survey2[,cols]#applet2
  temp2[["App_category"]]<- "Hub"
  temp2[["Trigger_category"]]<- "Hub"
  temp2[["Action_category"]]<- "CloudStorage"
  temp2[["CF_violation"]]<- "shared"
  
  
  cols <- c(1:14,63:76)
  temp3<-Survey2[,cols]#applet3
  temp3[["App_category"]]<- "Appliance"
  temp3[["Trigger_category"]]<- "Appliance"
  temp3[["Action_category"]]<- "Communication"
  temp3[["CF_violation"]]<- "shared"
  
  
  cols <- c(1:14,77:95)
  temp4<-Survey2[,cols]#applet4
  temp4[["App_category"]]<- "Environment"
  temp4[["Trigger_category"]]<- "Environment"
  temp4[["Action_category"]]<- "Communication"
  temp4[["CF_violation"]]<- "shared"
  
  
  cols <- c(1:14,96:104)
  temp5<-Survey2[,cols]#applet5
  temp5[["App_category"]]<- "Security"
  temp5[["Trigger_category"]]<- "Security"
  temp5[["Action_category"]]<- "Communication"
  temp5[["CF_violation"]]<- "shared"
  
  
  cols <- c(1:14,105:128)#applet6
  temp6<-Survey2[,cols]
  temp6[["App_category"]]<- "VoiceAssistant"
  temp6[["Trigger_category"]]<- "VoiceAssistant"
  temp6[["Action_category"]]<- "Organization"
  temp6[["CF_violation"]]<- "who"
  
  
  cols <- c(1:14,129:145)#applet7
  temp7<-Survey2[,cols]
  temp7[["App_category"]]<- "Security"
  temp7[["Trigger_category"]]<- "Location"
  temp7[["Action_category"]]<- "Security"
  temp7[["CF_violation"]]<- "who"
  
}#survey 7


  vars<-colnames(df)
  
  cnames<-colnames(temp1)
  add <-vars[!(vars%in%(cnames))] 
  if (!is.na(add)){
  for (colname in add){
    
    temp1[[colname]]<-""
  }
}
  temp1[["App"]]<-(i-1)*7+1
  temp1[["Participant"]]<-(all_pnum+1):(all_pnum+p_num)
  temp1[["Survey"]]<-i
  
  
  
  cnames<-colnames(temp2)
  add <-vars[!(vars%in%(cnames))] 
  if (!is.na(add)){
  for (colname in add){
    
    temp2[[colname]]<-""
  }
  }
  temp2[["App"]]<-(i-1)*7+2
  temp2[["Participant"]]<-(all_pnum+1):(all_pnum+p_num)
  temp2[["Survey"]]<-i
  
  cnames<-colnames(temp3)
  add <-vars[!(vars%in%(cnames))] 
  if (!is.na(add)){
  for (colname in add){
    
    temp3[[colname]]<-""
  }
  }
  temp3[["App"]]<-(i-1)*7+3
  temp3[["Participant"]]<-(all_pnum+1):(all_pnum+p_num)
  temp3[["Survey"]]<-i
  
  cnames<-colnames(temp4)
  add <-vars[!(vars%in%(cnames))]
  if (!is.na(add)){
  for (colname in add){
    
    temp4[[colname]]<-""
  }
  }
  temp4[["App"]]<-(i-1)*7+4
  temp4[["Participant"]]<-(all_pnum+1):(all_pnum+p_num)
  temp4[["Survey"]]<-i
  
  
  cnames<-colnames(temp5)
  add <-vars[!(vars%in%(cnames))] 
  if (!is.na(add)){
  for (colname in add){
    
    temp5[[colname]]<-""
  }
  }
  temp5[["App"]]<-(i-1)*7+5
  temp5[["Participant"]]<-(all_pnum+1):(all_pnum+p_num)
  temp5[["Survey"]]<-i
  
  cnames<-colnames(temp6)
  add <-vars[!(vars%in%(cnames))] 
  if (!is.na(add)){
  for (colname in add){
    
    temp6[[colname]]<-""
  }
  }
  temp6[["App"]]<-(i-1)*7+6
  temp6[["Participant"]]<-(all_pnum+1):(all_pnum+p_num)
  temp6[["Survey"]]<-i
  
  cnames<-colnames(temp7)
  add <-vars[!(vars%in%(cnames))] 
  if (!is.na(add)){
  for (colname in add){
    
    temp7[[colname]]<-""
  }
  }
  temp7[["App"]]<-(i-1)*7+7
  temp7[["Participant"]]<-(all_pnum+1):(all_pnum+p_num)
  temp7[["Survey"]]<-i
  
  all_pnum<-all_pnum+p_num
  
  #df include long view of  applets with all responses

  
  
  df[,4:52] <- lapply(df[,4:52], as.character)
  
  
  df<-bind_rows(df,temp1)
  df<-bind_rows(df,temp2)
  df<-bind_rows(df,temp3)
  df<-bind_rows(df,temp4)
  df<-bind_rows(df,temp5)
  df<-bind_rows(df,temp6)
  df<-bind_rows(df,temp7)
  
  
}#for loop over surveys 

#------------ *******RQ1*********----------


 
  Concern_applets <- dplyr::select(df,Participant, App, App_category,Trigger_category, Action_category, privacy)
 scenario_applets <- dplyr::select(df,Participant, App, App_category,Trigger_category, Action_category, scenario_concern)
  

 
   #------RQ1 one way anova analysis
  Concern_applets_numerical <- Concern_applets %>% mutate_at("privacy", funs(recode_factor(., "Extremely concerned"=5L, "Moderately concerned"=4L, "Somewhat concerned"=3L,"Slightly concerned"=2L,  "Not at all concerned"=1L, .default = NULL)))
  Concern_applets_numerical$App<-factor(Concern_applets_numerical$App)
  Concern_applets_numerical$privacy<-as.numeric(as.character(Concern_applets_numerical$privacy))
  
  plot_data<-Concern_applets_numerical[,c("Participant","App","privacy")]  
  
  aaa_RQ1<-Concern_applets_numerical[,c("App","privacy")] %>%  
    group_by(App) %>%  summarise_each(funs(mean, sd))  # mean of concern for each appelt
  
  concern_mean<-mean(Concern_applets_numerical$privacy) # mean of concern across all applets
  concern_sd<-sd( Concern_applets_numerical$privacy)
  aaa_RQ1<-aaa_RQ1[order(aaa_RQ1$mean),]
  #---RQ1- Figure 1 concern with applets-----
  ggplot(aaa_RQ1, aes(x=reorder(App,-mean), y=mean)) + 
    geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2) +
    geom_line() +
    geom_point()+ 
    
  
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                        panel.background = element_blank(), axis.line = element_line(colour = "black"),
                        axis.text.x=element_text(hjust = 0.8, size = 8, vjust=0.9)) +  xlab("Applets")+ ylab("Concern with the Applet")+

  geom_hline(yintercept=2:3, color= "red")
  #-----RQ1- plot------------
  data_wide <- spread(plot_data,App,privacy)
  items <- as.data.frame(data_wide[ ,2:50])
  q<-likert (items, nlevels = 5)
  plot(q)
  q$items$App=c('1','2','3','4','5','6','7','8','9','10','11','12','13','14','15',
                                  '16','17','18','19','20','21','22','23','24','25','26','27','28','29','30','31','32',
                                     '33','34','35','36','37','38','39','40','41','42','43','44','45','46','47','48','49')
  #title = "I contribute to F/OSS because..."
  title = "concern"
  
  scale_height = knitr::opts_chunk$get('fig.height')*0.7
  scale_width = knitr::opts_chunk$get('fig.width')*1.25
  knitr::opts_chunk$set(fig.height = scale_height, fig.width = scale_width)
  theme_update(plot.title = element_text(color="black", hjust = 0.8, size=10, face="bold")) 
    size_collumn = .7
  plot1 = likert.bar.plot(q, center=3,cex.axis=10 ) + theme(text = element_text(size = 12,color="black", face="bold"),axis.title.x=element_blank(),panel.background = element_rect(color = "gray",size= 1.2, fill = "white")) +theme( legend.text =element_text(color="black",size=8)) + theme( legend.title  =element_text(color="black",size=12))
  plot1$layers[[1]]$geom_params$width = size_collumn
  plot1$layers[[2]]$geom_params$width = size_collumn
  plot1$layers[[3]]$geom_params$width = size_collumn
  #dev.off()
  plot1
  
                                                                                                              
  
  #descriptive analysis
  count(Concern_applets_numerical,privacy)
  count(Concern_applets,privacy)
  
 
  #----RQ1-bar_plot_RQ1, Distribution of concern scores across all responses in RQ1
  scenario_applets_numerical <- scenario_applets %>% mutate_at("scenario_concern", funs(recode_factor(., "Extremely concerned"=5L, "Moderately concerned"=4L, "Somewhat concerned"=3L,"Slightly concerned"=2L,  "Not at all concerned"=1L, .default = NULL)))
  scenario_applets_numerical$App<-factor(scenario_applets_numerical$App)
  scenario_applets_numerical$scenario_concern<-as.numeric(as.character(scenario_applets_numerical$scenario_concern))
  
  
  
  
  privacyplot<-Concern_applets_numerical %>%
    group_by(privacy) %>%
    summarise(count = n()) %>% 
    mutate(perc=count/sum(count)*100)
  privacyplot$perc<-round((privacyplot$perc),2)
  
  #barplot for post applet description concern
  ggplot(privacyplot, aes(privacy, perc, fill = privacy)) +
    geom_bar(position = 'dodge',stat="identity") +          # determine type of plot
    theme_bw() +                         # use black & white theme
    # add and define text
    geom_text(aes(label=perc), vjust=-.25, color="black",
              position = position_dodge(0.9), size=4)+
  
    theme(legend.position="none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), panel.border = element_blank(),axis.line = element_line(colour = "black"),
axis.text.x=element_text(hjust = 0.8, size = 10, vjust=0.9),
          axis.text.y=element_text(hjust = 0.8, size = 10, vjust=0.9))+ ylab("Percentage of responses")+xlab("Concern score")
  
  
#------------********RQ2*********-----------------------------------------

Concern_applets <- dplyr::select(df,App, App_category,Trigger_category, Action_category, privacy)
appconcern_cols <- "privacy"


Concern_applets <- Concern_applets %>% mutate_at(appconcern_cols, funs(recode_factor(., "Extremely concerned"=5L, "Moderately concerned"=4L, "Somewhat concerned"=3L,"Slightly concerned"=2L,  "Not at all concerned"=1L, .default = NULL)))
Concern_applets<-mutate(Concern_applets,applet_bconcern=ifelse( Concern_applets$privacy %in% 3:5,1,0))

concerns<-Concern_applets %>%
  group_by(App) %>% summarize( app_concern=sum((applet_bconcern))/n()*100)



  #-------RQ2.1. Generating scatter plot of delta on users' privacy concern RQ2.1-----

Q1_CF_concerns<-dplyr::select(df,App, privacy,Morning,Afternoon,Night, Spouse, Kids, Outsider,Visitors,WSpouse, WKids, WOutsider,WVisitors,
                              AKitchen,ALivingroom,ABedroom,ABathroom,AFrontdoor,Kitchen,Livingroom,Bedroom,Bathroom,Frontdoor,shared,scenario_concern)

factor_cols <- names(Q1_CF_concerns[,3:24])
Q1_CF_concerns <- Q1_CF_concerns %>% mutate_at(factor_cols, funs(recode_factor(., "Extremely Concerned"=5L, "Moderately Concerned"=4L, 
                       "Slightly Concerned"=2L, "Somewhat concerned"=3L, "Not at all Concerned"=1L, .default = NULL)))
factor_cols <- c("privacy","scenario_concern")
Q1_CF_concerns <- Q1_CF_concerns %>% mutate_at(factor_cols, funs(recode_factor(., "Extremely concerned"=5L, "Moderately concerned"=4L, 
                                                                         "Slightly concerned"=2L, "Somewhat concerned"=3L, "Not at all concerned"=1L, .default = NULL)))
Q1_CF_concerns[ , 3:24] <- apply(Q1_CF_concerns[ , 3:24], 2,            # Specify own function within apply
                    function(x) as.numeric(as.character(x)))





Q1_CF_concerns$max_whoaround<-apply(Q1_CF_concerns[,10:13], 1, )

Q1_CF_concerns$max_time<-apply(Q1_CF_concerns[,3:5], 1, function(x) ifelse(all(is.na(x)),NA,max(x,na.rm = TRUE)))
Q1_CF_concerns$max_who<-apply(Q1_CF_concerns[,6:9], 1, function(x) ifelse(all(is.na(x)),NA,max(x,na.rm = TRUE)))
Q1_CF_concerns$max_whoaround<-apply(Q1_CF_concerns[,10:13], 1, function(x) ifelse(all(is.na(x)),NA,max(x,na.rm = TRUE)))

Q1_CF_concerns$max_alocation<-apply(Q1_CF_concerns[,14:18], 1, function(x) ifelse(all(is.na(x)),NA,max(x,na.rm = TRUE)))
Q1_CF_concerns$max_tlocation<-apply(Q1_CF_concerns[,19:23], 1, function(x) ifelse(all(is.na(x)),NA,max(x,na.rm = TRUE)))
Q1_CF_concerns$max_shared<-apply(Q1_CF_concerns["shared"], 1, function(x) ifelse(all(is.na(x)),NA,max(x,na.rm = TRUE)))


Q1_CF_concerns$av_CF<-apply(Q1_CF_concerns[,26:31], 1, function(x) ifelse(all(is.na(x)),NA,mean(x,na.rm = TRUE)))


Q1_CF_max_concerns <- dplyr::select(Q1_CF_concerns,App, privacy,max_time,max_who,max_whoaround,max_alocation,max_tlocation,max_shared,av_CF,scenario_concern)
Q1_CF_max_concerns <- Q1_CF_max_concerns %>% mutate (delta_time=as.numeric(max_time)- as.numeric(as.character( privacy) ),
                                                     delta_who=as.numeric(max_who)- as.numeric(as.character( privacy) ) ,
                                                      delta_whoaround=as.numeric(max_whoaround)- as.numeric(as.character( privacy) ),
                                                     delta_tlocation=as.numeric(max_tlocation)- as.numeric(as.character( privacy) ),
                                                     delta_alocation=as.numeric(max_alocation)- as.numeric(as.character( privacy) ),
                                                     delta_shared=as.numeric(max_shared)- as.numeric(as.character( privacy) ),
                                                     delta_CF_scenario=as.numeric(as.character(scenario_concern))- as.numeric(( av_CF) ))


Q1_CF_delta <- dplyr::select(Q1_CF_max_concerns,App, privacy,delta_time,delta_who,delta_whoaround,delta_alocation,delta_tlocation,delta_shared)

write.csv(Q1_CF_delta,"Q1_delta.csv")
Q1_CF_delta_concerns<-Q1_CF_delta %>%
  group_by(App) %>% summarize( time_mean=mean(as.numeric(delta_time)),who_mean=mean(as.numeric(delta_who)),
                               whoaround_mean=mean(as.numeric(delta_whoaround)), tlocation_mean=mean(as.numeric(delta_tlocation)),
                               alocation_mean=mean(as.numeric(delta_alocation)),shared_mean=mean(as.numeric(delta_shared)),privacy_mean=mean(as.numeric(as.character(privacy))))


scatter_data<-Q1_CF_delta %>%
  group_by(App) %>% summarize( time_mean=mean(as.numeric(delta_time)),who_mean=mean(as.numeric(delta_who)),
                               whoaround_mean=mean(as.numeric(delta_whoaround)), tlocation_mean=mean(as.numeric(delta_tlocation)),
                               alocation_mean=mean(as.numeric(delta_alocation)),shared_mean=mean(as.numeric(delta_shared)))


Q1_CF_delta_concerns_matrix <- Q1_CF_delta_concerns[,-c(1,7)]
Q1_CF_delta_concerns_matrix<-(sapply(Q1_CF_delta_concerns_matrix, as.numeric)) 

Q1_CF_delta_concerns_matrix<-data.frame(Q1_CF_delta_concerns_matrix ,row.names=Q1_CF_delta_concerns$App)
Q1_CF_delta_concerns_matrix<-round((Q1_CF_delta_concerns_matrix),2)

#--------scatter plot for delat
names(scatter_data) <- c("App","Time", "Who can use", "Who is around", "Trigger location", "Action location", "OnlineServices")

scatter_data_long<-gather(scatter_data, CF, delta, Time:OnlineServices, factor_key=TRUE)  # wide to long reshaping
labels=c("Trigger location", "Action location", "Online services", "Time","Who can use", "Who is around")
scatter_data_long$CF = factor(scatter_data_long$CF, levels=c( "Trigger location", "Action location", "OnlineServices", "Time","Who can use", "Who is around"))

#scatter_data_long2 <- arrange(transform(scatter_data_long,scatter_data_long$CF=factor(scatter_data_long$CF,levels=labels),scatter_data_long$CF))
ggplot(scatter_data_long, aes(x=App, y=delta, group=CF)) +
  
  geom_point(aes(shape=CF, color=CF, size=CF),size=2)+
  scale_size_manual(values=c(1, 1,1,1, 1,1),labels=labels)+
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  
  scale_color_manual(values=c('brown3','dark green', "blue","purple", "gold4","black"),
                     labels=labels)+
  scale_shape_manual(values=c(15,1,17,18,4,8), 
                     labels=labels)+
  facet_wrap( ~CF , nrow=1) +labs( x="Applets")+ theme(panel.spacing = unit(.5, "cm"))+
  theme(legend.position = "bottom",legend.title=element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x=element_text(hjust = .8, size = 12, vjust=1.2)) +  ylab("Change in concern") +
  scale_x_continuous(limits = c(1, 49), expand = c(0, 0))+
  geom_text_repel(aes(label = ifelse(delta<0,as.character(App),'')), size = 4)+
  theme(legend.text=element_text(size=16))+
  theme(axis.title.y = element_text(size = 16), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  theme(axis.title.x = element_text(size = 16))+
  theme(strip.text.x = element_text(size = 13))+
  theme(axis.text.y   = element_text(size = 14))+
  theme(strip.text.y = element_text(size = 12))
 

  
  
  #--------CLMM anlalysis to explore interaction between CF values, apps and privacy concern---------

      #--  -------------time---------

time_plot<-time_data[c("Participant","Survey","App", "Morning","Afternoon","Night","Trigger_category","Action_category")]


timeConcerns<-df[c("App", "Participant","time_concern")]
timeConcerns$length<-sapply(strsplit(timeConcerns$time_concern, " "), length)
timeConcerns<-na.omit(timeConcerns)
timeConcerns<-timeConcerns[(timeConcerns$length>0),]
timeConcerns<-timeConcerns[(timeConcerns$length<4),]

write.csv(timeConcerns, "timeconcern.csv") 

data_long <- gather(time_plot, timeofday, concern, Morning:Night, factor_key=TRUE)  # wide to long reshaping
d2 <- data_long %>% 
  group_by(App,timeofday,concern) %>% 
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
plot <- ggplot(d2, aes(x=factor(App),y = 100*perc, fill=factor(concern))) +
  geom_bar(stat="identity",width = 0.7) + xlab(labs(title="Apps")) + ylab(labs(title="Time of day")) +
  facet_wrap(timeofday~.,nrow = 1)+
  theme(axis.text.x=element_blank(),legend.position="bottom")+
  
  scale_fill_manual(values=c("brown3","salmon", "burlywood1","palegreen3","seagreen"))
ggsave("overall_time.png", plot, device='png', height=4, width=4, dpi=300, units="in")
print(plot)
  
 




  #---factor anova analysis 
  #-------one way Anova with controlfactor App
data_long$Survey<-factor(data_long$Survey)
data_long$App<-factor(data_long$App)
data_long$timeofday<-factor(data_long$timeofday)
#data_long$concern<-factor(data_long$concern)
#data_long$concern<-as.numeric((data_long$concern))
data_long$concern = factor(data_long$concern,
                       ordered=TRUE)


nullm <- clmm(concern ~ timeofday+(1|Participant) + (1|App)  , data = data_long)
ordinalmodel<- clmm(concern ~ timeofday+(1|Participant) + (1|App) +(1|Survey) , data = data_long)
ordinalmodelnull<- clmm(concern ~ 1+(1|Participant) + (1|App) + (1|Survey), data = data_long)

nagelkerke(fit  = ordinalmodel,
           null = ordinalmodelnull)

nagelkerke(fit  = ordinalmodel,
           null = nullm)
marginal = lsmeans(ordinalmodel,
                   pairwise ~ timeofday,
                   adjust="tukey")         ###  Tukey-adjusted comparisons

marginal

summary(ordinalmodel)


      #----  -----------Tlocation---------------
tlocation_plot<-Tlocation_data[c("Participant","Survey","App", "Kitchen","Livingroom","Bedroom","Bathroom", "Frontdoor")] #Run 1 for Frontddor applet, Run 2 for applets that not applicable to frontdoor
tlocation_plot_withcateory<-Tlocation_data[c("Participant","Survey","App", "Kitchen","Livingroom","Bedroom","Bathroom", "Frontdoor","Trigger_category","Action_category")]# Run1, Run2



tlocation_plot_withcateory<-tlocation_plot_withcateory[!(tlocation_plot_withcateory$App== 2 | tlocation_plot_withcateory$App== 6 |
                                                           tlocation_plot_withcateory$App== 12 | 
                                                           tlocation_plot_withcateory$App== 31 |
                                                          tlocation_plot_withcateory$App== 35 | tlocation_plot_withcateory$App== 37 |
                                                          tlocation_plot_withcateory$App== 39 | tlocation_plot_withcateory$App== 44  ),]# Run 2

tlocation_plot_withcateory<-na.omit(tlocation_plot_withcateory) # Run1
factor(tlocation_plot_withcateory$Trigger_category)
factor(tlocation_plot_withcateory$Action_category)
#----trigger locationcincern excluding frontdoor
TriggerLocationConcerns<-df[c("App", "Participant","Tlocation_concern")]
TriggerLocationConcerns<-TriggerLocationConcerns[(TriggerLocationConcerns$App== 2 | TriggerLocationConcerns$App== 6 |
                                                     TriggerLocationConcerns$App== 12 | 
                                                     TriggerLocationConcerns$App== 31 |
                                                     TriggerLocationConcerns$App== 35 | TriggerLocationConcerns$App== 37 |
                                                     TriggerLocationConcerns$App== 39 | TriggerLocationConcerns$App== 44  ),]
TriggerLocationConcerns$length<-sapply(strsplit(TriggerLocationConcerns$Tlocation_concern, " "), length)

TriggerLocationConcerns<-na.omit(TriggerLocationConcerns)
TriggerLocationConcerns<-TriggerLocationConcerns[(TriggerLocationConcerns$length>0),]
TriggerLocationConcerns<-TriggerLocationConcerns[(TriggerLocationConcerns$length<4),]

write.csv(TriggerLocationConcerns, "triggerlocationconcernewFD.csv")
   #--
#Run1 and Run2 
data_long <- gather(tlocation_plot_withcateory, locationofhome, concern, Kitchen:Frontdoor, factor_key=TRUE)  # wide to long reshaping


data_long<-na.omit( data_long) # Run1, Run 2



d2 <- data_long %>% 
  group_by(App,locationofhome,concern) %>% 
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
plot <- ggplot(d2, aes(x=factor(App),y = 100*perc, fill=factor(concern))) +
  geom_bar(stat="identity",width = 1) + xlab(labs(title="Apps")) + ylab(labs(title="TLocation of Home")) +
facet_wrap(locationofhome~.,nrow = 1)+
  theme(axis.text.x=element_blank(),legend.position="bottom",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  scale_fill_manual(values=c("brown3","salmon", "burlywood1","palegreen3","seagreen"))
ggsave("overall_time.png", plot, device='png', height=4, width=4, dpi=300, units="in")
print(plot)

#--------anova 
data_long$locationofhome <- as.character(data_long$locationofhome)
data_long$locationofhome[data_long$locationofhome=="Kitchen" | data_long$locationofhome =="Livingroom" ]<-"private-semi"
data_long$locationofhome[data_long$locationofhome =="Frontdoor"]<-"public"

data_long$locationofhome[data_long$locationofhome=="Bedroom" | data_long$locationofhome =="Bathroom"]<-"private"


#---------glmm
data_long$Survey<-factor(data_long$Survey)
data_long$App<-factor(data_long$App)
data_long$locationofhome<-factor(data_long$locationofhome)
#data_long$concern<-as.numeric((data_long$concern))
data_long$concern<-factor((data_long$concern), ordered=TRUE)



#--------------
ordinalmodel<- clmm(concern ~ locationofhome+(1|Participant) + (1|App) , data = data_long, link = "probit")

ordinalmodelnull<- clmm(concern ~ 1+(1|Participant) + (1|App), data = data_long, link = "probit")

ordinalmodelsurvey<- clmm(concern ~ locationofhome +(1|Participant) + (1|App) + (1|Survey), data = data_long, link = "probit")


nagelkerke(fit  = ordinalmodel,
           null = ordinalmodelnull)


nagelkerke(fit  = ordinalmodel,
           null = ordinalmodelsurvey)

summary(ordinalmodel)

marginal = lsmeans(ordinalmodel,
                   pairwise ~ locationofhome,
                   adjust="tukey")         ###  Tukey-adjusted comparisons
marginal
#--------------



      #------------------Alocation---------------


alocation_plot<-Alocation_data[c("Participant","Survey","App", "AKitchen","ALivingroom","ABedroom","ABathroom")]
alocation_plot_withcateory<-Alocation_data[c("Participant","Survey","App", "AKitchen","ALivingroom","ABedroom","ABathroom", "AFrontdoor","Trigger_category","Action_category")]
alocation_plot_withcateory<-na.omit(alocation_plot_withcateory)
factor(alocation_plot_withcateory$Trigger_category)
factor(alocation_plot_withcateory$Action_category)

ActionLocationConcerns<-df[c("Participant","Survey","App", "Alocation_concern")]
ActionLocationConcerns$length<-sapply(strsplit(ActionLocationConcerns$Alocation_concern, " "), length)

ActionLocationConcerns<-na.omit(ActionLocationConcerns)
ActionLocationConcerns<-ActionLocationConcerns[(ActionLocationConcerns$length>0),]
ActionLocationConcerns<-ActionLocationConcerns[(ActionLocationConcerns$length<4),]

write.csv(ActionLocationConcerns, "actionlocationconcern.csv") # exclude app 4 7 9 


data_long <- gather(alocation_plot_withcateory, locationofhome, concern, AKitchen:AFrontdoor, factor_key=TRUE)  # wide to long reshaping
data_long<-na.omit( data_long)



d2 <- data_long %>% 
  group_by(App,locationofhome,concern) %>% 
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
plot <- ggplot(d2, aes(x=factor(App),y = 100*perc, fill=factor(concern))) +
  geom_bar(stat="identity",width = 0.8) + xlab(labs(title="Apps")) + ylab(labs(title="Action Location")) +
  facet_wrap(locationofhome~.,nrow = 1)+
   theme(axis.text.x=element_blank(),legend.position="bottom")+
  
  scale_fill_manual(values=c("brown3","salmon", "burlywood1","palegreen3","seagreen"))
ggsave("overall_time.png", plot, device='png', height=4, width=4, dpi=300, units="in")
print(plot)

data_long$locationofhome <- as.character(data_long$locationofhome)
data_long$locationofhome[data_long$locationofhome=="AKitchen" | data_long$locationofhome =="ALivingroom" ]<-"private-semi"

data_long$locationofhome[data_long$locationofhome =="AFrontdoor"]<-"public"
data_long$locationofhome[data_long$locationofhome=="ABedroom" | data_long$locationofhome =="ABathroom"]<-"private"

#---------glmm
data_long$Survey<-factor(data_long$Survey)
data_long$App<-factor(data_long$App)
data_long$locationofhome<-factor(data_long$locationofhome)
#data_long$concern<-as.numeric((data_long$concern))
data_long$concern<-factor(data_long$concern, ordered = TRUE)

 
#--------------
nullm<- clmm(concern ~ locationofhome+(1|Participant) + (1|App) , link = "probit",data = data_long)


ordinalmodel<- clmm(concern ~ locationofhome+(1|Participant) + (1|App) +(1|Survey), link = "probit",data = data_long)

ordinalmodelnull<- clmm(concern ~ 1+(1|Participant) + (1|App) + (1|Survey) , data = data_long, link = "probit", control = list("model.frame"))
nagelkerke(fit  = ordinalmodel,
           null = ordinalmodelnull)

nagelkerke(fit  = ordinalmodel,
           null = nullm)


Anova(ordinalmodel,
      type = "II")



marginal = lsmeans(ordinalmodel,
                   pairwise ~ locationofhome,
                   adjust="tukey")         ###  Tukey-adjusted comparisons
marginal



      #-----------------Who-----------------


who_plot<-who_data[c("Participant","Survey","App","Spouse","Kids","Visitors","Outsider", "Trigger_category","Action_category")]

who_plot<-na.omit(who_plot)
mean(as.numeric(as.character( who_plot$Spouse)))
mean(as.numeric(as.character(who_plot$Kids)))
mean(as.numeric(as.character(who_plot$Visitors)))
mean(as.numeric(as.character(who_plot$Outsider)))


sd(as.numeric(as.character( who_plot$Spouse)))
sd(as.numeric(as.character(who_plot$Kids)))
sd(as.numeric(as.character(who_plot$Visitors)))
sd(as.numeric(as.character(who_plot$Outsider)))



colnames(who_plot)<- c("Participant","Survey","App","Spouses","Kids","Visitors","Outsiders", "Trigger_category","Action_category")

data_long <- gather(who_plot, whois, concern, Spouses:Outsiders, factor_key=TRUE)  # wide to long reshaping


data_long<-na.omit( data_long)

WhoConcerns<-df[c("App","Participant", "who_concern")]
WhoConcerns$length<-sapply(strsplit(WhoConcerns$who_concern, " "), length)

WhoConcerns<-na.omit(WhoConcerns)
WhoConcerns<-WhoConcerns[(WhoConcerns$length>0),]
WhoConcerns<-WhoConcerns[(WhoConcerns$length<4),]

write.csv(WhoConcerns, "whoconcern.csv") 


d2 <- data_long %>% 
  group_by(App,whois,concern) %>% 
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
plot <- ggplot(d2, aes(x=factor(App),y = 100*perc, fill=factor(concern))) +
  geom_bar(stat="identity",width = 1) + xlab(labs(title="Applets")) + ylab(labs(title="Who can use")) +
   facet_wrap(whois~.,nrow = 1)+
  theme(axis.title.x=element_blank(),axis.line.x=element_blank(),axis.text.x=element_blank(),legend.position="bottom", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  
  scale_fill_manual(values=c("brown3","salmon", "burlywood1","palegreen3","seagreen"))
ggsave("overall_time.png", plot, device='png', height=4, width=4, dpi=300, units="in")
print(plot)


#---------glmm
data_long$Participant<-factor(data_long$Participant)
data_long$App<-factor(data_long$App)
data_long$whois<-factor(data_long$whois)
#data_long$concern<-as.numeric((data_long$concern))
data_long$concern<-factor((data_long$concern), ordered = TRUE)

#--------------
nullm<- clmm(concern ~ whois+(1|Participant) + (1|App) , data = data_long)

ordinalmodel<- clmm(concern ~ whois+Survey + (1|Participant) + (1|App) , data = data_long)

ordinalmodelnull<- clmm(concern ~ whois+(1|Participant) + (1|App) , data = data_long)
#surveymodel<- clmm(concern ~ whois+ Survey+(1|Participant) + (1|App), data = data_long)
nagelkerke(fit  = ordinalmodel,
           null = nullm)


nagelkerke(fit  = ordinalmodel,
           null = ordinalmodelnull)



Anova(ordinalmodel,
      type = "II")



marginal = lsmeans(ordinalmodel,
                   pairwise ~ whois,
                   adjust="tukey")         ###  Tukey-adjusted comparisons
marginal



#---cluster
d2$concern<-as.factor((d2$concern))
gower.dist <- daisy(d2[],c("App","concern"), metric = c("gower"))
#d <- dist(d2, method = "euclidean") # distance matrix
fit <- hclust(gower.dist, method="ward.D") 
plot(fit) # display dendogram
groups <- cutree(fit, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
kfit <- kmeans(na.omit(d2), 5)

rect.hclust(fit, k=5, border="red")


      #-----------------Whoaround--------


whoaround_plot<-whoaround_data[c("Participant","Survey","App","WSpouse","WKids","WVisitors","WOutsider", "Trigger_category","Action_category")]
na.omit(whoaround_plot)

mean(as.numeric(as.character( whoaround_plot$WSpouse)))
mean(as.numeric(as.character(whoaround_plot$WKids)))
mean(as.numeric(as.character(whoaround_plot$WVisitors)))
mean(as.numeric(as.character(whoaround_plot$WOutsider)))


sd(as.numeric(as.character( who_plot$Spouse)))
sd(as.numeric(as.character(who_plot$Kids)))
sd(as.numeric(as.character(who_plot$Visitors)))
sd(as.numeric(as.character(who_plot$Outsider)))

colnames(whoaround_plot)<- c("Participant","Survey","App","Spouses","Kids","Visitors","Outsiders", "Trigger_category","Action_category")
data_long <- gather(whoaround_plot, whoiss, concern, Spouses:Outsiders, factor_key=TRUE)  # wide to long reshaping

data_long<-na.omit( data_long)


WhoaroundConcerns<-df[c("App", "Participant","whoaround_concern")]
WhoaroundConcerns$length<-sapply(strsplit(WhoaroundConcerns$whoaround_concern, " "), length)

WhoaroundConcerns<-na.omit(WhoaroundConcerns)
WhoaroundConcerns<-WhoaroundConcerns[(WhoaroundConcerns$length>0),]
WhoaroundConcerns<-WhoaroundConcerns[(WhoaroundConcerns$length<4),]

write.csv(WhoaroundConcerns, "whoaroundconcern.csv") 



d2 <- data_long %>% 
  group_by(App,whoiss,concern) %>% 
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
plot <- ggplot(d2, aes(x=factor(App),y = 100*perc, fill=factor(concern))) +
  geom_bar(stat="identity",width = 1) + xlab(labs(title="Applets")) + ylab(labs(title="Who is around")) +
  facet_wrap(whoiss~., nrow = 1)+
  theme(axis.text.x=element_blank(),legend.position="bottom", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  
  scale_fill_manual(values=c("brown3","salmon", "burlywood1","palegreen3","seagreen"))
print(plot)

data_long$Survey<-factor(data_long$Survey)
data_long$App<-factor(data_long$App)
data_long$whoiss<-factor(data_long$whoiss)
#data_long$concern<-as.numeric((data_long$concern))

data_long$concern<-factor((data_long$concern), ordered = TRUE)
#model<-(lmer(concern ~ whoiss+(1|Participant)+(1|App), data = data_long))



#-------
ordinalmodel<- clmm(concern ~ whoiss+(1|Participant) + (1|App) + (1|Survey), data = data_long)
nullm<- clmm(concern ~ whoiss+(1|Participant) + (1|App) , data = data_long)
nullmmm<- clmm(concern ~ whoiss+(1|Participant)  , data = data_long)

#surveymodel<- clmm(concern ~ whoiss+ Survey+(1|Participant) + (1|App), data = data_long)
nagelkerke(fit  = ordinalmodel,
           null = nullmmm)

ordinalmodelnull<- clmm(concern ~ 1+(1|Participant) + (1|App) + (1|Survey), data = data_long)

nagelkerke(fit  = ordinalmodel,
           null = ordinalmodelnull)



Anova(ordinalmodel,
      type = "II")



marginal = lsmeans(ordinalmodel,
                   pairwise ~ whoiss,
                   adjust="tukey")         ###  Tukey-adjusted comparisons
marginal




      #-----------------shared-----------


  shared_plot<-shared_data[c("App","shared")]
  
  
 
  shared_plot<-na.omit( shared_plot)
  
  shareConcerns<-df[c("App","Participant", "shared_concern")]
  shareConcerns<-na.omit(shareConcerns)
  shareConcerns<-shareConcerns[!(shareConcerns$shared_concern==""),]
  shareConcerns$length<-sapply(strsplit(shareConcerns$shared_concern, " "), length)
  shareConcerns<-shareConcerns[(shareConcerns$length>0),]
  
  
  write.csv(shareConcerns, "shareConcerns.csv") 
  
  d2 <- shared_plot %>% 
    group_by(App,shared) %>% 
    summarise(count=n()) %>% 
    mutate(perc=count/sum(count))
  plot <- ggplot(d2, aes(x=factor(App),y = 100*perc, fill=factor(shared))) +
    geom_bar(stat="identity",width = 0.7) + xlab(labs(title="Apps")) + ylab(labs(title="shared")) +
    coord_flip()+
    theme(axis.text.x = element_text( angle=50))
    
    #scale_fill_manual(values=c("red", "green","yellow"))
  print(plot)
  
  #---anova?????????  
  
  q<-shared_plot %>% dplyr::select(App, shared)%>%
    filter(shared == 4)
  shared_concern <- d2 %>% dplyr::select(App, shared, perc)%>%
    filter(shared == 4)
  mean(shared_concern$perc)
  
  shared_plot$App<-factor(shared_plot$App)
  shared_plot$shared<-as.numeric(as.character(shared_plot$shared))
  anova(lm(shared ~ App,data=shared_plot))

 lsmeans(model,
         pairwise ~ App,
         adjust="tukey") 
  
  
  an<-aov(shared ~ App , data = shared_plot)
  etaSquared( an, type = 2, anova = FALSE )  #effect size
 
 
  

  #---------------Analysis on applets that Location is as trigger---------------------
 
  Location_applets<-  df%>% 
    dplyr::select(App, Trigger_category,Action_category, Morning, Afternoon, Night, Kitchen, Livingroom,
           Bathroom,Bedroom,Frontdoor,Spouse,Kids,Visitors,Outsider, WSpouse,WKids
           ,WVisitors,WOutsider,AKitchen, ALivingroom,ABedroom,ABathroom, AFrontdoor,
           shared) %>% 
    filter(Trigger_category == "Location" & (Action_category=="Communication" | Action_category=="Organization" | Action_category=="SocialNetworks"  | Action_category=="CloudStorage"))
  
  Location_applets_long <- gather(Location_applets, CF, concern, Morning:shared, factor_key=TRUE)  # wide to long reshaping
 Location_applets_long[Location_applets_long==""] <- NA
 Location_applets_long<-na.omit( Location_applets_long)
 
  cols<-"concern"
  Location_applets_long <- Location_applets_long %>% mutate_at(cols, funs(recode_factor(., "Extremely Concerned"=5L, "Moderately Concerned"=4L, "Slightly Concerned"=2L, "Somewhat concerned"=3L, "Not at all Concerned"=1L, .default = NULL)))
  
  model<-lm(as.numeric(concern) ~ CF + Action_category +CF:Action_category , data = Location_applets_long)
  an<-anova(model)
  
  lsmeans(model,
          pairwise ~ CF,
          adjust="tukey") 
  lsmeans(model,
          pairwise ~ Action_category,
          adjust="tukey") 
  a<-lsmeans(model,
          pairwise ~ CF:Action_category,
          adjust="tukey") 
  
  summary(a)
  
  
  
#---------********RQ3************
#---------------****RQ3*****-------------------------------  

#--------****Demographics Analysis*****-------------
  Demographics <- dplyr::select(df,Participant, Survey,Gender,Age,Education, IFTTT, IoT, IUIPC1, IUIPC2, IUIPC3, IUIPC4, IUIPC5, IUIPC6, IUIPC7, IUIPC8, IUIPC9)
  
  Demographics<-distinct(Demographics)
  Demographics<- cbind(Demographics, replicate(1,Demographics$IUIPC5)) 
  
  colnames(Demographics) <- c("Participant","Survey","Gender","Age","Education", "IFTTT", "IoT",
                                             "IUIPC1", "IUIPC2", "IUIPC3", "IUIPC4", "IUIPC5", "IUIPC7", "IUIPC8", "IUIPC9", "IUIPC10","IUIPC6")

    #-------For each IUIPC score we calculate mean of IUIPCs related to that score-----------
  factor_cols<-c("IUIPC1", "IUIPC2", "IUIPC3", "IUIPC4", "IUIPC5", "IUIPC7", "IUIPC8", "IUIPC9", "IUIPC10","IUIPC6")
  Demographics <- Demographics %>% mutate_at(factor_cols, funs(recode_factor(., "Strongly agree"=5L, "Somewhat agree"=4L, 
                                                                             "Neither agree nor disagree"=3L, "Somewhat disagree"=2L,  "Strongly disagree"=1L, .default = NULL)))
  
  
  Demographics <- Demographics %>% mutate(Awareness=(as.numeric(as.character(IUIPC4))+as.numeric(as.character(IUIPC5))+as.numeric(as.character(IUIPC6)))/3,
                                                     Control=(as.numeric(as.character(IUIPC1))+as.numeric(as.character(IUIPC2))+as.numeric(as.character(IUIPC3)))/3,
                                                    Collection=(as.numeric(as.character(IUIPC7))+as.numeric(as.character(IUIPC8))+as.numeric(as.character(IUIPC9))+as.numeric(as.character(IUIPC10)))/4)
  
  Demographics <- Demographics %>% mutate_at("Education", funs(recode_factor(., "Less than high school"=1L, "High school graduate"=2L,
                                                           "Some college"=2L,"2 year degree"=3L,  "4 year degree"=3L,
                                                           "Doctorate"=4L,"Professional degree"=4L ,.default = NULL)))
  
  Demographics$IFTTT[Demographics$IFTTT == "5-Mar"] <- "4-Mar" # fix the problem with survey design
  
  Demographics <- Demographics %>% mutate_at(c("IFTTT","IoT"), funs(recode_factor(., "0"=0L, "2-Jan"=1L,
                                                                             "4-Mar"=2L,  "More than 5"=3L
                                                                              ,.default = NULL))) # recode
  
  Demographics$background<-apply(Demographics[,c("IFTTT","IoT")], 1, max)# max (IoT, IFTTT) is set as background level of participant
  
  
  Concern_scenario <-dplyr::select(df,App,App_category,Trigger_category, Action_category, scenario_concern,CF_violation)
  
  Concern_scenario$scenario_concern = factor(Concern_scenario$scenario_concern,
                                             levels = concern_level,
                                             ordered = TRUE)
  
  temp <-dplyr::select(df,Participant,App,App_category,Trigger_category, Action_category,privacy, scenario_concern,CF_violation)
  
  factor_cols<-c("privacy","scenario_concern")
  temp <- temp %>% mutate_at(factor_cols, funs(recode_factor(., "Extremely concerned"=5L, "Moderately concerned"=4L, 
   
                                                             "Slightly concerned"=2L, "Somewhat concerned"=3L, "Not at all concerned"=1L, .default = NULL)))
 temp <- temp %>% mutate(delta_concern = as.numeric(as.character(scenario_concern))- as.numeric(as.character(privacy)))
  
 
 mean(Demographics$Awareness)
 sd(Demographics$Awareness)

 max(Demographics$Awareness)
 min(Demographics$Awareness)
  
 mean(Demographics$Collection)
 sd(Demographics$Collection)
 max(Demographics$Collection)
 min(Demographics$Collection)
 
 mean(Demographics$Control)
 sd(Demographics$Control) 
 
 max(Demographics$Control)
 min(Demographics$Control) 
 
   
   
  
  RQ3<-full_join(temp, Demographics, by = "Participant")

  RQ3$privacy<-as.numeric(as.character(RQ3$privacy))
  RQ3$scenario_concern<-as.numeric(as.character(RQ3$scenario_concern))


  
  Survey1 <- RQ3[ which(RQ3$App==1 |  RQ3$App==8 | RQ3$App==15 | RQ3$App==22 | RQ3$App==29 | RQ3$App==36 | RQ3$App==43 ),]
  
  Survey1 <- RQ3[ which(RQ3$Survey==1), ]
  
 
  
  #------learning effect check---------
  Concern_applets <- dplyr::select(df,Survey,Participant, App, App_category,Trigger_category, Action_category, privacy)
  
  Concern_applets_numerical <- Concern_applets %>% mutate_at("privacy", funs(recode_factor(., "Extremely concerned"=5L, "Moderately concerned"=4L, "Somewhat concerned"=3L,"Slightly concerned"=2L,  "Not at all concerned"=1L, .default = NULL)))
  
  
  
  Concern_applets_numerical$App<-factor(Concern_applets_numerical$App)
  Concern_applets_numerical$privacy<-as.numeric(as.character(Concern_applets_numerical$privacy))
  
  
  Concern_applets_numerical$App<-(as.numeric (Concern_applets_numerical$App))%%7
  
  Concern_applets_numerical$App<-factor(Concern_applets_numerical$App)

  
  Concern_applets_numerical$privacy<-factor((Concern_applets_numerical$privacy), ordered = TRUE)
  Survey1 <- Concern_applets_numerical[ which(Concern_applets_numerical$Survey==1), ]
  
  Concern_applets_numerical$privacy<-factor((Concern_applets_numerical$privacy), ordered=TRUE)
  
  nullmodel<-clmm(privacy~ 1+(1|Participant), data = Concern_applets_numerical)
  Agemodel<-clmm(privacy~ App+(1|Participant), data = Concern_applets_numerical)
  nagelkerke(fit  = Agemodel,
             null = nullmodel)
  

  
  #--------RQ1 demographic------
  RQ1<-full_join(temp, Demographics, by = "Participant")
  #RQ1 <- RQ1 %>% mutate_at("Education", funs(recode_factor(., "Less than high school"=1L, "High school graduate"=2L,
  #                                                       "Some college"=2L,"2 year degree"=3L,  "4 year degree"=3L,
  #                                                      "Doctorate"=4L,"Professional degree"=4L ,.default = NULL)))
  
 RQ1$App<-factor(RQ1$App)
  
 RQ1$privacy<-as.numeric(as.character(RQ1$privacy))
 RQ1$scenario_concern<-as.numeric(as.character(RQ1$scenario_concern))
 
 
 RQ1_mean<-  RQ1[,c("Participant","privacy","scenario_concern")]  %>%  
   group_by(Participant) %>%  summarise_each(funs(mean))
 RQ1_mean<-full_join(RQ1_mean, Demographics, by = "Participant")


 RQ1clust<-RQ1_mean[,c("Awareness","Collection","Control")]
 
 
 set.seed(42)
 k2 <- kmeans(RQ1clust,3, iter.max=500)
 
 str(k2)
 
 #Elbow Method for finding the optimal number of clusters
 set.seed(123)
 # Compute and plot wss for k = 2 to k = 15.
 k.max <- 15
 data <- RQ1clust
 wss <- sapply(1:k.max, 
               function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
 wss
 plot(1:k.max, wss,
      type="b", pch = 19, frame = FALSE, 
      xlab="Number of clusters K",
      ylab="Total within-clusters sum of squares")
 
 
 RQ1clust$cluster<-k2$cluster
 RQ1clust$scenario_concern<-RQ1_mean$scenario_concern
 RQ1clust$privacy<-RQ1_mean$privacy
 RQ1clust[,c("Gender","Age","Education","background")]<-Demographics[,c("Gender","Age","Education","background")]
 RQ1clust<-mutate(RQ1clust,delta=RQ1clust$scenario_concern - RQ1clust$privacy)
 
 group<-compareGroups(cluster~.,data=RQ1clust)
 clustab<-createTable(group)
 clustab
 
 model<-lm(privacy~cluster, data = RQ1clust)
 an<-anova(model)
 an
 model<-lm(scenario_concern~cluster, data = RQ1clust)
 an<-anova(model)
 an
 
 model<-lm(delta~cluster, data = RQ1clust)
 an<-anova(model)
 an
 
 kruskal.test(delta~cluster, data = RQ1clust)
 
 #fviz_cluster(k2, data = RQ1_mean)
 
 ###---------
 RQ1$privacy<-factor((RQ1$privacy), ordered=TRUE)
 
 nullmodel<-clmm(privacy~ 1+(1|App), data = RQ1)
 Agemodel<-clmm(privacy~ Age+(1|App), data = RQ1)
 nagelkerke(fit  = Agemodel,
            null = nullmodel)
 
 AgeGmodel<-clmm(privacy~ Age+Gender+(1|App), data = RQ1) 
 nagelkerke(fit  = AgeGmodel,
            null = Agemodel)
 
 AgeGEmodel<-clmm(privacy~ Age+Gender+Education+(1|App), data = RQ1) 
 nagelkerke(fit  = AgeGEmodel,
            null = AgeGmodel) 
 AgeGEImodel<-clmm(privacy~ Age+Gender+Education+IoT+(1|App), data = RQ1) 
 nagelkerke(fit  = AgeGEImodel,
            null = AgeGEmodel)
 nagelkerke(fit  = AgeGEIFmodel,
            null = nullmodel)
 

 
 m= anova(nullmodel,AgeGEIFmodel,test="Chisq")
 
 AgeGEIFmodel<-clmm(privacy~ Age+Gender+Education+IoT+IFTTT+(1|App), data = RQ1) 
 marginal = lsmeans(AgeGEIFmodel,
                    pairwise ~ Age + Gender,
                    adjust="tukey")        ### Tukey-adjusted comparisons
 marginal
 
 summary(AgeGEIFmodel)
 Anova(nullmodel,
       type = "II")
 #-----Age
 model<-lm(privacy~Age+App, data = RQ1)
 an<-anova(model)
 an
 lsmeans(model,
         pairwise ~ Age,
         adjust="tukey") 
 
 an<-aov(privacy ~Age , data = RQ1)
 etaSquared( an, type = 2, anova = FALSE )  #effect size
 
 nullmodel<-clmm(privacy~ 1+(1|App), data = RQ1)
 
 nagelkerke(fit  = model,
            null = nullmodel)
 
 nullmodel<-clmm(privacy~ 1+(1|App)+(1|Participant), data = RQ1)
 Agemodel<-clmm(privacy~ Age+(1|App)+(1|Participant), data = RQ1)
 nagelkerke(fit  = Agemodel,
            null = nullmodel)
 
 
 lsmeans(Agemodel,
         pairwise ~ Age,
         adjust="tukey") 
 
 
 #-----Gender
  
  model<-lm(privacy~Gender+App, data = RQ1)
  an<-anova(model)
  an
  lsmeans(model,
          pairwise ~ Gender,
          adjust="tukey") 
  
  
  
  an<-aov(mean ~Gender , data = RQ1)
  etaSquared( an, type = 2, anova = FALSE )  #effect size
  

  
  model<-clmm(privacy~Gender+ (1|App)+(1|Participant), data = RQ1)
  nullmodel<-clmm(privacy~ 1+(1|App)+(1|Participant), data = RQ1)
  anova(model,nullmodel)
  nagelkerke(fit  = model,
             null = nullmodel)
 
  lsmeans(model,
          pairwise ~ Gender,
          adjust="tukey") 
 
  
  #-------Education
  
  model<-lm(privacy~Education+App, data = RQ1)
  an<-anova(model)
  an
  lsmeans(model,
          pairwise ~ Education,
          adjust="tukey") 
  
  an<-aov(privacy ~Education , data = RQ1)
  etaSquared( an, type = 2, anova = FALSE )  #effect size
  
  model<-clmm(privacy~Education+ (1|App) + (1|Participant), data = RQ1)
  nullmodel<-clmm(privacy~ 1+(1|App)+ (1|Participant), data = RQ1)
  
  nagelkerke(fit  = model,
             null = nullmodel)
 
  lsmeans(model,
          pairwise ~ Education,
          adjust="tukey") 
  
  #------background
  RQ1$privacy<-factor((RQ1$privacy), ordered=TRUE)
  
  
  model<-clmm(privacy~IFTTT+ (1|App)+(1|Participant), data = RQ1)
  nullmodel<-clmm(privacy~ 1+(1|App)+(1|Participant), data = RQ1)
  
  nagelkerke(fit  = model,
             null = nullmodel)
  an<-anova(model)
  an
  lsmeans(model,
          pairwise ~ IFTTT,
          adjust="tukey") 
  
  an<-aov(privacy ~IFTTT , data = RQ1)
  etaSquared( an, type = 2, anova = FALSE )  #effect size
  
  
  model<-clmm(privacy~IFTTT+ (1|App), data = RQ1)
  nullmodel<-clmm(privacy~ 1+(1|App), data = RQ1)
  
  nagelkerke(fit  = model,
             null = nullmodel)
  
  lsmeans(model,
          pairwise ~ IFTTT,
          adjust="tukey") 
  
  
  #-------IFTTT
  
  model<-lm(privacy~IFTTT+App, data = RQ1)
  an<-anova(model)
  an
  lsmeans(model,
          pairwise ~ IFTTT,
          adjust="tukey") 
  
  an<-aov(privacy ~IFTTT , data = RQ1)
  etaSquared( an, type = 2, anova = FALSE )  #effect size
  
  #-------IoT
  model<-lm(privacy~IoT+App, data = RQ1)
  an<-anova(model)
  an
  lsmeans(model,
          pairwise ~ IoT,
          adjust="tukey") 
  
  an<-aov(privacy ~IoT , data = RQ1)
  etaSquared( an, type = 2, anova = FALSE )  #effect size
  
  model<-clmm(privacy~IoT+ (1|App)+(1|Participant), data = RQ1)
  nullmodel<-clmm(privacy~ 1+(1|App)+(1|Participant), data = RQ1)
  
  nagelkerke(fit  = model,
             null = nullmodel)
  an<-anova(model)
  an
  lsmeans(model,
          pairwise ~ IoT,
          adjust="tukey") 
  
  #-------Awareness
  cor_result<-cor.test(RQ1$privacy, RQ1$Awareness, method="spearman")
  cor_result<-cor.test(RQ1$privacy, RQ1$Collection, method="pearson")
  cor_result<-cor.test(RQ1$privacy, RQ1$Control, method="pearson")
  
  
  
  model<-lm(privacy~Awareness+App, data = RQ1)
  an<-anova(model)
  an
  lsmeans(model,
          pairwise ~ Awareness,
          adjust="tukey") 
  
  an<-aov(privacy ~Awareness , data = RQ1)
  etaSquared( an, type = 2, anova = FALSE )  #effect size
  
  #-------Control
  
  model<-lm(privacy~Control+App, data = RQ1)
  an<-anova(model)
  an
  an<-aov(privacy ~Control , data = RQ1)
  etaSquared( an, type = 2, anova = FALSE )  #effect size
  
  #-------Collection
  model<-lm(privacy~Collection+App, data = RQ1)
  an<-anova(model)
  an
  an<-aov(privacy ~Collection , data = RQ1)
  etaSquared( an, type = 2, anova = FALSE )  #effect size
  
    #-----------Locationapplets RQ1-------

Locationsharing_applets<-temp %>% dplyr::select(Participant, App, Action_category,Trigger_category,privacy, scenario_concern)%>%
  filter(Trigger_category == "Location" & (Action_category == "SocialNetworks" | Action_category=="Organization"
                                           | Action_category=="Communication" | Action_category=="CloudStorage"))

NLocationsharing_applets<-temp %>% dplyr::select(Participant, App, Action_category,Trigger_category,privacy, scenario_concern)%>%
  filter(!(Trigger_category == "Location" & (Action_category == "SocialNetworks" | Action_category=="Organization"
                                           | Action_category=="Communication" | Action_category=="CloudStorage")))
LocvsNloc <- data.frame(type= "loc", privacy=Locationsharing_applets$privacy, Participant=Locationsharing_applets$Participant)
mean(as.numeric(as.character(LocvsNloc$privacy)))
sd(as.numeric(as.character(LocvsNloc$privacy)))



LocvsNloc2 <- data.frame(type="Nloc", privacy=NLocationsharing_applets$privacy, Participant=NLocationsharing_applets$Participant)

mean(as.numeric(as.character(LocvsNloc2$privacy)))
sd(as.numeric(as.character(LocvsNloc2$privacy)))

LNL<-rbind(LocvsNloc,LocvsNloc2)

LNL$privacy<-as.numeric(as.character(LNL$privacy))
LNL$Participant<-factor(LNL$Participant)
model<-glmer(privacy~type|Participant, data=LNL, family = poisson)
model<-lm(privacy~type, data = LNL)
an<-anova(model)
an
lsmeans(model,
        pairwise ~ type,
        adjust="tukey") 

an<-aov(privacy ~type , data = LNL)

etaSquared( an, type = 2, anova = FALSE )  #effect size



