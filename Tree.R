Tree<-function(ProbIng=0.021, ProbICU=0.001, ProbExitusICU=0.001, ProbExitus=0.001){
  Hosp_ICU_Cure<-ProbIng*ProbICU*(1-ProbExitusICU)
  Hosp_ICU_Exitus<-ProbIng*ProbICU*ProbExitusICU
  Hosp_Cure<-ProbIng*(1-ProbExitus-ProbICU)
  Hosp_Exitus<-ProbIng*ProbExitus
  No_Hosp<-1-ProbIng
  
  Propcada<-c(Hosp_ICU_Cure,Hosp_ICU_Exitus, Hosp_Cure, Hosp_Exitus, No_Hosp)
  names(Propcada)<-c("Hosp_UCI_Curados", "Hosp_UCI_Muertos", "Hosp_Curados",
                    "Hosp_Exitus", "No_Hosp")
  Propcada
}