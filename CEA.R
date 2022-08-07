CEA<-function(Pob=100000, t=52, ProbHosp=0.15, ProbExitus=0.05, ProbAlta=0.95,
              ProbUCI=0.15, ProbPlanta=0.5){
  inf<-vector(length=t, mode="numeric")
  Hosp<-vector(length=t, mode="numeric")
  UCI<-vector(length=t, mode="numeric")
  Exitus<-vector(length=t, mode="numeric")
  Curados<-vector(length=t, mode="numeric")
  
  inf[1]<-Pob
  Hosp[1]<-0
  UCI[1]<-0
  Exitus[1]<-0
  Curados[1]<-0
  
  for(i in 2:t){
  inf[i]<-inf[i-1]-(ProbHosp+ProbAlta)*inf[i-1]
  Hosp[i]<-Hosp[i-1]+ProbHosp*inf[i-1]-Hosp[i-1]*(ProbExitus+ProbAlta+ProbUCI)
  UCI[i]<-UCI[i-1]+ProbUCI*Hosp[i-1]-UCI[i-1]*(ProbExitus+ProbPlanta)
  Exitus[i]<-Exitus[i-1]+Hosp[i-1]*ProbExitus+UCI[i-1]*(ProbExitus)
  Curados[i]<-Curados[i-1]+Hosp[i-1]*ProbAlta
  }
  list(inf=inf,Hosp=Hosp,UCI=UCI,Exitus=Exitus,Curados=Curados)
}