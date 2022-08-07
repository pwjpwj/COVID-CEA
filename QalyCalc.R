QalyCalc<-function(pob1=100000, m=0, edad=65, r=0.03, w=1){
  ajuste=-13
  Qaly<-vector(mode="numeric", length(edad:100))
  pob<-vector(mode="numeric", length(edad:100))
  pob[1]=pob1
  Qaly[1]=pob1
  for (i in 2:(99-edad)) {
    pob[i]<-pob[i-1]-pob[i-1]*m[i+edad+ajuste,3]
    Qaly[i]<-Qaly[i]+(pob[i]*w)/(1+r)^i
  }
pob
Qaly
}