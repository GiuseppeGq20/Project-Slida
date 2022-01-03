
datadir="./AG2_ramp/DATI"

dir=list.dirs(datadir)[-1]

n=c(20,21,22,24,25,42:49)

nstr=as.character(n)
lista=list()

for (i in 1:length(dir)){
  
  files=list.files(dir[i])
  
  listadir=list()
  
  for (file in files) {
    
    append(listadir,read.csv(name)))
           
  }
  append(lista,listadir)
}

paste()
