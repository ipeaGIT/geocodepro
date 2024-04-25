# infelizmente isso precisa vir aqui pra que a gente consigar usar o {arrow}. o
# sistema de diagnóstico do rstudio pré-carrega os pacotes usados no script
# sendo editado, o que pode fazer com que o arrow seja carregado e
# consequentemente inviabilize o use do arcpy. essa chamada no .Rprofile,
# portanto, força a versão do arcpy logo no startup.
#
# notar que isso é diferente da chamada feita no corpo da função para também
# forçar a versão do arcpy, já que aquela chamada é necessária para o pacote em
# si funcionar, enquanto esta é necessária para desenvolver o pacote no rstudio.
reticulate::use_condaenv(
  "C://Program Files/ArcGIS/Pro/bin/Python/envs/arcgispro-py3"
)
invisible(reticulate::import("arcpy"))

if (file.exists("~/.Rprofile")) source("~/.Rprofile")
