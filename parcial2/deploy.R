library(rsconnect)

rsconnect::setAccountInfo(name='srolcv-juan0pablo-aldana0henao', token='2F03680965CD8700403C5EE7E524E30E', secret='DYr0uFVSoRc28w2kYHzupj1QmXL4fOzvG0256sOu')

rsconnect::deployApp(
  appDir  = "C:/Users/juanh/Desktop/UNAL/SEMESTRE 2026-1/Mineria de datos/parcial2",
  appName = "annual-reviews"
)
