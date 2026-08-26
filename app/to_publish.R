library(rsconnect)
library(quarto)
rsconnect::showLogs()

getwd()                          
rsconnect::deployments(".")      
quarto_publish_app("ethnic_projection_report.qmd")