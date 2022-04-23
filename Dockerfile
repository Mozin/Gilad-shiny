FROM 023356703893.dkr.ecr.ap-southeast-2.amazonaws.com/shiny-base:latest

RUN mkdir /root/app

COPY . /root/app/

RUN R -e "install.packages(c('Kendall'), repos='http://cran.rstudio.com/', dependencies = T)"

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/app', port = 3838, host = '0.0.0.0')"]