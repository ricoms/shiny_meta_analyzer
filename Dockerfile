FROM rocker/shiny:4.0.0
LABEL maintainer="ricardosavii@gmail.com"

RUN R -e "install.packages('plyr',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinydashboard',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('rhandsontable',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('ggplot2',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('meta',dependencies=TRUE, repos='http://cran.rstudio.com/')"

COPY scripts .
RUN Rscript turnToBin.R
RUN rm turnToBin.R dictionary.csv

COPY src /srv/shiny-server/
RUN cp translation.bin /srv/shiny-server/translation.bin
WORKDIR /srv/shiny-server/

EXPOSE 3838

CMD ["R", "-e", "options('shiny.port'=3838,shiny.host='0.0.0.0'); shiny::runApp('.')"]
