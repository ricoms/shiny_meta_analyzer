FROM rocker/shiny
LABEL maintainer="ricardosavii@gmail.com"

WORKDIR /opt/program
RUN R -e "install.packages('plyr',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinydashboard',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('rhandsontable',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('ggplot2',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('meta',dependencies=TRUE, repos='http://cran.rstudio.com/')"

COPY scripts/turnToBin.R scripts/
COPY dictionary.csv .
RUN Rscript scripts/turnToBin.R
RUN rm -r scripts
RUN rm dictionary.csv

COPY server.R .
COPY ui.R .
COPY data_examples_app.R .
COPY define_meta_app.R .
COPY www/* www/

EXPOSE 3838

CMD R -e "options('shiny.port'=3838,shiny.host='0.0.0.0'); shiny::runApp('.')"
