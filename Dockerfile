FROM r-base:4.0.1
LABEL maintainer="ricardosavii@gmail.com"

# Install Ubuntu packages
RUN apt-get update && apt-get install --no-install-recommends -y \
        sudo=1.9.0-1 \
        gdebi-core=0.9.5.7+nmu3 \
        pandoc=2.5-3+b1 \
        pandoc-citeproc=0.15.0.1-1+b4 \
        libcurl4-gnutls-dev=7.68.0-1 \
        libcairo2-dev/unstable \
        libxt-dev=1:1.1.5-1+b3 \
        libssl-dev=1.1.1g-1 \
    && VERSION=1.5.7.890 \
        && wget --no-verbose "https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-12.04/x86_64/shiny-server-$VERSION-amd64.deb" -O ss-latest.deb \
        && gdebi -n ss-latest.deb \
        && rm -f version.txt ss-latest.deb \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install R packages that are required
RUN R -e "install.packages('plyr',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinydashboard',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('rhandsontable',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('ggplot2',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('meta',dependencies=TRUE, repos='http://cran.rstudio.com/')"

COPY scripts .
RUN Rscript turnToBin.R
RUN rm turnToBin.R dictionary.csv
RUN mv translation.bin /srv/shiny-server/translation.bin

COPY shiny-server.conf  /etc/shiny-server/shiny-server.conf
COPY src /srv/shiny-server/
WORKDIR /srv/shiny-server/
EXPOSE 80

# Copy further configuration files into the Docker image
COPY shiny-server.sh /usr/bin/shiny-server.sh

CMD ["/usr/bin/shiny-server.sh"]
