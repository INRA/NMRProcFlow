FROM ubuntu:16.04

MAINTAINER "Daniel Jacob" daniel.jacob@u-bordeaux.fr

RUN apt-get update && apt-get -y upgrade && apt-get install -y \
      apache2 \
      php \
      libapache2-mod-php \
      php-gd

# Install modules
RUN apt-get update && apt-get install -y \
    sudo ca-certificates wget curl git vim ed p7zip \
    libfreetype6-dev libmcrypt-dev libpng12-dev libcurl4-gnutls-dev libcairo2-dev \
    libxt-dev libxml2-dev libv8-dev gnuplot \
    gdebi-core \
    libssl-dev openssl software-properties-common \
    libgsl0-dev gsl-bin libnlopt-dev


# See https://www.digitalocean.com/community/tutorials/how-to-install-r-on-ubuntu-16-04-2
RUN sh -c 'echo "deb http://cran.rstudio.com/bin/linux/ubuntu xenial/" >> /etc/apt/sources.list' && \
    apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9

# Install R / ...
RUN apt-get update && apt-get install -y \
    r-recommended \
    r-cran-rcurl \
    r-cran-foreach \
    r-cran-multicore \
    r-cran-base64enc \
    r-cran-xml \
    r-cran-rcpp \
    r-base-dev \
    apache2-dev

# See https://github.com/jeffreyhorner/rapache
RUN cd /usr/src \
 && git clone https://github.com/jeffreyhorner/rapache.git \
 && cd rapache \
 && ./configure \
 && make && make install \
 && cd /usr/src && rm -rf ./rapache

RUN apt-get update && apt-get install -y locales \
    && echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen \
    && locale-gen en_US.utf8 \
    && /usr/sbin/update-locale LANG=en_US.UTF-8

ENV LC_ALL en_US.UTF-8
ENV LANG en_US.UTF-8

# Download and install shiny server
RUN wget --no-verbose http://download3.rstudio.org/ubuntu-12.04/x86_64/VERSION -O "version.txt" && \
    VERSION=$(cat version.txt)  && \
    wget --no-verbose "http://download3.rstudio.org/ubuntu-12.04/x86_64/shiny-server-$VERSION-amd64.deb" -O ss-latest.deb && \
    gdebi -n ss-latest.deb && \
    rm -f version.txt ss-latest.deb

# Install dependencies
RUN R -e "install.packages(c('gsl','RcppGSL','inline','rjson'), repos='http://cran.rstudio.com')"

# Install Shiny and some extensions and some other dependencies
RUN R -e "install.packages( c( 'httpuv', 'mime', 'jsonlite', 'xtable', 'htmltools', 'R6', 'shiny'), repos='http://cran.rstudio.com/')"
RUN R -e "install.packages(c('shinyBS', 'shinyjs', 'stringi', 'docopt','doParallel','signal','ptw', 'openxlsx'), repos='http://cran.rstudio.com')"
RUN R -e "source('http://bioconductor.org/biocLite.R'); biocLite('MassSpecWavelet'); biocLite('impute'); install.packages('speaq', repos='http://cran.rstudio.com')"

ENV APACHE_RUN_USER www-data
ENV APACHE_RUN_GROUP www-data
ENV APACHE_LOG_DIR /var/log/apache2
ENV APACHE_PID_FILE /var/run/apache2.pid
ENV APACHE_RUN_DIR /var/run/apache2
ENV APACHE_LOCK_DIR /var/lock/apache2
ENV APACHE_SERVERADMIN admin@localhost
ENV APACHE_SERVERNAME localhost
ENV APACHE_SERVERALIAS docker.localhost
ENV APACHE_DOCUMENTROOT /var/www/html

COPY ./etc/apache2.conf /etc/apache2/apache2.conf
COPY ./nmrspec/conf/shiny-server.conf /etc/shiny-server/shiny-server.conf
COPY ./nmrspec/conf/my-site.conf /etc/apache2/sites-enabled/001-my-site.conf
COPY ./nmrspec/conf/launch-server.sh /usr/bin/launch-server.sh

# NMRVIEW
COPY ./nmrviewer/src /opt/

RUN cd /opt/ && make

ADD ./nmrviewer/www /var/www/html/nv

RUN mkdir -p /opt/data \
    && rm -f /etc/apache2/sites-enabled/000-default.conf \
    && chmod 755 /usr/bin/launch-server.sh
#    && mkdir -p /var/www/html/nv/tmp \
#    && chown -R www-data.www-data /var/www/html/nv \
#    && chmod 777 /var/www/html/nv/tmp

# Volume as the root directory that will contain the output data
# the data will be stored within the directory /opt/data/<DATAID>
VOLUME /opt/data

# NMRSPEC
ADD ./nmrspec /srv/shiny-server

RUN chown -R shiny.shiny /srv/shiny-server \
    && adduser www-data shiny

## Install Rnmr1D package
RUN cd /home && \
    echo 'library(Rcpp); Rcpp.package.skeleton(name="Rnmr1D", code_files="/srv/shiny-server/exec/libspec/Rnmr.R",  cpp_files = "/srv/shiny-server/exec/libspec/libCspec.cpp", example_code = FALSE, author="Daniel Jacob", email="djacob65@gmail.com"); ' | /usr/bin/R BATCH --vanilla && \
    cp /srv/shiny-server/exec/libspec/configure* ./Rnmr1D/ && \
    chmod 755 ./Rnmr1D/configure* && \
    cp /srv/shiny-server/exec/libspec/Makevars.in ./Rnmr1D/src && \
    echo 'install.packages("Rnmr1D", lib=c("/usr/local/lib/R/site-library/"), repos = NULL, type = "source");' | /usr/bin/R BATCH --vanilla && \
    [ -d "./Rnmr1D" ] && rm -rf ./Rnmr1D && rm -rf /srv/shiny-server/exec/libspec

WORKDIR /var/www/html

#EXPOSE 3838
EXPOSE 80

CMD ["/usr/bin/launch-server.sh"]

