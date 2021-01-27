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
    gdebi-core  \
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


CMD ["/bin/bash"]

