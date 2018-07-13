FROM php:5.6-apache

MAINTAINER "Daniel Jacob" daniel.jacob@u-bordeaux.fr

# Install modules
RUN apt-get update && apt-get install -y \
        libfreetype6-dev \
        libjpeg62-turbo-dev \
        libmcrypt-dev \
        libpng12-dev \
        gnuplot \
        sudo \
    && docker-php-ext-install iconv mcrypt \
    && docker-php-ext-configure gd --with-freetype-dir=/usr/include/ --with-jpeg-dir=/usr/include/ --with-png-dir=/usr/include/ \
    && docker-php-ext-install gd

COPY ./src /opt/

RUN cd /opt/ \
    && make \
    && rm -f /opt/*

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

ADD ./www /var/www/html/nv
COPY ./www/etc/launch-server.sh /usr/bin/launch-server.sh

RUN  mkdir -p /var/www/html/nv/tmp \
    && chown -R www-data.www-data /var/www/html/nv \
    && chmod 777 /var/www/html/nv/tmp \
    && chmod 755 /usr/bin/launch-server.sh

# Volume as the root directory that will contain the output data
# the data will be stored within the directory /opt/data/<DATAID>
RUN mkdir -p /opt/data
VOLUME /opt/data

WORKDIR /var/www/html

EXPOSE 80

CMD ["/usr/bin/launch-server.sh"]

