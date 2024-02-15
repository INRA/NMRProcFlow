FROM php:7.4.18-apache

MAINTAINER "Daniel Jacob" daniel.jacob@inrae.fr

# Install modules
RUN apt-get update && apt-get install -y \
        libfreetype6-dev libjpeg62-turbo-dev libmcrypt-dev libpng-dev \
        gnuplot sudo vim \
# Install & configure mcrypt & gd for PHP
    && pecl install mcrypt \
    && docker-php-ext-enable mcrypt  \
    && docker-php-ext-configure gd --with-freetype-dir=/usr/include/ --with-jpeg-dir=/usr/include/ --with-png-dir=/usr/include/ \
    && docker-php-ext-install gd \
    && rm -f /usr/src/php.tar.*

# Environment for Apache
ENV LC_ALL=en_US.UTF-8 \
    LANG=en_US.UTF-8 \
    APACHE_RUN_USER=www-data \
    APACHE_RUN_GROUP=www-data \
    APACHE_LOG_DIR=/var/log/apache2 \
    APACHE_PID_FILE=/var/run/apache2.pid \
    APACHE_RUN_DIR=/var/run/apache2 \
    APACHE_LOCK_DIR=/var/lock/apache2 \
    APACHE_SERVERADMIN=admin@localhost \
    APACHE_SERVERNAME=localhost \
    APACHE_SERVERALIAS=docker.localhost \
    APACHE_DOCUMENTROOT=/var/www/html

# Copy configuration and source files 
COPY ./src /opt/
COPY ./www/etc/launch-server.sh /usr/bin/launch-server.sh
ADD ./www /var/www/html/nv 

# Final touch
RUN echo 'en_US.UTF-8 UTF-8' > /etc/locale.gen && locale-gen \
    && cd /opt/ && make && rm -f /opt/* \
    && mkdir -p /var/www/html/nv/tmp \
    && chown -R www-data.www-data /var/www/html/nv \
    && chmod 777 /var/www/html/nv/tmp \
    && chmod 755 /usr/bin/launch-server.sh \
    && mkdir -p /opt/data 

# Volume as the root directory that will contain the output data
# the data will be stored within the directory /opt/data/<DATAID>
VOLUME /opt/data

WORKDIR /var/www/html

EXPOSE 80

CMD ["/usr/bin/launch-server.sh"]

