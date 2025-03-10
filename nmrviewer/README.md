# NMRViewer Docker Image - Standalone Installation 

   * NMRViewer module is included within the NMRProcFlow docker image, but you can also build and use NMRViewer as a standalone docker image

### Based on: Docker Apache + php Image
   * cf https://hub.docker.com/_/php

### Docker NMRViewer Container

   * FROM Docker Apache + php
   * Add php extensions
   * Add gnuplot
   * COPY src of bin4gp_1r tool && compile
   * Shared volume
          * application  : -v /<mrviewer path>/www:/var/www/html/nv
          * data         : -v /<data root path>:/opt/data
   * web link : http://<your_host>/nv/view/<session identifier>

          * the <session identifier> must correspond to a subdirectory under /opt/data, with at least the following files:

               * specs.pack : The matrix of the binary spectra (N spectra x M points) (Binary format)
               * samples.csv The sample file that links together Raw name, sample name, factor names for each spectrum (TXT format, semicolon as separator)
               * factors : List of information type available for the NMR spectra viewer (TXT format, semicolon as separator)
               * nuc.txt : Acquisition Nucleus (1H, 13C or 31P)(TXT format)
               * ppmrange.txt: The full ppm range of the spectra in the frequency domain (ppm max;ppm min)(TXT format, semicolon as separator)


##  INSTALL

* get the php:7.4.18-apache

```
   $ docker php:7.4.18-apache
```

* build the nmrview docker image

```
   $ docker build -t nmrview .
```

##  RUN

* Run an instance (container) of the nmrview docker image

```
  $ docker run -it --rm -v /opt/data:/opt/data -p 80:80 --name nvapp nmrview
```


