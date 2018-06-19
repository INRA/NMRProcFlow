# NMRViewer Docker Image - Standalone Installation 

   * NMRViewer module is included within the NMRProcFlow docker image, but you can also build and use NMRViewer as a standalone docker image

### Based on: Docker Apache + php Image
   * cf https://registry.hub.docker.com/_/php/
        https://github.com/docker-library/php/tree/master/5.6

### Docker NMRViewer Container

   * FROM Docker Apache + php
   * Add php extensions
   * Add gnuplot
   * COPY src of bin4gp_1r tool && compile
   * Shared volume
          * application  : -v /<mrviewer path>/www:/var/www/html/nv
          * data         : -v /<data root path>:/opt/data
   * web link : http://<your_host>/nv/view/<session identifier>

          * the <session identifier> must correspond to a subdirectory under /opt/data, with the following files:

               * specs.pack : The matrix of the binary spectra (N spectra x M points) (Binary format)
               * samples.csv The sample file that links together Raw name, sample name, factor names for each spectrum (CSV format)
               * factors : List of information type available for the NMR spectra viewer (TXT format)


##  INSTALL

* get the php:5.6-apache image

```
   $ docker pull php:5.6-apache
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


