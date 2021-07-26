![logo](nmrpf_logo_full.png)

[![SWH](https://archive.softwareheritage.org/badge/origin/https://github.com/INRA/NMRProcFlow/)](https://archive.softwareheritage.org/browse/origin/?origin_url=https://github.com/INRA/NMRProcFlow)
[![NPFLOW](https://img.shields.io/badge/biotools-nmrprocflow-blue)](https://bio.tools/nmrprocflow)


https://bio.tools/nmrprocflow

## NMRProcFlow

An efficient GUI tool for spectra processing from 1D NMR metabolomics data

### Description

* The NMRProcFlow open source software provides an efficient GUI tool for spectra processing from 1D NMR metabolomics data, based on an interactive interface for the spectra visualization, that greatly helps spectra processing. 
* The 'NMR spectra viewer' is the central tool of NMRProcFlow and the core of the application. It allows the user to visually explore the spectra overlaid or stacked, to zoom on intensity scale, to group set of spectra by color based on their factor level.
* NMRProcFlow was built by involving NMR spectroscopists eager to have a quick and easy tool to use.
* the spectra processing includes: the calibration of the ppm scale, the base line correction (locally or  fully), the realignment of a set of ppm areas, and the binning (Intelligent, variable size or fixed width)


### Installation

Requirements:

* a recent Linux OS that support Docker (see https://www.docker.com/)


From GitHub, clone the repository

```
    $ git clone https://github.com/INRA/NMRProcFlow.git
```

Then `cd` to your clone path

```
    $ cd nmrproc
```

You need to have the docker image 'npflow' into your local docker library. For that, two ways are possible:

* You have to create the docker image:

		$ sh ./npflow build


* You can simply pull the docker image from docker hub:

		$ docker pull docker.io/nmrprocflow/npflow

Then, you should have something like below:

	$ docker image
```
REPOSITORY                   TAG                 IMAGE ID            CREATED             SIZE
nmrprocflow/nmrprocflow      latest              c7bf03be9a38        48 seconds ago      1.449 GB
```

Then edit **./etc/npflow.conf**, and pay attention to these lines :

```
# The URL root of the PROXY if applicable
PROXY_URL_ROOT=

# HTTP port number of the web application
HTTP_PORT=8080

# the root of the directory where data will be stored within a subdirectory and used by all modules
# The default path is /opt/data. If you choose to define a different location of the working space, 
# the corresponding directory will be created if it does not exist.
DATASETS=/opt/data

# Duration (in days) of validity of a session 
# before its destruction (counted from the last change)
PURGESESSIONS=2

# Max ZIP size (Mo)
MAXZIPSIZE=400

# NB CORES (0 means Auto)
CORES=0

# User connexion management
# 0 : no connexion management
# 1 : connexion management based on the /opt/data/conf/userlist file
#     Its structure is one user per line and each line following the format:
#        login;LastName;FirstName;Country;Institution;Email;Password
#     a minimal set of this 'userlist' file could be: npflow;;;;;;nppass
USRCONMGR=0
#
```


### Usage

#### Run the application

```
    $ sh ./npflow start
```

#### Stop the application

```
    $ sh ./npflow stop
```


#### View the status of the application

```
    $ sh ./npflow ps
```


#### check on http://<your_local_host>:<port>/npflow/


See more information in http://nmrprocflow.org/c_download

---

[![](https://images.microbadger.com/badges/image/nmrprocflow/nmrprocflow.svg)](https://microbadger.com/images/nmrprocflow/nmrprocflow "Get your own image badge on microbadger.com")

---

### Main contributors:

* Daniel Jacob, Catherine Deborde, Marie Lefebvre, Michaël Maucourt
* Special thanks to Alain Girard (INRA Bordeaux) for designing the logo.

### Publication:

Jacob, D., Deborde, C., Lefebvre, M., Maucourt, M. and Moing, A. (2017) NMRProcFlow: A graphical and interactive tool dedicated to 1D spectra processing for NMR-based metabolomics, Metabolomics 13:36. [doi:10.1007/s11306-017-1178-y](http://link.springer.com/article/10.1007%2Fs11306-017-1178-y)

### Funded by:

* INRAE UMR 1332 BFP, Bordeaux Metabolomics Facility
* the ANR-11-INBS-0010 grant (MetaboHUB)

### License

Copyright (C) 2016-2021  Daniel Jacob - INRAE 

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
