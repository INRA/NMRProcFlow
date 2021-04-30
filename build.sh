#!/bin/bash

sh ./run stop
sh ./run build
dockclean
dockpurge
sh ./run start

