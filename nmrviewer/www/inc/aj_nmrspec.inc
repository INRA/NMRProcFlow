<?php
/*
# Nom du fichier: getnmrspec.inc
# Auteur(s): D.Jacob
# Copyright: (c) INTA - 2013

    This file is part of NMRProcFlow.

    NMRProcFlow is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    NMRProcFlow is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with NMRProcFlow.  If not, see <http://www.gnu.org/licenses/>.
*/


$start = time();

# View previous selection
if ($conf->prev==1 && $conf->echo_previous_img()==1) exit();

# If full spectra is request and the full spectra image is available, then take this one.
if ($conf->FLGALL && is_file($TMP.'/'.$conf->FULL_SPEC)) {
    $conf->echo_img($conf->FULL_SPEC);
    exit();
}

# Retrieve the list of spectrum, ordered according to the color parameter
$list_spec = $conf->get_list_spectra();

# Write the list of spectrum in the 'listspec' file
if ($conf->WRITE_SELECTED) $conf->write_list_spectra($list_spec);

# Build the PLOT line for GNUPLOT command, based on 1) selected ppm range, 2) the list of spectra and their groups (color)
$conf->get_PLOT_line($list_spec);

# Build the GNUPLOT instructions for bucket zones
$conf->get_Buckets();
$t1 = time();

# Generate the spectra image and send it as output
$status = $conf->generate_Spectra_image();
$t2 = time();

if ($status==0) {
    $conf->echo_img($conf->hPLOT['IMG']);
    $elapsed1 = round($t1 - $start,3);
    $elapsed2 = round($t2 - $t1,3);
    $total    = round(time() - $start,3);
    #echo "</br>T1 = $elapsed1 - T2 = $elapsed2 - Total = $total<br/>";
}


?>
