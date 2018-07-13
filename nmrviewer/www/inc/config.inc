<?php
/* ID config.inc v1.0
 * Copyright (C) 2013 INRA - DJ

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


include('nmrproc_view.inc');

# NNMRProcFlow Sofware Version
$x_version = "1.0.0";

# HELP File (User Manual)
$helpfile = "";

# Instanciate a new configuration session
$conf = new nmrproc_view(array('-conf'=>"etc/nmrview.conf",'-global'=>TRUE, '-section'=>'NMRVIEWER'));
if ($conf->need_to_restore) {
    # A valid SID was given within URL : restore this session
    $conf = $conf->restore_session();
    # continue with configuration
    $conf->nmrproc_next_conf();
}

# Populate some variables
$urlroot = $conf->urlroot;
$tmpdir  = $conf->tmpdir;
$datadir = $conf->DATADIR;
$myurl   = $conf->myurl;

# a requested function ?
if ($conf->func) {
  # include the appropriate script for the requested function.
  $func = $conf->func;
  $prog = "inc/aj_$func.inc";
  @include $prog;
  exit;
}

# We arrive here only once to initialize the web interface (see main.inc).
# After that, all calls are made in AJAX javascript with a specified 'func'

$factors_file="$datadir/factors";
$colortype = array();
if (($handle = fopen($factors_file, "r")) !== FALSE) {
  while (($line = fgetcsv($handle, 1000, ";")) !== FALSE) {
     $colortype[] = $line ; 
  }
  fclose($handle);
}
$color_id= $_GET['color_id'] > 0 ? $_GET['color_id'] : 1;

$arr_level = array();
$c=$color_id+1;
exec("cat $datadir/samples.csv | awk -F';' '{ print \$$c }' | sort | uniq 2>/dev/null",$arr_output,$retval);
if ($retval == 0) {
  $n=1;
  foreach ($arr_output as $value) {
       $arr_level[] = array($n, $value);
       $n++;
  } 
}
$level_id= strlen($_GET['level_id']) ? $_GET['level_id'] : 0;

$IMGSIZE = strlen($_GET['IMGSIZE']) ? $_GET['IMGSIZE'] : $conf->IMGSIZE;
$IMGHEIGHT = strlen($_GET['IMGHEIGHT']) ? $_GET['IMGHEIGHT'] : $conf->IMGHEIGHT;

# Delete previous history
if (is_readable("$tmpdir/history")) `rm -f $tmpdir/history`;

# Initialize AJAX services
$SELECT_AJAX="$myurl?func=select";
$VIEW_AJAX="$myurl?func=nmrspec";

?>
