<?php
/*
# Nom du fichier: aj_select.inc
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

include ('nmrproc_ui.inc');

$main_select= $_GET['main_select'] > 0 ? $_GET['main_select'] : 0;
$color= $_GET['color'] > 0 ? $_GET['color'] : 1;
$level_id= $_GET['level_id'] > 0 ? $_GET['level_id'] : 0;


do {

   if ($main_select>0) {
      $factors_file="$datadir/factors";
      $colortype = array();
      if (($handle = fopen($factors_file, "r")) !== FALSE) {
         while (($line = fgetcsv($handle, 1000, ";")) !== FALSE)
            $colortype[] = $line ; 
         fclose($handle);
      }
      echo DS_select_or_input('color',$main_select,$colortype,1,"spectrum_level()",1,"Color by");
      break;
   }

   $arr_level = array();
   $c=$color+1;
   
   if ($level_id==0) {
       exec("cat $datadir/samples.csv | awk -F';' '{ print \$$c }' | sort | uniq 2>/dev/null",$arr_output,$retval);
   } else {
       $level_name = trim(`cat $datadir/samples.csv | awk -F';' '{ print \$$c }' | sort | uniq | head -$level_id | tail -1 2>/dev/null`);
       exec("cat $datadir/samples.csv | awk -F';' '{ print \$2,\$$c }' | grep '$level_name' |  awk -F' ' '{ print \$1 }' 2>/dev/null",$arr_output,$retval);
   }
   
   if ($retval == 0) {
     $n=1;
     foreach ($arr_output as $value) {
          $arr_level[] = array($n, $value);
          $n++;
     }
   }

   if ($color<2 && $level_id>0) {
      echo '';
      break;
   }

   if ($level_id==0) {
      echo DS_select_or_input('level',0,$arr_level,1,"spectrum_sub_level()",1,"all levels");
      break;
   }

   if ($level_id>0) {
      echo DS_select_or_input('sub_level',0,$arr_level,1,"spectrum_view()",1,"all levels");
      break;
   }

   echo '';
   break;

} while(0);



?>