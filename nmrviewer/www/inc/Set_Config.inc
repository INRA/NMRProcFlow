<?php
/* ID Set_Config.inc.php v1.0
 * Copyright (C) 2003 CBIB - DJ
 *
    This file is part of X2DBI.

    X2DBI is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    X2DDBI is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with X2DBI.  If not, see <http://www.gnu.org/licenses/>.
*/

class Set_Config {

   /* 
    * Fichier de configuration global.
    * 2 types de section : ENV et GLOBAL
    * Section ENV : declaration des variables d'environnement
    *     (ajoutees au tableau global $_ENV)
    * 
    * Section GLOBAL : declaration des variables globales
    *     (accessible comme attribut de l'instance de l'objet $objet->VAR)
    * A cela s'ajoute un sous-type de section, les '_OTHERS' :
    *  ENV_OTHERS et GLOBAL_OTHERS identiques dans leur interpretation
    *  repectivement comme ENV et GLOBAL. Par défaut, ces sections ne sont pas
    * prises en compte, sauf en positionnant le flag -other a 1. 
    */

   /********************************************************
    * Methode d'initialisation de l'instance
    ********************************************************/
    function Set_Config($input=array('-global'=>TRUE, '-section'=>'NMRVIEWER')) {

        $this->global  = isset($input['-global'])  ? $input['-global']  : TRUE; 
        $this->section = isset($input['-section']) ? $input['-section'] : ''; 

        if (isset($input['-conf']) && $this->global)
           $this->_read_configuration($input['-conf'],'GLOBAL');
        if (isset($input['-conf']) && strlen($this->section)>0)
            $this->_read_configuration($input['-conf'], $this->section);
    }

    // Positionnement d'une variable d'environnement
    function _Set_Env($var,$valeur) {
        $_ENV[$var]=$valeur;
        putenv("$var=$valeur");
    }

    // Positionnement d'une variable globale
    function _Set_Global($var,$valeur) {
           $this->$var=$valeur;
    }

    /******************************************************** 
     * Parse le fichier de configuration (parametre d'entree)
     ********************************************************/
    function _read_configuration($file, $thesection) {

    // Ouverture fichier
        $fd = fopen($file,"r") 
             or exit("Impossible d'ouvrir le fichier de configuration $file");
    // Pour chaque ligne
        while (!feof($fd)) {
           $line = fgets($fd,1024);

    // Traite le cas des lignes superflues 
           $line = preg_replace("/#.*/","",$line);
           $line = preg_replace("/\s+$/","",$line);
           $line = preg_replace("/^\s+/","",$line);
           if (! strlen($line) ) continue;

    // Traite le cas des entetes de section 
           if ( preg_match("/^\[(.*)\]$/",$line,$entete) ) {
                if ( ! preg_match("/^(\S+)_(\S+)$/",$entete[1],$asection) )
                   preg_match("/^(.*)$/",$entete[1],$asection);
                $section = $asection[1];
                $others  = isset($asection[2]) ? TRUE : FALSE; 
                continue;
           }

    // Traite le cas des lignes d'affection selon la section
           if ( preg_match("/^(\S+)\s*=\s*(.*)$/",$line,$defvar) ) {
              $var    = $defvar[1];
              $valeur = $defvar[2];
              switch ($section):
                // Section ENV 
                case 'ENV':
                    $this->_Set_Env($var,$valeur);
                    break;
                // Section GLOBAL
                case 'GLOBAL':
                    $this->_Set_Global($var,$valeur);
                    break;
                // Section $thesection
                case $thesection:
                    $this->_Set_Global($var,$valeur);
              endswitch;
           }

        }
        fclose($fd);
    }

} // class


?>
