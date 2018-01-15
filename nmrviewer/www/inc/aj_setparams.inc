<?php
/*
# Nom du fichier: aj_setparams.inc
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

foreach ($_GET as $var => $get_value) { 
    $conf->save_within_session($var,$get_value,!is_int($get_value));
}


?>
