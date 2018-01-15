<?php
/*
 * nmrproc_ui.inc
 * (C) INRA - DJ - 2012
 * Design of the web interface of NMRProcFlow
 
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

#---------------------------------------------------------------------------#
# Fonction permettant d'afficher un texte dans la <div> centrale de la page #
# $msg : chaîne de caractère contenant le message à afficher                #
#---------------------------------------------------------------------------#
function DS_PanelMsg ($msg)
{
   $html = <<<EOT
    <table class="ui-widget ui-widget-content" cellspacing=0 width=100%>
     <tr>
      <td width="100%" colspan="2" style="padding-left: 10; padding-right: 10">
       <p>&nbsp;</p>
       <p style="margin-top: 10">
       <center><font size="+1">$msg</font></center>
       <p>&nbsp;</p>
      </td>
     </tr>
    </table>
EOT;
   return $html;
}

#-------------------------------------------------------------------------------------------------------#
# Fonction permettant d'afficher sur une meme ligne, diffents elements (code HTML) contenu dans $array  #
# $array : tableau regroupant les différents elements (code HTML)                                       #
#-------------------------------------------------------------------------------------------------------#
function DS_html_elems($array_html,$align='',$array_col=array(),$array_row=array())
{
    $html = <<<EOT
<table><tr align="left">
EOT;
   foreach ($array_html as $key=>$value) {
         $colspan=''; $rowspan=''; $td_align='align=\"left\"';
         if (isset($align))
             if (is_array($align) && $align[$key])
                $td_align="align=\"$align[$key]\"";
             else
                $td_align="align=\"$align\"";
         if (isset($array_col) && $array_col[$key])
             $colspan="colspan=$array_col[$key]";
         if (isset($array_row) && $array_row[$key])
             $rowspan="rowspan=$array_row[$key]";
         $html .= "<td $td_align $colspan $rowspan>$value</td>";
   }
   $html .= "</tr></table>\n";
   return $html;
}

#------------------------------------------------------------------------------------------------#
# Fonction permettant d'afficher un formulaire de sélection ou pas selon les paramètres transmis #
# $field : nom de la variable à rechercher                                                       #
# $field_value : valeur de la variable à sélectionner (si elle existe déjà)                      #
# $tab : tableau regroupant les valeurs à sélectionner                                           #
# $access :  1: accès autorisé; 0: accès refusé                                                  #
# $script : lien URL sur evenement OnChange, dans la cas d'une selection                         #
# $flag  : Type du script : 0 =  URL location, 1 = javascript OnChange event                     #
#------------------------------------------------------------------------------------------------#
function DS_select_or_input ($field,$field_value,$tab,$access,$script='',$flag=0,$label='')
{
   $field_name = $field."_name";
   $field_id = $field."_id";
   $nb_tab = count($tab);
   $sep = preg_match ('/\?/',$script) ? "&" : "?" ;
   $label = strlen($label)>0 ? "-- $label --" : ' --- ';
   $html = "";
   if ($access > 0) {
     if (strlen($script)) {
         if ($flag)
            $event = "onChange=\"javascript:$script;\"";
         else
            $event = "onChange=\"location='$script$sep$field_id='+$field_id.value\"";
     }
     $html .= "<select class=\"ui-widget ui-widget-content ui-corner-all\" ID=\"$field\" name=\"$field_id\" $event>";
     if ($access == 1) {
         $selected = ! $field_value ? 'selected' : '';
         $html .= "<option value=\"0\" $selected>$label-</option>\n";
     }
     for ($k=0;$k<$nb_tab;$k++) {
        $id = $tab[$k][0]; $name=$tab[$k][1];
        $selected = $field_value == $id ? 'selected' : '';
        $html .= "<option value=\"$id\" $selected>$name</option>\n";
     }
     $html .= "</select>";
   }

   else {
     for ($k=0;$k<$nb_tab;$k++) {
        $id = $tab[$k][0]; $name=$tab[$k][1];
        if ($field_value == $id) break;
     }
     $html .= "<input type=hidden name=\"$field_id\" value=\"$field_value\">";
     $html .= "<input class=\"ui-widget ui-widget-content\" type=text id=\"$field_name\" name=\"$field_name\" size=25 value=\"$name\" readonly>";
   }
   return $html;
}

#-------------------------------------------------------------------------------------------------------#
# Fonction permettant d'afficher la <div> de sélection des champs (<div> "Select")                      #
# $name : nom de l'entité à sélectionner                                                                #
# $arr_select : tableau regroupant le(s) formulaire(s) de sélection ainsi que le(s) boutons (ex: 'New') #
#-------------------------------------------------------------------------------------------------------#
function DS_fieldset_select ($name,$arr_select,$label='Select')
{
   $html = <<<EOT
<fieldset class="ui-widget ui-widget-content ui-corner-all"><legend><b>$label</b></legend><div><center>
<table>
<tr><form>
EOT;
   if (strlen($name))
      $html .= "<td><b>$name</b>:</td>";
   foreach ($arr_select as $value) $html .= "<td>$value</td>";
   $html .= <<<EOT
</form></tr>
</table>
</center>
</div></fieldset>
EOT;
   return $html;
}

?>
