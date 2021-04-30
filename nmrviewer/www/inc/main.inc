<?php
/*
 * main.inc
 * (C) INRA - DJ - 2012
 * Design of the web interface of NMRProcFlow.
 
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
?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en" style="overflow-y: hidden;">
<head>
  <meta charset="utf-8" />
  <TITLE>NMR Spectra viewer</TITLE>
  <link rel="stylesheet" href="<?php echo $urlroot; ?>/css/redmond/jquery-ui.min.css">
  <link rel="stylesheet" href="<?php echo $urlroot; ?>/css/style.css">
  <script language=JavaScript src="<?php echo $urlroot; ?>/js/jquery-1.9.1.min.js" type="text/javascript"></script>
  <script language=JavaScript src="<?php echo $urlroot; ?>/js/jquery-ui-1.10.1.min.js" type="text/javascript"></script>
  <script language=JavaScript src="<?php echo $urlroot; ?>/js/spectrum.js" type="text/javascript"></script>
  <style>
     body { font-family: "Trebuchet MS", "Helvetica", "Arial",  "Verdana", "sans-serif"; font-size: 62.5%; }
     .ui-noselect {-webkit-touch-callout: none; -webkit-user-select: none; -khtml-user-select: none; -moz-user-select: none; -ms-user-select: none; user-select: none; }
     .ui-nopad { vertical-align:top; margin:0px 0px 0px 0px; padding=0px; }
     .ui-button-text-only .ui-button-text { padding-right: 0.4em; padding-left: 0.2em; }
     .ui-widget select, .ui-widget button { min-height: 20px; }
}
  </style>
  <script language=JavaScript type="text/javascript">
  $(function() {

<?php if (!$conf->error) { echo<<<EOT
  \$( "bZout" ).button()
                  .click( function( event ) { Spectrum.AJ_XMS(); })
               .attr('title', 'Return to the full width of ppm');
  \$( "bZprev" ).button()
                  .click( function( event ) { Spectrum.AJ_XMS('prev=1'); })
                  .attr('title', 'Return to the previous selected zone');
  \$( "bBefore" ).button({
                  icons: {  primary: "ui-icon-circle-arrow-w" },
                  text: false
                }).click( function( event ) { shift_left(); })
                  .attr('title', 'Shift to the left');
  \$( "bAfter" ).button({
                  icons: {  primary: " ui-icon-circle-arrow-e" },
                  text: false
                }).click( function( event ) { shift_right(); })
                  .attr('title', 'Shift to the right');
  \$( "#color" ).attr('title', 'Select the criterion in order to discriminate the groups of spectra by applying differents colors.');
  \$( "#level" ).attr('title', 'Select an item');
  \$( "#check" ).button({ icons: { primary: "ui-icon-flag" },text: false })
               .click( function( event ) { spectrum_view(); });
  \$( "#ordplot" ).button({ icons: { primary: "ui-icon-arrowthick-2-n-s" },text: false })
               .click( function( event ) { spectrum_view(); });
  \$( "#stack" ).button().click( function( event ) { spectrum_view(); });
  Spectrum.callback= function () {
     if (Spectrum.first) {
           slider_view();
     }
  }
  Spectrum.AJ_XMS( );

EOT;
}?>

  });
  </script>
</head>
<body>
<div ID="SPECVIEW" class="ui-noselect" style="overflow:visible;">
<?php
if (!$conf->error) {
  $arr_select = array ();
  $arr_select[]='<span id="main_select">'.DS_select_or_input('color',$color_id,$colortype,1,"spectrum_level()",1,"Color by").'</span>';
  $arr_select[]='<span id="level_select">'.DS_select_or_input('level',$level_id,$arr_level,1,"spectrum_view()",1,"all levels").'</span>';
  $arr_select[]='<span id="sub_level_select"></span>';
}
?>
<table width=100% style="padding: 0px 0px 0px 0px;">
<tr><td><img src="<?php echo $urlroot; ?>/images/img_00.gif" width=1 height=256></td>
<td><div id="MAINSPEC" class="ui-nopad">
<table class="ui-nopad" style="padding: 0px 0px 0px 0px;">
<tr class="ui-nopad"><td class="ui-nopad" style="text-align:left;">
<div id="DSPEC" class="ui-nopad" style="width:100%;">
  <img id="spec" border=0 src="<?php echo $urlroot; ?>/images/img_00.gif" width=1044 height=10><br/>
  <?php if ($conf->error) echo DS_PanelMsg($conf->ERRORMSG); ?>
</div>
</td><td class="ui-nopad" style="text-align:left; width=20px;">
<img src="<?php echo $urlroot; ?>/images/img_00.gif" width=0 height=12>
<div id="slider-vertical" style="height: <?php echo $conf->IMGHEIGHT-40; ?>px;"></div>
</td></tr></table>
<div id="verticale" style="position:absolute; display:none; top:0px; left=0px; height:232px; width:0px; font-size:1px; border-left:dotted 1px #00ff00; background-color:green; z-index:10;">
   <img src="<?php echo $urlroot; ?>/images/img_00.gif" width=0 height=256>
</div>
</div></td></tr>
<tr><td></td>
<td>
<table width="100%"><tr><td>
<table align="left" style="min-width: 700px;"><tr>
<td width="20%"><bZout>Zoom out</bZout><bZprev>Previous</bZprev></td>
<td align="center"><div id="info" class="ui-widget"><?php if (!$conf->error) echo DS_html_elems($arr_select); ?></div></td>
<td><input type="checkbox" id="check"><label for="check">Force the display of the legend</label></td>
<td><input type="checkbox" name="ordplot" id="ordplot"><label for="ordplot">Reverse the plot order</label></td>
<td width="15%"><input type="checkbox" name="stack" id="stack"><label for="stack">Stacked spectra</label></td>
<td style="width:200px;" align="center"><div id="right" class="ui-widget">-</div></td>
</tr></table>
</td><td align="right"><bBefore>&#60;-</bBefore><bAfter>-&#62;</bAfter></td><td style="width:20px;">&nbsp;</td></tr></table>
</td></tr>
<tr><td></td><td><div id="annotation"></div></td></tr>
</table>
<div id="waitdiv" style="display: none">
<table border=0 align=center><tr><td><img src="<?php echo $urlroot; ?>/images/loading.gif"></td><td width=20></td><td><font size=+1> please wait .... </font></td></tr></table>
</div>
<div id="selppm" name="selppm" style="display:none;"></div>
<script language=JavaScript type="text/javascript">
   var Spectrum = new ObjImage();
       Spectrum.itype = 'MD'; // Medium Definition
       Spectrum.AJ_PROG ='<?php echo $VIEW_AJAX; ?>';
       Spectrum.params = "&color=<?php echo $color_id; ?>";
       Spectrum.params = Spectrum.params + "&level=<?php echo $level_id; ?>";
       Spectrum.params = Spectrum.params + "&IMGHEIGHT=<?php echo $IMGHEIGHT; ?>";
       Spectrum.params = Spectrum.params + "&IMGSIZE="+( $( document ).width() - 60 );

   var h = $(window).height(), w = $(window).width();
   var self=Spectrum;
   $(window).resize(function() {
       if(this.resizeTO) clearTimeout(this.resizeTO);
       this.resizeTO = setTimeout(function() { $(this).trigger('resizeEnd');}, 500);
   });
   var ctrlKey = 17;
   var altKey  = 18;
   $(window).keydown( function (e) { if ((e.keyCode==ctrlKey || e.keyCode==altKey) && typeof window.parent.CaptureMode != 'undefined') { 
            window.parent.CaptureMode=1; window.parent.toggle_capturemode();
   }});
   $(window).keyup( function (e)  { if ((e.keyCode==ctrlKey || e.keyCode==altKey) && typeof window.parent.CaptureMode != 'undefined') { 
            window.parent.CaptureMode=0; window.parent.toggle_capturemode();
            Spectrum.reactiveMouveOver();
   }});

   $(window).bind('resizeEnd', function() {});
   // Window Resize Event
   $(window).bind('resizeEnd', function() {
        var nh = $(window).height(), nw = $(window).width();
        if (w != nw) {
           spectrum_view('IMGSIZE='+( $( window ).width() - 60 ))
        }
        h = nh; w = nw;
   });
   var set_spectrum_imgheight = function(imgheight) {
       Spectrum.iheight=imgheight;
   }
   var set_spectrum_imgtype = function(imgtype) {
       Spectrum.itype=imgtype;
   }
 
// -- Begin: addOn for capturing a ppm range
   var set_spectrum_keycode = function(keycode) {
        Spectrum.keycode = keycode;
   }
   var get_spectrum_range = function() {
        ppmrange = $('#selppm').text();
        if (ppmrange.length>0) ppmrange = ppmrange + "\n";
        $('#selppm').html('')
        Spectrum.hide_waitdiv();
        return ppmrange;
   }
   var reset_spectrum_range = function() {
        Spectrum.keycode = keycode;
        $('#selppm').html('')
        Spectrum.mDwn=0;
        Spectrum.hide_waitdiv();
   }

// -- End: addOn for capturing a ppm range

   var spectrum_select = function(id) {
       $.ajax({
           url:'<?php echo $SELECT_AJAX; ?>'+'&main_select='+id,
           async:false,
           global:true
       }).done(function ( response ) {
           document.getElementById('main_select').innerHTML = response;
           eval($(response).find("script").text());
       });
       spectrum_level();
   }

   var spectrum_level = function() {
       $.ajax({
           url:'<?php echo $SELECT_AJAX; ?>'+'&main_select=0&color=' + $('#color option:selected').val(),
           async:false,
           global:true
       }).done(function ( response ) {
           document.getElementById('level_select').innerHTML = response;
           document.getElementById('sub_level_select').innerHTML = '';
           eval($(response).find("script").text());
       });
       spectrum_view();
   }

   var spectrum_sub_level = function() {
       $.ajax({
           url:'<?php echo $SELECT_AJAX; ?>'+'&main_select=0&color=' + $('#color option:selected').val() + '&level_id=' + $('#level option:selected').val(),
           async:false,
           global:true
       }).done(function ( response ) {
           if ( $('#level option:selected').val()>0 ) {
               document.getElementById('sub_level_select').innerHTML = response;
           } else {
               document.getElementById('sub_level_select').innerHTML = '';
           }
           eval($(response).find("script").text());
       });
       spectrum_view();
   }

   var spectrum_view = function (params) {
       Spectrum.params = '&color=' + $('#color option:selected').val();
       Spectrum.params = Spectrum.params + '&level=' + $('#level option:selected').val();
       Spectrum.params = Spectrum.params + '&sublevel=' + $('#sub_level option:selected').text();
       Spectrum.params = Spectrum.params + '&IMGSIZE=' + Spectrum.iwidth;
       Spectrum.params = Spectrum.params + '&IMGHEIGHT=' + Spectrum.iheight;
       Spectrum.params = Spectrum.params + '&IMGTYPE=' + Spectrum.itype;
       Spectrum.params = Spectrum.params + '&SMAX=' + $( "#slider-vertical" ).slider( "value" );
       if (params) Spectrum.params = Spectrum.params + '&' + params;
       if ( $('#check').is(':checked') ) Spectrum.params = Spectrum.params + '&legend=1';
       if ( $('#ordplot').is(':checked') ) Spectrum.params = Spectrum.params + '&ordplot=1';
       if ( $('#stack').is(':checked') ) {
             Spectrum.params = Spectrum.params + '&stacked=1';
             $('#stack').button( 'option', 'label', 'Overlaid spectra');
       } else {
             $('#stack').button( 'option', 'label', 'Stacked spectra');
       }
       Spectrum.callback = function () { $('#slider-vertical').css('height',( Spectrum.iheight-40 )+'px' ); }
       Spectrum.view();
   }

   var spectrum_reset = function () {
       Spectrum.AJ_XMS();
   }

   var slider_view = function () {
       $( "#slider-vertical" ).slider({
           orientation: "vertical",
           range: "min",
           min: 1,
           max: 100,
           value: 100,
           start: function( event, ui ) { Spectrum.fslider=1; },
           stop: function( event, ui ) { Spectrum.fslider=0; spectrum_view(); }
        }).attr('title', 'Zoom on low intensities.');
   }

   var shift_left = function () {
        if (Spectrum.Xmax < Spectrum.SpecValMax) {
           val2=Spectrum.Xmax + 0.5*(Spectrum.Xmax-Spectrum.Xmin)
           if (val2 >= Spectrum.SpecValMax) {
               val2=Spectrum.SpecValMax
           }
           val1=val2 - (Spectrum.Xmax-Spectrum.Xmin)
           Spectrum.AJ_XMS('val1='+val1+'&val2='+val2)
        }
   }

   var shift_right = function () {
        if (Spectrum.Xmin > Spectrum.SpecValMin) {
           val1=Spectrum.Xmin - 0.5*(Spectrum.Xmax-Spectrum.Xmin)
           if (val1 <= Spectrum.SpecValMin) {
               val1=Spectrum.SpecValMin
           }
           val2=val1 + (Spectrum.Xmax-Spectrum.Xmin)
           Spectrum.AJ_XMS('val1='+val1+'&val2='+val2)
        }
   }
</script>
</div>
</body>
</html>

