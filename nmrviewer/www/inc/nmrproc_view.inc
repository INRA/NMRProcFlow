<?php
/* ID nmrproc_view.inc v1.0
 * Copyright (C) 2013 INRA - DJ
 *
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

include('nmrproc_conf.inc');

class nmrproc_view extends nmrproc_conf {

    var $color    = 1;      # group id (1 color per group) : 1 for samplecode, 2 for factor1, 3 for factor2, ...
    var $level    = 0;      # level in the group id : 0 for all
    var $sublevel = '';     # samplecode in the group id : 0 for all
    var $stacked  = 0;      # plot spectra as Stacked spectra (1) or Unstacked (0)
    var $legend   = 0;      # spectra legend: boolean true for display
    var $sepfield = '';     # Delimiter within the nomenclature (samplecodes of factor groups)
    var $fieldval = '';     # List of fields within the nomenclature (positions delimited by comma)  on which will be based the spectra grouping.
    var $spectype = 0;      # Type of information (not used yet)
    var $val1     = 0;      # Start value of the selected range (ppm unit)
    var $val2     = 0;      # End value of the selected range (ppm unit)
    var $prev     = 0;      # Boolean value indicating whether the previous selection should be displayed again.
    var $FLGALL   = 0;      # Boolean value indicating whether the full spectra should be displayed.
    var $IMGSIZE  = 1024;   # Width of the image (pixels)
    var $IMGHEIGHT= 384;    # Height of the image (pixels)
    var $SMAX     = 100;    # the slider value (range 0-100)
    var $slider_factor = 4; # 1 => 10^(-slider_factor) ; 100 => 1
    var $gnuplot_leftbox_offset = 70; # 60 with gunplot 4.6 patchlevel 4

    # An hash table for the GNUPLOT command
    var $hPLOT    = array ( 'PLOT' => '',      # Instructions for the plot command of gnuplot (main data)
                            'PLOT_SUP' => '',  # Additionnal Instructions (information such as bucket areas, average spectrum, ...)
                            'IMG' => '',       # PNG file name
                            'SPEC' => 0,       # number of spectra
                            'YMIN' => 0,       # Min spectra intensity value within the current ppm range
                            'YMAX' => 0,       # Max spectra intensity value within the current ppm range
                            'IMAX' => 0        # Max spectra intensity value within the whole ppm range
                    );
    var $gnuplot_colors   = array ( "red", "web-green", "web-blue", "dark-magenta", "orange", "magenta", "dark-cyan", 
                            "turquoise", "purple", "dark-green", "light-red", "medium-blue", "sea-green", "orange-red", 
                            "light-coral", "salmon", "olive", "steelblue", "orchid", "dark-pink", "spring-green" );

/********************************************************
* Init. Method / Constructor
********************************************************/
    function __construct($input) { $this->nmrproc_view($input); } # PHP 5.3-compliant

/********************************************************
* nmrproc__view
********************************************************/
    function nmrproc_view($input) {

        # Get parameters from Configuration file (etc/nmrview.conf)
        parent::__construct($input);

    }

/********************************************************
* nmrproc_next_config
********************************************************/
    # Next Configutation (with or without restored session)
    function nmrproc_next_conf() {
        parent::nmrproc_next_conf();
        $this->__getvars();
        $this->TMPurl=$this->urlroot.'/tmp/'.$this->sid;
        $this->TMP=$this->tmproot.'/'.$this->sid;
        $this->DATADIR= $this->get_ProcDir();
        $handle = fopen($this->DATADIR."/nuc.txt","r");
        if ($handle && ! feof($handle)) {
           if ( trim(fgets($handle))=='13C' ) {
              $this->PPM_MIN = $this->PPM_MIN_13C;
              $this->PPM_MAX = $this->PPM_MAX_13C;
           }
           fclose($handle);
        }
        $this->PPM1 = $this->val1 ? $this->val1 : $this->PPM_MIN;
        $this->PPM2 = $this->val2 ? $this->val2 : $this->PPM_MAX;
        $this->MEXTRACT = $this->UNPACK_BIN;
        #$this->FLGALL = (! $this->val1 && ! $this->val2 ) ? 1 : 0 ;
        $this->FLGALL = 0 ;
        $this->FULL_SPEC = "SPEC_".$this->color.'_'.$this->level.'_'.$this->IMGSIZE.'_'.$this->IMGHEIGHT.'_'.$this->stacked.'_NMR.png';
        # Fix the GNUPLOT left box offset based on GNUPLOT patchlevel
        $cmd = $this->GNUPLOT.' -V | cut -d" " -f4 | tr -d "\n"';
        $this->gnuplot_leftbox_offset = trim(`$cmd`)==4 ? 60 : 70;
    }


/********************************************************
* echo_img() Echo Spectrum Image along with its javascript initialization
********************************************************/
    function echo_img($spec_img) {
        # init internal variables
        list( $VMIN, $VMAX, $PPM1, $PPM2, $TMP, $TMPurl, $img_width, $img_height ) = array (
                  $this->PPM_MIN, $this->PPM_MAX, $this->PPM1, $this->PPM2, $this->TMP, $this->TMPurl, $this->IMGSIZE, $this->IMGHEIGHT );
        $leftbox_offset = $this->gnuplot_leftbox_offset;
        # Update history
        $cmd  = "grep -v \"^$PPM1;$PPM2;\" $TMP/history > $TMP/history2; cat $TMP/history2 > $TMP/history";
        $status = $this->System_cmd($cmd);
        $cmd = "echo \"$PPM1;$PPM2;$spec_img\" >> $TMP/history";
        $status = $this->System_cmd($cmd);

        # New Image along with its javascript initialization
        $width_px = $img_width.'px';
        $height_px = $img_height.'px';
        echo<<<EOT
<div id='myimg'><!-- $spec_img - $PPM1,$PPM2  -->
<img id="spec" src="$TMPurl/$spec_img" border="0" width="$width_px" height="$height_px">
<script type="text/javascript" defer="defer">
Spectrum.init($img_width,$img_height,$leftbox_offset,$img_width-5,14,$img_height-26,$PPM1,$PPM2,$VMIN,$VMAX,1);
</script>
</div>
EOT;
    }

/********************************************************
* echo_previous_img() Echo Previous selection
********************************************************/
    function echo_previous_img() {
        list( $TMP, $color, $img_width ) = array( $this->TMP, $this->color, $this->IMGSIZE );
        $status=0;
        do {
           if ( $this->prev==0 || ! is_readable("$TMP/history")) break;
           $line = trim(`tail -2 $TMP/history | head -1`);
           echo "<!-- $line -->";
           $matches=preg_split("/;/",$line,-1, PREG_SPLIT_NO_EMPTY);
           if (count($matches)==0) {
              $status = $this->System_cmd("rm -f $TMP/history");
              #$this->FLGALL=1;
              break;
           }
           `head -\$(expr \$(cat $TMP/history | wc -l) - 1) $TMP/history > $TMP/history2; cat $TMP/history2 > $TMP/history`;
           $this->PPM1=trim($matches[0]);
           $this->PPM2=trim($matches[1]);
           $this->FLGALL=0;
        } while (0);
        return $status;
    }

/********************************************************
* get_list_spectra() : Retrieve the list of spectrum, ordered according to the color parameter
********************************************************/

    function get_list_spectra() {

        function __cmp_speckey($a,$b) { return strcmp($a[0], $b[0]); }

        list( $datadir, $sepfield, $fieldval ) = array( $this->DATADIR, $this->sepfield, $this->fieldval );
        $filename="$datadir/samples.csv";

        # Get normalization factor for each samples
        $Tnormfac = array();
        $fcalib="$datadir/normsamples.csv";
        if (($handle = fopen($fcalib, "r")) !== FALSE) {
           while (($line = fgetcsv($handle, 1000, ";")) !== FALSE) {
                $Tnormfac[$line[0]] = $line[1];
           }
           fclose($handle);
        }

        # Check if the chosen 'level' exists, then put into $level_name
        $level_name="";
        if ($this->level>0) {
           $c=$this->color+1;
           exec("cat $filename | awk -F';' '{ print \$$c }' | sort | uniq 2>/dev/null",$arr_output,$retval);
           if ($retval == 0) {
             $n=1;
             foreach ($arr_output as $value) {
                 if($n == $this->level) { $level_name=$value; break; }
                 $n++;
             }
           }
        }

        # Get the list of samples with some attributes initialized depending on the chosen 'level'
        $list_idx=array();
        if (($handle = fopen($filename, "r")) !== FALSE) {
            $line_id=1;
            while (($line = fgetcsv($handle, 1000, ";")) !== FALSE) {
                 if (strlen($this->sublevel)>0 && $this->sublevel != "-- all levels ---") {
                    $select = 0;
                    $PREM = $line[1];
                    if ($this->sublevel == $PREM) $select=1;
                 } else {
                    $PREM = $line[$this->color];
                    # Extract the fields (fieldval) based on separator (sepfield)
                    if (strlen($sepfield) && strlen($fieldval)) $PREM=trim(`echo "$PREM" | cut -d'$sepfield' -f$fieldval`);
                    $select = strlen($level_name) ? 0 : 1;
                    if (strlen($level_name) && $PREM==$level_name) $select = 1;
                 }
                 $normfac = array_key_exists($line[1], $Tnormfac) ? $Tnormfac[$line[1]] : 1.0 ;
                 $list_idx[]=array( $PREM, $line[1], $line_id, $select, $normfac );
                 $line_id++;
            }
            fclose($handle);
            usort($list_idx,"__cmp_speckey");
        }
        return $list_idx;
    }

/********************************************************
* write_list_spectra() : write the list of spectrum in the 'listspec' file
********************************************************/

    function write_list_spectra($list) {

        list( $SELECTED, $color, $level, $sublevel ) = array( $this->DATADIR.'/'.$this->SPEC_SELECTED, 
                                                              $this->color, $this->level, $this->sublevel );
        $LISTID="$color;$level;$sublevel\n";
        if(is_writable($this->DATADIR)) {
            `echo "$LISTID" | head -1 > $SELECTED`;
        }
    }

/********************************************************
* get_PLOT_line($list) : Build the PLOT statement for GNUPLOT command, based on:
                       - selected ppm range
                       - the list of spectra and their groups (color)
  Input : the list of spectrum, ordered according to the color parameter
          provided by the get_list_spectra() method
********************************************************/
    function get_PLOT_line($list) {

        list( $TMP, $PACKED ) = array( $this->TMP, $this->DATADIR.'/'.$this->SPEC_PACKED );

        $VARCOL=1;
        $PREVAR = "xx";
        $PREVSPL='xx';
        $PLOT='';
        $SPEC=0;
        $record='';
        $fac_items=0;
        $offset_val=0.0;
        $int_fac=1.0;
        $RES=$this->IMGHEIGHT-40;
        $cnt=0;

        # Preparation if spectra

        if (count($list)>0) {

            # count the number of items within the chosen factor
            $arr_items=array();
            foreach ($list as $idx => $arr) { $arr_items[] = $arr[0]; }
            $fac_items=count(array_unique($arr_items));

            # Extract each part of spectra into separate binary files ($TMP/subspec*.bin);
            $CMD=$this->MEXTRACT ." $PACKED $TMP/subspec" . ' -min '.$this->PPM1.' -max '.$this->PPM2;
            `echo "$CMD" >>$TMP/debug`;
            list( $record_line, $min_line, $max_line ) = $this->Exec_cmd($CMD,"$TMP/bin4gp.log");
            $record = trim(`echo '$record_line' | cut -d'=' -f2  | cut -d 'x' -f1`);
            $record .= 'x1';
            $this->hPLOT['YMIN'] = trim(`echo '$min_line' | cut -d'=' -f2`);
            $this->hPLOT['YMAX'] = round(trim(`echo '$max_line' | cut -d'=' -f2`),2);
            if ($this->hPLOT['YMAX']>$this->hPLOT['IMAX']) {
                $this->hPLOT['IMAX']=$this->hPLOT['YMAX'];
                $this->save_within_session('IMAX',$this->hPLOT['IMAX'],1);
            }
            if ($this->stacked>0) {
               list( $MIN, $MAX, $IMAX, $SMAX) = array( $this->hPLOT['YMIN'], $this->hPLOT['YMAX'], $this->hPLOT['IMAX'], $this->SMAX );
               $ymin=$MIN; $ymax=$MAX;
               $UMAX = pow(10, $this->slider_factor*($SMAX-100)/99)*$IMAX;
               if ($SMAX>0 && $UMAX<$ymax) { $ymax=$UMAX; $ymin = $MIN*abs($UMAX/$MAX); }

               # Zoom in Y scale
               foreach ($list as $idx => $arr) { if ($arr[3]>0) $cnt++; }
               $alpha=0.4;
               $offset_val = (($alpha/(1-$alpha))*($ymax - $ymin) + $ymin)/$cnt;
               $this->hPLOT['YMAX'] += $offset_val*$cnt;
               if ($cnt>1) { $int_fac=0.25; } else { $int_fac=0.8; }
               $this->hPLOT['YMAX'] *= 0.5;
            }
        }

        # Generate the corresponding gnuplot 'plot' statement
        foreach ($list as $idx => $arr) {
            $PREM = $arr[0];
            $sample = $arr[1];
            $COL=$arr[2];
            $SEL=$arr[3];
            $FAC=$arr[4];
            $TITLE='';
            $PLOTFILE="$TMP/subspec$COL.bin";
            if ("$PREM" != "$PREVAR" && $this->level==0) {
                if ($fac_items<=10 || $this->legend==1) $TITLE="$PREM";
                $VARCOL++;
            }
            if ( "$sample" != "$PREVSPL" && $this->level>0 && $this->legend==1) $TITLE="$sample";
            $PREVAR=$PREM;
            $PREVSPL=$sample;
            if ($SEL==0) continue;
            if ($this->level>0) $VARCOL++;
            if (strlen($PLOT)) $PLOT .= ', ';
            $COLOR=$this->gnuplot_colors[($VARCOL-2)%count($this->gnuplot_colors)];
            $PLOT .="\"$PLOTFILE\" binary record=$record using 1:(\$2*$int_fac*$FAC + $offset_val*$SPEC) title \"$TITLE\" with lines lt rgb \"$COLOR\"";
            $SPEC++;
        }

        $this->hPLOT['SPEC']=$SPEC;
        $this->hPLOT['PLOT']=$PLOT;
    }

/********************************************************
* get_GPCMD_rects : Build the GNUPLOT instructions for ppm zones defined in 'file_list' file
  Input :
       - selected ppm range
       - the files of the zone list
       - fcrgb1 : fc rgb color
       - fcrgb2 : fc rgb color in case of 2 alternate colors (one out of 2 is fcrgb2, otherwise fcrgb1); put an empty string if not the case. 
       - offset : If there are several type of 'obj rect' to put on the graph; each 'obj rect' need an different identifier. 
********************************************************/
    function get_GPCMD_rects($file_list, $fcrgb1, $fcrgb2, $offset) {

        list( $TMP, $ZONES_LIST, $PPM1, $PPM2 ) = array( $this->TMP, $this->DATADIR.'/'.$file_list, $this->PPM1, $this->PPM2 );
        do {
           if ( ! is_readable($ZONES_LIST)) break;

           $cnt=0;
           $gpcmd_str = '';
           $toggle = 0;
           if (($handle = fopen($ZONES_LIST, "r")) == FALSE) break;
           while (($vals = fgetcsv($handle, 0, "\t")) !== FALSE) {
              if ($vals[0]>$PPM2 || $vals[0]<$PPM1) continue;
              $cnt++;
              $fcrgb  = ($toggle==1 && strlen($fcrgb2)>0) ? $fcrgb2 : $fcrgb1 ;
              $toggle = $toggle==0 ? 1 : 0 ;
              $fcrgb = '@'.$fcrgb.'@';
              $ppm_val1 = $vals[0]-$vals[1]/2; $ppm_val2 = $vals[0]+$vals[1]/2; 
              $line = sprintf("set obj %d rect from %7.3f, graph 0 to %7.3f, graph 1 fs solid 0.15 noborder fc rgb %s ; ",
                                     $cnt+$offset, $ppm_val1, $ppm_val2, $fcrgb);
              $line = str_replace("@", "\"", $line );
              $gpcmd_str .= $line ;
           }
           fclose($handle);

           $this->hPLOT['PLOT_SUP'] .= $gpcmd_str;

        } while (0);
    }


/********************************************************
* get_Buckets : Build the GNUPLOT instructions for bucket zones
  Input :
       - selected ppm range
       - the list of buckets
********************************************************/
    function get_Buckets() {
        $this->get_GPCMD_rects($this->BUCKET_LIST, "#778899","#B0C4DE", 0);
        $this->get_GPCMD_rects("zones1_list.in", "blue", "", 10000);
        $this->get_GPCMD_rects("zones2_list.in", "magenta", "", 20000);
        $this->get_GPCMD_rects("zones3_list.in", "magenta", "", 30000);
        $this->get_GPCMD_rects("zones4_list.in", "#6495ED", "", 40000);
        $this->get_GPCMD_rects("zones5_list.in", "blue", "", 50000);
        $this->get_GPCMD_rects("vsb_list.in", "blue", "", 60000);
    }

/********************************************************
* plot_SPECTRA() : Build the final GNUPLOT command, based on:
                       - the PLOT line
                       - the temporary files SPEC.lst.*
                   Launch the GNUPLOT command
********************************************************/
    function generate_Spectra_image() {

        list($TMP,$SPEC) = array ( $this->TMP, $this->hPLOT['SPEC'] );
        if ($SPEC == 0) return 1;

        #$spec_img  = $this->color.'_'.$this->IMGSIZE.'_'.$this->genKEY().'.svg'; ## SVG 
        $spec_img  = $this->color.'_'.$this->IMGSIZE.'_'.$this->genKEY().'.png';
        $spec_log  = "$TMP/gnuplot.log";
        $spec_cmd  = "$TMP/gnuplot.cmd";

        if ($this->FLGALL) $spec_img=$this->FULL_SPEC;
        $this->hPLOT['IMG'] = $spec_img;

        list( $PPM1, $PPM2, $GNUPLOT, $img_width, $img_height, $PLOT, $PLOT_SUP, $MIN, $MAX, $IMAX, $SMAX ) = array(
              $this->PPM1, $this->PPM2, $this->GNUPLOT, $this->IMGSIZE, $this->IMGHEIGHT,
              $this->hPLOT['PLOT'], $this->hPLOT['PLOT_SUP'],
              $this->hPLOT['YMIN'], $this->hPLOT['YMAX'], $this->hPLOT['IMAX'], $this->SMAX );

        # Zoom in Y scale
        $ymin = $this->stacked==0 ? $MIN : 0 ;
        $ymax = $MAX;
        $UMAX = pow(10, $this->slider_factor*($SMAX-100)/99)*$IMAX;
        if ($this->SMAX>0 && $UMAX<$MAX) $ymax=$UMAX;

        #set terminal svg  enhanced mouse font "Arial,8" size $img_width,$img_height; ## SVG 
        $gnuplot = <<<EOT
        set terminal pngcairo  font "Arial,8" size $img_width,$img_height;
        set origin 0.0, 0.0;
        set output "$TMP/$spec_img";
        set lmargin 10;
        set bmargin 2;
        set tmargin 1;
        set rmargin 1;
        unset autoscale x;
        set xrange [$PPM2:$PPM1];
        unset autoscale y;
        set yrange [$ymin:$ymax];
        set grid;
        $PLOT_SUP
        plot $PLOT;
EOT;

        if (($handle = fopen($spec_cmd, "w")) == TRUE) {
            $fwrite = fwrite($handle, $gnuplot);
            fclose($handle);
            $status = $this->System_cmd("$GNUPLOT $spec_cmd",$spec_log);
            if ( ! file_exists("$TMP/$spec_img") || $status != 0 ) {
                ## echo '<PRE>'.`cat $spec_log`.'</PRE>';
                echo '<PRE>loading ...</PRE>';
                return 2;
            }
            `echo "$MIN, $MAX - $ymin, $ymax" >> $spec_log`; # for debugging
        }
    }

/********************************************************
* __getvars() : populate this object with the  $_GET variables
********************************************************/
    function __getvars() {
        foreach ($_GET as $var => $get_value) { $this->{$var} = $get_value; }
    }
}

?>
