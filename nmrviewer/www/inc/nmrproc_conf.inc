<?php
/* ID nmrproc_conf.inc v1.0
 * Copyright (C) 2013 CBIB - DJ
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

include('Set_Config.inc');

class nmrproc_conf extends Set_Config {

   /*
       NMRProcFlow Configuration module

       # Usage Template :
       $conf = new nmrproc_conf(array('-conf'=>"etc/nmrview.conf",'-global'=>TRUE, '-section'=>'NMRVIEWER'));
       if ($conf->need_to_restore) {
           $conf = $conf->restore_session();
           $conf->nmrproc_next_conf();
       }

   */

   var $SESSION_FILE="sessparms";

   # GUI parameters (see etc/nmrview.conf)
   var $DEBUG = 0;

   # Dataset identifier
   var $datasetID='';
   var $IDREGEXP='_[0-9a-zA-Z]+';
   var $IDMINLEN=10;

   # SESSION parameters
   var $sid=0;
   var $new_session=0;
   var $need_to_restore=0;
   var $need_to_reload=0;

   # OS Architecture
   var $os_arch = 'i386';

/********************************************************
* Init. Method / Constructor
********************************************************/
    function __construct($input) { $this->nmrproc_conf($input); } # PHP 5.3-compliant

    # Init. Vars.
    function nmrproc_init() {

        # OS Architecture
        $this->os_arch = trim(`/bin/uname -i`);

        # APACHE version
        $this->APACHE2  = preg_match('/Apache.+1.3/',$_SERVER["SERVER_SOFTWARE"]) ? 0 : 1;

        # SERVER Root Pathways (directory & URL)
        # They can be forced with other values defined in the etc/nmrview.conf file
        $this->MYURI    = $this->PROXY_URL_ROOT.dirname($_SERVER['SCRIPT_NAME']);
        $this->MYDIR    = dirname($_SERVER['SCRIPT_FILENAME']);

        # SERVER Pathways (directory & URL)
        $this->mypath    = $this->MYDIR;
        $this->tmproot   = $this->mypath.'/tmp';
        $this->urlroot   = $this->MYURI;
        $this->myurl     = $this->MYURI.'/view';
        $tmproot         = $this->tmproot;
        $this->purge_TemporaryDir();
    }

/********************************************************
* nmrproc__config
********************************************************/
    function nmrproc_conf($input) {

        # Get parameters from Configuration file (etc/nmrview.conf)
        parent::Set_Config($input);

        # Init. Vars.
        $this->nmrproc_init();

        # Error Management
        $this->ERRORMSG='';
        $this->error=0;

        # Is Need to Restore ?
        if ($this->is_need_to_restore())
            $this->need_to_restore=1;
        else
            $this->nmrproc_next_conf();
    }

/********************************************************
* nmrproc_next_config
********************************************************/
    # Next Configutation (with or without restored session)
    function nmrproc_next_conf() {

        # Get PATH_INFO informations
        $this->parse_PATH_INFO();

        if ($this->error>0) return;

        # Come from which method (GET/POST) ?
        $this->http_method = $_SERVER["REQUEST_METHOD"] ;

        # Init a new SESSION if needed
        if ($this->new_session) $this->set_new_session();

        # AJAX Function Call
        $this->func = $_GET['func'] ? $_GET['func'] : $_POST['func'];

        # Tracing
        $this->Tracing();

        # Debug information 
        if ($this->DEBUG) {
            $this->print_log(sprintf("%s %s",$this->http_method,$_SERVER["REQUEST_URI"]),$this->tmpdir);
            $this->print_log(sprintf("SID = %s",$this->sid),$this->tmpdir);
            $this->print_log(sprintf("func = %s",$this->func),$this->tmpdir);
            if ($this->need_to_reload)
               $this->print_log(sprintf("need_to_reload = %d",$this->need_to_reload),$this->tmpdir);
        }

        # Redirect (reload) if needed
        if ($this->need_to_reload) $this->redirect();

    }

/********************************************************
* parse_PATH_INFO
********************************************************/
    # Options come from PATH_INFO
    function parse_PATH_INFO() {
         do {

            # No PATH_INFO : stop here
            if (!$this->APACHE2 || ! $_SERVER['PATH_INFO']) {
                $this->set_errmsg(10);
                break;
            }

            # Retrieve options within the URL
            $options = explode("/",$_SERVER['PATH_INFO']);

            # First option => SID
            array_shift($options);
            $sid = array_shift($options);

            # SID is encoding a temporary session
            if (preg_match("/^".$this->IDREGEXP."$/",$sid) && strlen($sid)>=$this->IDMINLEN) {
                if ($this->session_exists($sid)==0) {
                   $this->datasetID=$sid;
                   $this->new_session=1;
                }
            } else {
                $this->set_errmsg(11);
                break;
            }

            # In all cases, datasetID must be initialized
            $dsprocdir = $this->get_ProcDir();
            if (!is_readable($dsprocdir)) {
                $this->set_errmsg(12);
                break;
            }

            # Get other URL parameters
            foreach ($options as $opt) {
                if ( preg_match('/^no(\w+)$/',$opt, $matches) ) {
                   $this->$matches[1] = 0;
                   if ($this->sid && $this->DEBUG)
                      $this->print_log(sprintf("%s = 0",$matches[1]),$this->tmpdir);
                }
                else {
                   $this->$opt=1;
                   if ($this->sid && $this->DEBUG)
                      $this->print_log(sprintf("%s = 1",$opt),$this->tmpdir);
              }
            }

         } while(0);
    }

/********************************************************
* set_new_session
********************************************************/
    # New/Change SESSION
    function set_new_session() {

         # New Session : new
         $this->nmrproc_init();
         $sid=$this->initSID();

         $this->new_session=0;
         $this->save_session();
         $this->need_to_reload=0;
    }

/********************************************************
* initSID
********************************************************/
    # SESSION Init.
    function initSID() {
        $this->sid=$this->datasetID;
        $this->urldir=$this->urlroot.'/tmp/'.$this->sid;
        $this->tmpdir=$tmpdir=$this->tmproot.'/'.$this->sid;
        $this->myurl .= '/'.$this->sid;
        if(!is_dir($tmpdir))
           `mkdir $tmpdir`;
        return $this->sid;
    }

/********************************************************
* session_file
********************************************************/
    # get SESSION FILENAME
    function session_file($sid='') {
        $sessid = isset($sid) && strlen($sid) ? $sid : $this->sid;
        return $this->tmproot."/$sessid/".$this->SESSION_FILE;
    }

/********************************************************
* session_exists
********************************************************/
    # Is SESSION FILENAME exists ?
    function session_exists($sid) {
        return is_readable($this->session_file($sid)) ? 1 : 0 ;
    }

/********************************************************
* save_session
********************************************************/
    # Save SESSION into the SESSION_FILE
    function save_session() {
        # Not saving the 'need_to_reload' avoid redirect loops
        $need_to_reload=$this->need_to_reload;
        $this->need_to_reload=0;
        $this->save_serialize_object($this->session_file(),$this->mySerialize_object($this));
        $this->need_to_reload=$need_to_reload;
    }

/********************************************************
* restore_session
********************************************************/
    # Restore SESSION from the SESSION_FILE
    function restore_session() {
       $session_file = $this->session_file();
       $obj = trim(`cat $session_file`);
       return $this->myUnserialize_object($obj);
    }

/********************************************************
* is_need_to_restore
********************************************************/
    # Is Need to Restore ?
    function is_need_to_restore() {
         do {
            # No PATH_INFO : stop here
            if (!$this->APACHE2 || ! $_SERVER['PATH_INFO']) break;

            # Retrieve options within the URL
            $options = explode("/",$_SERVER['PATH_INFO']);
            array_shift($options);

            # First option => SID
            $sid = array_shift($options);

            # is SID encoding a temporary session ?
            if (preg_match("/^".$this->IDREGEXP."$/",$sid) && strlen($sid)>=$this->IDMINLEN)
               if ($this->session_exists($sid)) $this->sid=$sid;
         } while (0);

         return $this->sid;
    }

/********************************************************
* save_within_session : 
   $sparam = name of the parameter
   $value  = value of the parameter
   $btype = type of the parameter : 0 for integer, 1 for string
********************************************************/    
    function save_within_session($sparam,$value,$btype) {
         $session_file = $this->session_file();
         do {
             $ret = trim(`which base64; echo $?`);
             if ($ret>0) break;
             $base64 = trim(`which base64`);
             $ret = trim(`cp $session_file $session_file.old; echo $?`);
             if ($ret>0) break;
             $len_param=strlen($sparam);
             $json_param = "s:$len_param:\\\"$sparam\\\"";
             $json_prev_value = trim(`$base64 -d $session_file 2>/dev/null | sed -e "s/;/\\n/g" | grep -A1 '$json_param' | tail -1`);
             if (preg_match('/^(s:\d+:)(.*)$/',$json_prev_value,$matches)) {
                  $json_prev_value = $matches[1].'\"'.$matches[2].'\"';
             }
             $len_value = strlen($value);
             $json_new_value = $btype ? "s:$len_value:\\\"$value\\\"" : "i:$value";
             $ret = trim(`$base64 -d $session_file | sed -e "s/$json_param;$json_prev_value/$json_param;$json_new_value/" > $session_file.old; echo $?`);
             if ($ret>0) break;
             $ret = trim(`cat $session_file.old | $base64 - | tr -d '\\n' > $session_file; echo $?`);
             if ($ret>0) break;
             $ret = trim(`rm -f $session_file.old; echo $?`);
         } while (0);
         return $ret;
    }

/********************************************************
* get_ProcDir
********************************************************/
    function get_ProcDir() {
        $dsprocdir = $this->DATASETS.'/'.$this->datasetID;
        return $dsprocdir;
    }

/********************************************************
* purge_TemporaryDir
********************************************************/
    function purge_TemporaryDir() {
        $tmproot = $this->tmproot;
        $dsprocdir = $this->DATASETS;
        $subdir_handle = @opendir($tmproot);
        while ($entry = readdir($subdir_handle)) {
            if (is_dir("$tmproot/$entry") && ! is_dir("$dsprocdir/$entry") ) `rm -rf $tmproot/$entry`;
        }
    }

/********************************************************
* Exec_cmd
********************************************************/
    function Exec_cmd($cmd,$errfile='') {
        $arr_output = array();
        $stderr = strlen($errfile) ? $errfile : $this->tmpdir.'/system_cmd.err';
        exec("$cmd 2>$stderr",$arr_output,$retval);
        if ($retval) $this->cmd_error_msg($cmd,$stderr,$retval);
        return $arr_output;
    }

/********************************************************
* System_cmd
********************************************************/
    function System_cmd($cmd,$errfile='') {
        $stderr = strlen($errfile) ? $errfile : $this->tmpdir.'/system_cmd.err';
        $retval=trim(`$cmd 2>$stderr; echo $?`);
        if ($retval) $this->cmd_error_msg($cmd,$stderr,$retval);
        return $retval;
    }

/********************************************************
* my(Un)Serialize_object
********************************************************/
    function mySerialize_object( $obj ) {
        return base64_encode(serialize($obj));
    }

    function myUnserialize_object( $txt ) {
        return unserialize(base64_decode($txt));
    }

/********************************************************
* save_serialize_object
********************************************************/
    function save_serialize_object($session_file,$str) {
        `cat > $session_file <<EOT
$str
EOT`;
    }

/********************************************************
* redirect
********************************************************/
    # Redirect to the page myurl/<parameters>'
    function redirect($param_url='') {
       $url = strlen($param_url) ? $this->myurl."/$param_url" : $this->myurl;
       if (strlen($_SERVER['QUERY_STRING'])) $url .= '?'.$_SERVER['QUERY_STRING'];
       header("Location: $url");
       exit(0);
    }

/********************************************************
* getmyip()
********************************************************/
    function get_ip () {
        if ($_SERVER["HTTP_X_FORWARDED_SERVER"]) {
           $ips = explode(',', $_SERVER["HTTP_X_FORWARDED_FOR"]);
           $myip = $ips[0];
        }
        else
           $myip = $_SERVER["REMOTE_ADDR"];
        return $myip;
    }

/********************************************************
* Tracing
********************************************************/
    # Print a message into the debug file
    function Tracing () {
        $httpd_method = $this->http_method;
        $URI = $_SERVER["REQUEST_URI"];
        $date = date('r');
        $myip = $this->get_ip();
        $tracingfile = $this->tracing."/$myip";
        `echo "$date - $httpd_method $URI " >> $tracingfile`;
    }

/********************************************************
* genKEY
********************************************************/
    # Generate a KEY (30  alpha-num. char. )
    function genKEY() {
        return md5(uniqid(rand(),true));
    }

/********************************************************
* cmd_error_msg
********************************************************/
    # Generate the message when a CMD failed into the debug file
    function cmd_error_msg ($cmd,$stderr,$retval) {
        $this->error++;
        $this->ERRORMSG.='ERROR'.$this->error.":&nbsp;(retval=$retval)&nbsp; - Func=".$this->func.'<BR>';
        $this->ERRORMSG.='CMD:&nbsp;'.$cmd.'<BR>===>&nbsp;'.$this->print_out($stderr).'<BR><BR>';
    }

/********************************************************
* print_log
********************************************************/
    # Print a message into the debug file
    function print_log ($msg,$tmpdir) {
        $sep = preg_match("/^(GET |POST )/",$msg) ? '' : '===' ;
        $msg_log = sprintf("%s $sep %s",$this->get_ip(),$msg);
        `echo "$msg_log" >> $tmpdir/debug`;
    }

/********************************************************
* print_out
********************************************************/
    function print_out($outfile) {
        if (is_readable("$outfile"))
           return `cat $outfile`;
        else
           return '';
    }

/********************************************************
* Error Message
********************************************************/
    function set_errmsg($msg_id) {
       switch ($msg_id) {
          case 10:
             $this->ERRORMSG='No available Dataset';
             break;
          case 11:
             $this->ERRORMSG='The temporary space is not available';;
             break;
          case 12:
             $this->ERRORMSG='The Dataset is not available';
             break;
       }
       $this->error=1;
    }
} // class


