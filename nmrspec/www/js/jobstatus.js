//  See spec_capture.js for jobname_calibration value
// jobname_calibration can change value (see Proc4.R) if a global variable is defined in the config.ini file

// jobStatus input element 
var jobStatusTrigger = function() {
    var el = $("#jobstatus");
    var el2 = $("#jobname");
    if (el.val()=='Ended') {
         if (el2.val()==jobname_calibration) {
            reset_spectrum();
         } else {
            refresh_spectrum(0);
         }
         $('div.row').find('.treset').val('');
         $('div.row').find('.capture').trigger("change")
    }
    el.trigger("change");
}
