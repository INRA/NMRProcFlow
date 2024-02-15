// jobStatus input element 
var jobStatusTrigger = function() {
    var el = $( "#jobstatus");
    var el2 = $( "#jobname");
    if (el.val()=='Ended') {
         if (el2.val()=='calibration') {
            reset_spectrum();
         } else {
            refresh_spectrum(0);
         }
         $('div.row').find('.treset').val('');
         $('div.row').find('.capture').trigger("change")
    }
    el.trigger("change");
}
