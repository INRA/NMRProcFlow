// jobStatus input element 
var  jobStatusTrigger = function() {
    var el = $( "#jobstatus");
    if (el.val()=='Ended') { 
         refresh_spectrum();
         $('div.row').find('.treset').val('');
         $('div.row').find('.capture').trigger("change")
    }
    el.trigger("change");
}
