$(document).on("click", ".inputtextarea", function (e) {
     if (CaptureMode==1) {
        get_spectrum_range();
     }
     var el = $(e.target);
     if (el.hasClass("noise") ) {
         $(".noise").each(function(){ 
              $(this).val( el.val() );
              if ( $(this) != el ) $(this).trigger("change"); 
         });
     }
     el.trigger("change");
})
