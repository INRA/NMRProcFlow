$(document).on("click", ".inputtextarea", function (e) {
     if (CaptureMode==1) {
        get_spectrum_range();
     }
     var el = $(e.target);
     el.trigger("change");
})
