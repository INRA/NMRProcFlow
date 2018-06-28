// -- spec_capture.js --
// (C) INRA 1332 BFP, PMB, MetaboHUB 2015
//
// Management of keyboard events for capturing any ppm ranges
// * The spectra viewer must be loaded within the iframe identified as 'ifspecview'
// * The 'Ctrl' keycode (17) serves to toggle between both capture and normal mode in the spectrum object
// * In 'normal' mode, we can zoom in/out the spectra
// * In 'capture' mode, the selected ppm range is put in a focused textarea by pressing any key after the ppm area selection
//
var CaptureMode=0;
var ctrlKey = 17;
var altKey = 18;
var viewerLoaded=0;

var toggle_capturemode = function() {
     do {
        if( ! $("#ifspecview").get(0) || viewerLoaded==0 ) break;
        if (CaptureMode==1) {
            $("#ifspecview").get(0).contentWindow.set_spectrum_keycode(ctrlKey);
            $("#ifspecview").css({ "background-color": "rgb(234, 239, 250)" });
            break;
        }
        if (CaptureMode==0) {
            $("#ifspecview").get(0).contentWindow.set_spectrum_keycode(0);
            $("#ifspecview").css({ "background-color": "rgb(255, 255, 255)" });
            break;
        }
     } while (0)
}

var refresh_spectrum = function() {
     do {
        if( ! $("#ifspecview").get(0) || viewerLoaded==0 ) break;
        $("#ifspecview").get(0).contentWindow.spectrum_view();
        setTimeout(function(){toggle_capturemode();}, 1000);
     } while (0)
}

var refresh_if_capturebox = function() {
     do {
        if ( ! $( document.activeElement ).hasClass( "capture" ) ) break;
        refresh_spectrum();
     } while (0)
}

var resize_spectrumHeight = function(imgheight) {
     do {
        if( ! $("#ifspecview").get(0) || viewerLoaded==0 ) break;
        ifrminheight=imgheight+76
        $("#ifspecview").height(ifrminheight);
        $("#ifspecview").get(0).contentWindow.set_spectrum_imgheight(imgheight);
        refresh_spectrum();
     } while (0)
}

var get_spectrum_range = function() {
     do {
        if( ! $("#ifspecview").get(0) || viewerLoaded==0 ) break;
        if (CaptureMode==1 && $( document.activeElement ).is("textarea")) {
            vallist = $( document.activeElement ).val().trim()
            if (vallist.length>0) vallist = vallist + "\n"
            if ( $( document.activeElement ).hasClass("single") ) {
                 $( document.activeElement ).val( $("#ifspecview").get(0).contentWindow.get_spectrum_range() )
            } else {
                 $( document.activeElement ).val( vallist + $("#ifspecview").get(0).contentWindow.get_spectrum_range() )
            }
        }
     } while (0)
}

var replace_by_NL = function(id) {
    $('#'+id).val(function(i, v) { //index, current value
        return v.replace(/;/g,'\n');
    });
}

$(window).on("keydown", function (e) {
    do {
       if( ! $("#ifspecview").get(0) || viewerLoaded==0 ) break;
       if (e.keyCode == ctrlKey || e.keyCode == altKey) {
            CaptureMode = 1;
            toggle_capturemode();
            e.preventDefault();
            e.stopPropagation();
        }
    } while (0)
});

$(window).on("keyup", function (e) {
    do {
       if( ! $("#ifspecview").get(0) || viewerLoaded==0 ) break;
       if (e.keyCode == ctrlKey || e.keyCode == altKey) {
            CaptureMode = 0;
            toggle_capturemode();
            e.preventDefault();
            e.stopPropagation();
        }
    } while (0)
});
