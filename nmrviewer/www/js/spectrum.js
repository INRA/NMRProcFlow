/*
# Nom du fichier: spectrum.js
# Auteur(s): D.Jacob
# Copyright: (c) CBiB - 2011
*/
// variables utilisées
var agt = navigator.userAgent.toLowerCase();
var isMac = (agt.indexOf('mac') != -1);
var isFirefox = (agt.indexOf('firefox') != -1);
var isChrome = (agt.indexOf('chrome') != -1);
var isOpera = (agt.indexOf('opera') != -1);
var IEver = parseInt(agt.substring(agt.indexOf('msie ') + 5));
var isIE = ((agt.indexOf('msie')!=-1 && !isOpera && (agt.indexOf('webtv')==-1)) && !isMac) && IEver<9;

function sprintf() {
if (!arguments || arguments.length < 1 || !RegExp) return;
var str = arguments[0], re = /([^%]*)%('.|0|\x20)?(-)?(\d+)?(\.\d+)?(%|b|c|d|u|f|o|s|x|X)(.*)/, a = b = [], numSubstitutions = 0, numMatches = 0;
while (a = re.exec(str)) {
  var leftpart = a[1], pPad = a[2], pJustify = a[3], pMinLength = a[4], pPrecision = a[5], pType = a[6], rightPart = a[7];
  numMatches++;
  if (pType == '%') subst = '%';
  else { numSubstitutions++;
    if (numSubstitutions >= arguments.length)
       alert('Error! Not enough function arguments (' + (arguments.length - 1) + ', excluding the string)\nfor the number of substitution parameters in string (' + numSubstitutions + ' so far).');
    var param = arguments[numSubstitutions], pad = '', justifyRight = true, minLength = -1, precision = -1, subst = param;
    if (pPad && pPad.substr(0,1) == "'") pad = leftpart.substr(1,1); else if (pPad) pad = pPad;
    if (pJustify && pJustify === "-") justifyRight = false;
    if (pMinLength) minLength = parseInt(pMinLength);
    if (pPrecision && pType == 'f') precision = parseInt(pPrecision.substring(1));
    if (pType == 'b') subst = parseInt(param).toString(2);
    else if (pType == 'c') subst = String.fromCharCode(parseInt(param));
    else if (pType == 'd') subst = parseInt(param) ? parseInt(param) : 0;
    else if (pType == 'u') subst = Math.abs(param);
    else if (pType == 'f') subst = (precision > -1) ? Math.round(parseFloat(param) * Math.pow(10, precision)) / Math.pow(10, precision): parseFloat(param);
    else if (pType == 'o') subst = parseInt(param).toString(8);
    else if (pType == 's') subst = param;
    else if (pType == 'x') subst = ('' + parseInt(param).toString(16)).toLowerCase();
    else if (pType == 'X') subst = ('' + parseInt(param).toString(16)).toUpperCase();
  }
  str = leftpart + subst + rightPart;
}
return str;
}

var ObjImage = function () {
   // GUI elements
   this.div       = 'DSPEC';
   this.img       = '#spec';
   this.msg       = '#right';
   this.waitdiv   = '#waitdiv';
   this.cursor    = '#verticale';
   this.container = '#MAINSPEC';
   this.selppm    = 'selppm';
   this.unit      = 'ppm';
   
   // AJAX parameters
   this.AJ_PROG   = '';
   this.params    = '';
   this.callback  = function(){};

   // IMAGE size & margins (pixels)
   this.iwidth  = 800;
   this.iheight = 256;
   this.x1     = 70; /* 60 with gunplot 4.6 patchlevel 4 */
   this.x2     = 793;
   this.y1     = 12;
   this.y2     = 232;
   this.resizeTO = 0;

   // Maximal boundaries of the X coordinate (in unit)
   this.SpecValMin=0;
   this.SpecValMax=0;

   // Current boundaries of the X coordinate (in unit)
   this.Xmin   = 0;
   this.Xmax   = 0;

   // internal variables
   this.val    = 0;
   this.val2   = 0;
   this.ox     = 0;
   this.oy     = 0;
   this.xpos   = 0;
   this.ypos   = 0;
   this.xneg   = 0;
   this.xval   = 0;
   this.mDwn   = 0;
   this.keycode= 0;
   this.flg    = 0;
   this.first  = 1;  // first call
   this.fslider = 0;

   this.init = function (iwidth, iheight, x1,x2,y1,y2,Xmin,Xmax,SpecValMin,SpecValMax,xneg) {
       this.x1 = x1; this.x2 = x2; this.y1 = y1; this.y2 = y2;
       this.Xmin = Xmin; this.Xmax = Xmax;
       this.iwidth = iwidth; this.iheight = iheight;
       this.SpecValMin=SpecValMin; this.SpecValMax=SpecValMax;
       this.xneg = xneg;
   }

   this.coords = function (e) { // capturer position de la souris
       var flg = 0;
       this.ox   = $(this.img).offset().left;
       this.oy   = $(this.img).position().top;
       var d = document.documentElement, b = document.body;
       this.xpos = isIE ? event.clientX + d.scrollLeft + b.scrollLeft : e.pageX;
       this.ypos = isIE ? event.clientY + d.scrollTop + b.scrollTop : e.pageY;
       if (isIE) { this.ox += 2; this.oy += 2;}
       this.xpos -= this.ox; this.ypos -= this.oy;
       if (isIE) { this.xpos -= 2; this.ypos -=2; }
       if (this.ypos<=this.iheight && this.ypos>=0 )
           flg =1 ;
       this.flg = flg ;
       return flg;
   }

   this.getVal = function (x) {
       var val;
       var xmax = this.x2 - this.x1 - 1;
       if (x<0) x=0;
       if (x>xmax) x=xmax;
           val = this.xneg ? Math.round(1000*(this.Xmax + (x/xmax)*(this.Xmin-this.Xmax)) )/1000 :
                             Math.round(1000*((x/xmax)*(this.Xmax-this.Xmin) + this.Xmin) )/1000;
       return val;
   }

   this.cursorDraw = function (x) {
       $(this.cursor).css('position','absolute');
       if (this.mDwn==0) {
           $(this.cursor).css({ 'borderRight' : '0px #00ff00', 'width' : 0 +'px', 'height' : this.y2 - this.y1 +'px', 
                                'top' : this.oy + this.y1 + 'px', 'left' : x + 'px'});
           if (isIE) $(this.cursor).css('filter', 'alpha(opacity=100)'); else $(this.cursor).css('opacity', 1.0);
           $(this.cursor).css('z-index', 3);
       }
       else {
           (x>this.xval) ?
               $(this.cursor).css({ 'borderRight' : 'dotted 1px #ff0000', 'width' : x - this.xval +'px', 'height' : this.y2 - this.y1 +'px', 'top' : this.oy + this.y1 + 'px','left' : this.xval + 'px'}) :
               $(this.cursor).css({ 'borderRight' : 'dotted 1px #ff0000', 'width' : this.xval -x +'px', 'height' : this.y2 - this.y1 +'px', 'top' : this.oy + this.y1 + 'px','left' : x + 'px'});
           if (isIE) $(this.cursor).css('filter', 'alpha(opacity=20)'); else $(this.cursor).css('opacity', 0.2);
       }
       $(this.cursor).css('display','inline');
   }

   this.cursorMove = function () {
       var x = this.xpos - this.x1;
       if (this.flg) {
          var xmax = this.x2 - this.x1 - 1;
          if (x>0 && x<=xmax) {
              this.cursorDraw(this.xpos + this.ox)
              var val = this.getVal(x);
              if (this.mDwn)
                    (this.xneg && val<this.val)  ? $(this.msg).text(this.unit+' : max='+sprintf("%07.4f",this.val)+' - min='+sprintf("%07.4f",val)) :
                                $(this.msg).text(this.unit+' : min='+sprintf("%07.4f",this.val)+' - max='+sprintf("%07.4f",val));
              else
                    $(this.msg).text(this.unit+' = '+sprintf("%07.4f",val));
          }
       }
   }

   this.captureWin = function () {
       var x = this.xpos - this.x1;
       if (this.flg) {
          var xmax = this.x2 - this.x1 - 1;
          this.val  = this.getVal(x);
          this.xval = this.xpos + this.ox;
          this.mDwn = 1;
       }
   }

   this.getNewIMG = function () {
       var x = this.xpos - this.x1;
       var xmax = this.x2 - this.x1 - 1;
       this.mDwn = 0;
       var v=x;
       if (x<0) v=0;
       if (x>xmax) v=xmax;
       if (this.flg && ! this.fslider) {
          this.val2  = this.getVal(v);
          var xval2 = (v + this.x1) + this.ox;
          if (this.xval > xval2) { 
              tmp=this.xval; this.xval=xval2; xval2=tmp; tmp=this.val; this.val=this.val2; this.val2=tmp;
          }
          if (this.keycode==0) {
              if (this.xval < xval2) {
                  this.xneg ? this.AJ_XMS('val1='+this.val2+'&val2='+this.val) :
                              this.AJ_XMS('val1='+this.val+'&val2='+this.val2);
              }
          } else {
              if (this.keycode==17 && this.val2!=0) {
                  document.getElementById(this.selppm).innerHTML = sprintf("%7.4f",this.val)+' '+sprintf("%7.4f",this.val2)
              }
          }
       }
   }

   this.captureReset = function () {
       this.mDwn = 0;
       document.getElementById(this.selppm).innerHTML = ""
       this.cursorMove();
   }

   this.show_waitdiv = function () {
       $(this.waitdiv).css({'position':'absolute','width': '200px', 'height': '50px','overflow': 0, 'z-index': 10, 'background-color': '#FFFFFF',
                            'top' : this.oy + 50 +'px', 'left' : this.ox + 250 +'px', 'display' : 'inline'});
       $(this.container).unbind();
   }
   this.hide_waitdiv = function () {
       $(this.waitdiv).css('display','none');
       $(this.cursor).css('display','none');
       $(this.container).mousedown($.proxy(this.e_captureWin,this));
       $(this.container).mousemove($.proxy(this.e_cursorMove,this));
   }

   this.AJ_XMS = function (params) {
       var my_params = this.params;
       if (params) my_params += '&'+params;

       this.show_waitdiv();
       var self=this;
       $.ajax({
            url:self.AJ_PROG+my_params,
            global:false
       }).done(function ( response ) {
            document.getElementById(self.div).innerHTML = response;
            eval($(response).find("script").text());
            self.hide_waitdiv();
            setTimeout(self.resizeIframe,500);
            self.callback();
            self.first=0;
       });
   }

   this.zoom = function (zoomtype) {
       var delta = Math.abs(this.Xmax - this.Xmin)/2.5;
       var zmin  = Math.max(Math.min(Math.min(this.Xmin,this.Xmax) - zoomtype*delta, this.SpecValMax),this.SpecValMin);
       var zmax  = Math.max(Math.min(Math.max(this.Xmin,this.Xmax) + zoomtype*delta, this.SpecValMax),this.SpecValMin);
       this.AJ_XMS('val1='+Math.min(zmin,zmax)+'&val2='+Math.max(zmin,zmax));
   }

   this.view = function () {
       this.AJ_XMS('val1='+this.Xmin+'&val2='+this.Xmax);
       this.mDwn=0;
   }

   this.resizeIframe = function (e) {
       var self=this;
       $("iframe", window.parent.document).each (function() {
           if ( $( this ).attr('id').match(/ifspec_.+/)) {
                $( this ).css('height',$( this ).get(0).contentWindow.document.body.offsetHeight + 'px');
           }
       });
   }

   this.e_cursorMove = function (e) {
       if (!e) e = window.event;
       if (this.coords(e))  { this.cursorMove(); }
       else  window.focus();
   }

   this.e_captureWin = function (e) {
       if (!e) e = window.event;
       $(this.container).unbind('mousedown');
       $(document).unbind('keyup');
       $(this.container).mouseup($.proxy(this.e_getNewIMG,this));
       if (this.coords(e)) this.captureWin();
   }

   this.e_getNewIMG = function (e) {
       $(this.container).unbind('mousemove');
       $(this.container).unbind('mouseup');
       $(this.container).mousedown($.proxy(this.e_captureWin,this));
       if (this.coords(e)) this.getNewIMG();
       $(document).keyup($.proxy(this.e_captureReset,this));
   }

   this.e_captureReset = function (e) {
       if (!e) e = window.event;
       if (e.which==27) {
           this.captureReset();
           $(this.container).mousemove($.proxy(this.e_cursorMove,this));
           $(document).unbind('keyup');
       }
   }
}
