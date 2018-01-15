<!--
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

//-------------------------------------------
// Eval - through AJAX caller
// SetInnerHTML secure (through AJAX caller)
//-------------------------------------------
(evalScript = function(e){ var h = evalScript.node, s = document.createElement("script"); s.type = "text/javascript"; s.text = e;  h.appendChild(s); h.removeChild(s);}).node = document.getElementsByTagName("head")[0] || document.getElementsByTagName("*")[0];
function setInnerHTML(divContent, HTML) { 
    divContent.innerHTML=HTML; var re = /<script[^>]+>([^<]+)<\/script>/mgi, re2=/<\/?script([^>])*>/gi, str_src=HTML.match(re);
    if (str_src) { code=str_src[0].replace(re2,""); evalScript(code);  };
    var AllScripts=divContent.getElementsByTagName("script");
    for (var i=0; i<AllScripts.length; i++) {var s=AllScripts[i]; if (s.src && s.src!="") { setTimeout("eval(getFileContent(" + s.src + "))",0)} else { evalScript(s.innerHTML)}}
}

//-->

