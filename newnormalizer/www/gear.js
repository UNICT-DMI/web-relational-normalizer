var globalCommand;
var hist;
var data;
var data1;

function generate_example() {
    document.getElementById("attrs").value = "A B C D E";
    document.getElementById("deps").value = "B E -> E\nA -> E\nB C E -> A\nD -> C";
    document.getElementById("attrs2").value = "A D E";
    document.getElementById("deps2").value = "B C -> A";
    document.getElementById("decomp").value = "(A B C E)\n(D C)";
}

function resetHist() {
    hist = "";
}

function addHist(n) {
    if (hist == "") {
	hist = hist.concat(n);
    } else {
	hist = hist.concat("*", n);
    }
}

function show(cl) {
    var all = document.getElementsByClassName(cl);
    for (var i = 0, x; x = all[i++];)
    {x.style.display='block';}
}

function hide(cl) {
    var all = document.getElementsByClassName(cl);
    for (var i = 0, x; x = all[i++];)
    {x.style.display='none';}
}

function showRest() {
    document.getElementById("res").style.display='block';
}

function showAll() {
    showRest();
    show('2col');
    show('1row');
    show('2row');
    show('3row');
    show('4row');
    show('datiulteriori');
}

function setGlobal(c) {
    globalCommand = c;
}

function executeGlobal(c) {
    switch (globalCommand) {
    case 1:
	closure();
	break;
    case 2:
	canonical();
	break;
    case 3:
	tnf();
	break;
    case 4:
	isbcnf();
	break;
    case 5:
	aimpb();
	break;
    case 6:
	aequivb();
	break;
    case 7:
	fkey();
	break;
    case 8:
	fprimes();
	break;
    case 10:
	is3();
	break;
    case 11:
	proj();
	break;
    case 12:
	tnfpd();
	break;
    case 13:
	tnftbc();
	break;
    case 14:
	bctdp();
	break;
    case 15:
	bcsbs();
	break;
    case 16:
	norm();
	break;
    case 17:
	is4();
	break;
    case 18:
	fnf();
	break;
    case 19:
	fakey();
	break;
    case 20:
	checkdec();
	break;
    case 21:
	checkdecpresdata();
	break;
    case 22:
	checkdecpresfdep();
	break;
    case 23:
	checkdecpresall();
	break;
    case 24:
	checkdec2nf();
	break;
    case 25:
	checkdec3nf();
	break;
    case 26:
	checkdecbcnf();
	break;
    default:
	alert('Wrong Error!');
    }
}

function fundep_att() {
    showAll();
    hide('1row');
    hide('2col');
    hide('4row');
}

function only_dep() {
    fundep_att();
    hide('3row');
    hide('4row');
    hide('datiulteriori');
}

function one_rel() {
    showAll();
    hide('2col');
    hide('3row');
    hide('4row');
    hide('datiulteriori');
}

function two_deps() {
    showAll();
    hide('1row');
    hide('3row');
    hide('4row');
}

function one_decomp() {
    showAll();
    hide('1row');
    hide('2col');
    hide('3row');
}

function callback(response) {
    var res = response;
    document.getElementById('res').innerHTML = res;
}

function synback(response) {
    var res = response.substring(69, response.indexOf("</response>"));
    if (res.substring(0,1) == "l") {
        alert ("Correct! But since the number of attributes is greater than 14, the functions that have an exponential cost (marked in red) will not be available.");
    } else if (res.substring(0,1) == "y") {
        alert("Correct!");
    } else if (res.substring(0,1) == "s") {
	alert("Incorrect! Limit of input size exceeded.")
    } else {
        alert("Incorrect: the functional dependencies contains one or more unknown attributes");
    }
}

function icallback(response) {
    var res = response.substring(69, response.indexOf("</response>"));
    if (res.substring(0,1) == "*") {
	document.getElementById('res').innerHTML = res.substring(1);
    } else if (res.substring(0,1) == "E") {
	alert("Incorrect: the functional dependencies contains one or more unknown attributes");
    } else {
	var user = prompt(res,"1");
	if (user == null) {
            user = "1";
	}
	addHist(user);
	bcsbs();
    }
}

function fetchURI(uri, callback) {
    var request;
    if (window.XMLHttpRequest) { request = new XMLHttpRequest(); }
    else {
	try { request = new ActiveXObject("Msxml2.XMLHTTP"); } catch (e) {
	    try { request = new ActiveXObject("Microsoft.XMLHTTP"); } catch (ee) {
		request = null;
	    }}}
    if (!request) alert("Browser couldn't make a request object.");

    request.open('GET', uri, true);
    request.onreadystatechange = function() {
	if (request.readyState != 4) return;
	if (((request.status>=200) && (request.status<300)) ||
	    (request.status == 304)) {
	    data = request.responseText; //request.responseXML.firstChild.nodeValue;
	    if (callback!=null) { callback(data); }
	}
	else { 
	    alert('Error while fetching URI ' + uri);
	}
    }
    request.send(null);
    delete request;
}

function ajax_call(func, callback, args) {
    var uri = '/ajax/' + encodeURIComponent(func) + '/';
    var i;
    if (args.length > 0) {
	uri += '?'
	for (i = 0; i < args.length; ++i) {
	    if (i > 0) { uri += '&' };
	    uri += 'arg' + i + '=' + encodeURIComponent(args[i]);
	}
    }
    fetchURI(uri, callback);
}

function check_syn() {
    var atts = document.getElementById('attrs').value;
    var deps = document.getElementById('deps').value;
    ajax_call('MYSYNTAX', synback, [atts, deps]);
}

function norm() {
    ajax_call('NORM', callback, [document.getElementById('attrs').value,
				document.getElementById('deps').value,
				document.getElementById('expl').checked]);
}

function closure() {
    ajax_call('CLOSURE', callback, [document.getElementById('attrs').value,
				    document.getElementById('deps').value,
				    document.getElementById('attrs2').value,
				    document.getElementById('expl').checked]);
}

function proj() {
    ajax_call('PROJECT', callback, [document.getElementById('attrs').value,
				    document.getElementById('deps').value,
				    document.getElementById('attrs2').value,
				    document.getElementById('expl').checked]);
}

function canonical() {
    ajax_call('CANONICALIZE', callback, [document.getElementById('attrs').value,
					 document.getElementById('deps').value,
					 document.getElementById('expl').checked]);
}

function fkey() {
    ajax_call('FINDALLKEYS', callback, [document.getElementById('attrs').value,
					document.getElementById('deps').value,
					document.getElementById('expl').checked]);
}

function fakey() {
    ajax_call('FINDONEKEY', callback, [document.getElementById('attrs').value,
					document.getElementById('deps').value,
					document.getElementById('expl').checked]);
}

function fprimes() {
    ajax_call('FINDPRIMES', callback, [document.getElementById('attrs').value,
				       document.getElementById('deps').value,
				       document.getElementById('expl').checked]);
}

function isbcnf() {
    ajax_call('ISBCNF', callback, [document.getElementById('attrs').value,
				   document.getElementById('deps').value,
				   document.getElementById('expl').checked]);
}

function is3() {
    ajax_call('IS3NF', callback, [document.getElementById('attrs').value,
				  document.getElementById('deps').value,
				  document.getElementById('expl').checked]);
}

function tnf() {
    ajax_call('TNF', callback, [document.getElementById('attrs').value,
				document.getElementById('deps').value,
				document.getElementById('expl').checked]);
}

function tnfpd() {
    ajax_call('TNFPD', callback, [document.getElementById('attrs').value,
				  document.getElementById('deps').value,
				  document.getElementById('expl').checked]);
}

function tnftbc() {
    ajax_call('TNFTBC', callback, [document.getElementById('attrs').value,
				   document.getElementById('deps').value,
				   document.getElementById('expl').checked]);
}

function bctdp() {
    ajax_call('BCTDP', callback, [document.getElementById('attrs').value,
				  document.getElementById('deps').value,
				  document.getElementById('expl').checked]);
}

function aimpb() {
    ajax_call('AIMPLB', callback, [document.getElementById('attrs').value,
				   document.getElementById('deps').value,
				   document.getElementById('deps2').value,
				   document.getElementById('expl').checked]);
}

function aequivb() {
    ajax_call('AEQUIVB', callback, [document.getElementById('attrs').value,
				   document.getElementById('deps').value,
				    document.getElementById('deps2').value,
				    document.getElementById('expl').checked]);
}

function bcsbs() {
    ajax_call('BCSBS', icallback, [document.getElementById('attrs').value,
				   document.getElementById('deps').value,
				   hist,
				   document.getElementById('expl').checked]);
}

function is4() {
    ajax_call('IS4NF', callback, [document.getElementById('attrs').value,
				  document.getElementById('deps').value,
				  document.getElementById('expl').checked]);
}

function fnf() {
    ajax_call('FNF', callback, [document.getElementById('attrs').value,
				document.getElementById('deps').value,
				document.getElementById('expl').checked]);
}

function checkdec() {
    ajax_call('CHECKDEC', callback, [document.getElementById('attrs').value,
				    document.getElementById('deps').value,
				    document.getElementById('decomp').value,
				    document.getElementById('expl').checked]);
}

function checkdecpresdata() {
    ajax_call('CHECKDECPRESDATA', callback, [document.getElementById('attrs').value,
					     document.getElementById('deps').value,
					     document.getElementById('decomp').value,
					     document.getElementById('expl').checked]);
}

function checkdecpresfdep() {
    ajax_call('CHECKDECPRESFDEP', callback, [document.getElementById('attrs').value,
					     document.getElementById('deps').value,
					     document.getElementById('decomp').value,
					     document.getElementById('expl').checked]);
}

function checkdecpresall() {
    ajax_call('CHECKDECPRESALL', callback, [document.getElementById('attrs').value,
					    document.getElementById('deps').value,
					    document.getElementById('decomp').value,
					    document.getElementById('expl').checked]);
}

function checkdec2nf() {
    ajax_call('CHECKDEC2NF', callback, [document.getElementById('attrs').value,
					document.getElementById('deps').value,
					document.getElementById('decomp').value,
					document.getElementById('expl').checked]);
}

function checkdec3nf() {
    ajax_call('CHECKDEC3NF', callback, [document.getElementById('attrs').value,
					document.getElementById('deps').value,
					document.getElementById('decomp').value,
					document.getElementById('expl').checked]);
}

function checkdecbcnf() {
    ajax_call('CHECKDECBCNF', callback, [document.getElementById('attrs').value,
					 document.getElementById('deps').value,
					 document.getElementById('decomp').value,
					 document.getElementById('expl').checked]);
}



