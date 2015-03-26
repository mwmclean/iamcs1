$('#fsshortcut').on("click", function(){
	if (document.mozFullScreen !== undefined && !document.mozFullScreen) {
	  document.body.mozRequestFullScreen("f");
	} else if (document.webkitIsFullScreen !== undefined && !document.webkitIsFullScreen) {
	  document.body.webkitRequestFullScreen("f");
	} else {
	  document.cancelFullScreen();
	}
	$(this).css( "display", "none" );
        $('#printshortcut').css( "display", "none" );
	$('#dlshortcut').css( "display", "none" );
});

$('#printshortcut').on("click", function(){
  var e = jQuery.Event("keydown");
  e.ctrlKey = true;
  e.keyCode = 80;
  console.log(e.keyCode);
  $(document.body).trigger(e);
  window.print();
});

/*
var e = jQuery.Event("keypress");
e.which = 70; // # Some key code value
$(".example").click(e);


$('a.example').click(function(){
    $( document.body ).trigger({
        type: 'keypress',
        which: 70,
        keyCode: 70
    });
});

$(document).keyup(function(e) {
    if (e.keyCode == 102) { // Esc
        window.close(); // or whatever you want
    }
});
*/
