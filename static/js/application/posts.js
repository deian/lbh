$(document).ready(function() {
	var preview_init=false;
	var do_preview=false;

  var refresh_preview = function() {
    $("#post-preview-body").empty();
    var url = $("#post-preview-body").data("src");
    $.get(url, function () {}).done(function(doc) {
      $(doc).find("#post-body").appendTo("#post-preview-body")
    });
  }

  $("#post-preview-btn").click( function () {
		if(!preview_init) {
			$("#post-preview").show();
		  preview_init = true;
		}
		// -- 
	  if(do_preview) {
			// hide preview
	    $("#post-preview").hide();
			preview_init = false;
	  } else {
			// refresh preview content
			refresh_preview();
		}
		// toggle
		do_preview=!do_preview;
  });

  $("#post-refresh-preview-btn").click( function () {
			refresh_preview();
	});

  $("#post-save-btn").click( function () {
			$.ajax({ url  : '/posts'
		         , type : 'PUT'
		         , data : $("#editPost").serialize()}
						).done(function() {
						    if(do_preview) { refresh_preview(); }
						});
	});

  $("#post-make-public-btn").click( function () {
			$.ajax({ url  : '/posts'
		         , type : 'PUT'
		         , data : $("#editPost").serialize()+"&isPublic=True"}
						).done(function() {
						    if(do_preview) { refresh_preview(); }
								$("#post-make-public-btn").hide();
						});
	});

});
