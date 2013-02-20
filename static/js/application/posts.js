$(document).ready(function() {
	var preview_init=false;
	var do_preview=false;

  var refresh_preview = function() {
    $("#post-preview-body").attr("src", $("#post-preview-body").attr("src"));
  }

  $("#post-preview-body").load( function() {
	  // TODO: this is pretty hacky, make it less so
	  // 1) hide navigation bar:
		var doc = $("#post-preview-body").contents();
    doc.find("#page-nav").hide();
	  doc.find('head').append("<style> body { padding-top: 0px; } </style>");
	  // 2) remove header:
    doc.find("#post-header").hide();
	  // 3) adjust height of
    $("#post-preview-body").height(doc.height());

  });

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

	$.map($(".active-haskell"), function(c_blk) {
			var mkController = function(id) {
				return $("<a>", { href : "#"
							      	  , click : function() {
							       			  console.log($("#raw-"+id).text());
													  return false;
							       		  }
				                , html : "<i class=\"icon-cog\"></i>Execute"
							       	  }).attr("class","pull-right");
			};
			var ctrl = mkController(c_blk.id);
			ctrl.appendTo(c_blk);
	});

});
