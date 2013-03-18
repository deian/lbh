$(document).ready(function() {

	if($("#newPost").length > 0 || $("#editPost").length > 0) {
  	//
    // New and Edit
  	//
  		
  	if($(".tagManager").length > 0) {
  			var pre = "";
  			/* fill existing tags */
  			if($("#prefilled-tagsAggr").length>0) {
  					pre = $("#prefilled-tagsAggr").val();
  			}
  			$(".tagManager").tagsManager( { maxTags : 10 /* fake limit*/
  																		, prefilled : pre
  																		, typeahead : true
  																		, typeaheadAjaxSource : '/tags'
  																		//, typeaheadAjaxPolling : true
  																		});
  	}
  
  	var set_tags = function(postId) {
  			if($("input[name=hidden-tagsAggr]").length>0) {
  				$("input[name=hidden-tagsAggr]").val().split(",").map( function(t) {
  						if(t !== "" && $("input[type=hidden][name=\"tags[]\"][value="
  														 +t+"]").length==0) {
  						   	$("<input>", { type : 'hidden'
  						   							 , name : 'tags[]'
  						   							 , value : t }).appendTo(postId);
  						}
  				});
  			}
  	}; 
  
  		
  	var post_body = CodeMirror.fromTextArea($("#body")[0], {
  		  lineWrapping: true
      , theme : "elegant"
			, viewportMargin: Infinity
			, lineNumbers: true
			, collapseRange: true
  	});

		post_body.on("change", function(inst,chObj) {
			$("#body").val(inst.getValue());
		});
	}
	
	(function () { 
   	//
   	// New
   	//

		$("#newPost").submit( function() {
				set_tags("#newPost");
		});

	})();

	(function () { 
  	//
  	// Edit
  	//

  
  	var preview_init=false;
  	var do_preview=false;
  
    var refresh_preview = function() {
      $("#post-preview-body").attr("src", $("#post-preview-body").attr("src"));
    }
  
  	window.addEventListener("message", function(e) {
  		if(e.data == "preview-resize") {
        $("#post-preview-body").height($("#post-preview-body").contents().height());
  		}
  	}, false);
  
    $("#post-preview-body").load( function() {
  	  // TODO: this is pretty hacky, make it less so
  	  // 1) hide navigation bar:
  		var doc = $("#post-preview-body").contents();
      doc.find("#page-nav").hide();
  	  doc.find('head').append("<style> body { padding-top: 0px; } </style>");
  	  // 2) remove header:
      doc.find("#post-header").hide();
  	  // 3) adjust height of preview iframe
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
  			// save and refresh preview content
        $("#post-save-btn").click();
  		}
  		// toggle
  		do_preview=!do_preview;
    });
  
    $("#post-refresh-preview-btn").click( function () {
  			refresh_preview();
  	});
  
    $("#post-save-btn").click( function () {
				set_tags("#editPost");
  			$.ajax({ url  : '/posts'
  		         , type : 'PUT'
  		         , data : $("#editPost").serialize()}
  						).done(function() {
  						    if(do_preview) { refresh_preview(); }
  						}).fail(function() {
  							 $("#main-alert-msg").html(
  									"<i class=\"icon-warning-sign\"></i> "+
  									"<strong>Server error:</strong> Save failed.");
  							 $("#main-alert").show();
							});
  	});

    $("#post-delete-btn").click( function () {
  			$.ajax({ url  : '/posts'
  		         , type : 'DELETE'
  		         , data : "_id="+$("#editPost").find("input[name=_id]").val()
									      +"&_method=DELETE"
							 }).always(function() {
									window.location="/posts";
  						});
  	});
  

    $("#post-make-public-btn").click( function () {
				set_tags("#editPost");
  			$.ajax({ url  : '/posts'
  		         , type : 'PUT'
  		         , data : $("#editPost").serialize()+"&isPublic=True"}
  						).done(function() {
  								$("#post-make-public-btn").hide();
  								$("#post-make-private-btn").show();
									refresh_preview();
  						}).fail(function() {
  							 $("#main-alert-msg").html(
  									"<i class=\"icon-warning-sign\"></i> "+
  									"<strong>Server error:</strong> Failed to make post public.");
  							 $("#main-alert").show();
							});
  	});

    $("#post-make-private-btn").click( function () {
				set_tags("#editPost");
  			$.ajax({ url  : '/posts'
  		         , type : 'PUT'
  		         , data : $("#editPost").serialize()+"&isPublic=False"}
  						).done(function() {
  								$("#post-make-private-btn").hide();
  								$("#post-make-public-btn").show();
									refresh_preview();
  						}).fail(function() {
  							 $("#main-alert-msg").html(
  									"<i class=\"icon-warning-sign\"></i> "+
  									"<strong>Server error:</strong> Failed to make post private.");
  							 $("#main-alert").show();
							});
  	});

		var cur_users = [];

		var removeFirst = function(val, arr) {
				var idx = $.inArray(val, arr);
        if(idx >= 0 ) { arr.splice(idx, 1);}
				return arr;
		};

    $('#post-add-collaborator').typeahead( {
				 source : function(query, process) {
  		     $.ajax({ url  : '/users'
  		            , type : 'GET'
									, headers : { accept : 'application/json' }
  		     			  }).done(function(users) {
											// remve owner and existing collaborators
											var owner = $("input[name=owner]").val();
											cur_users = users;
											removeFirst(owner , cur_users);
											$("input[name='collaborators[]']").map(function() {
													removeFirst($(this).val(), cur_users);
											});
											process(cur_users);
  					     }).fail(function() {
									 // close modal
									 $("#manageCollabs button[type=button].close").click();
									 // show error message
  					     	 $("#main-alert-msg").html(
  					     			"<i class=\"icon-warning-sign\"></i> "+
  					     			"<strong>Server error:</strong> Failed to add collaborator.");
  					     	 $("#main-alert").show();
						     });
				 }
		});

    $('#post-add-collaborator').change( function() {
				var new_c = $("#post-add-collaborator").val();
				if($.inArray(new_c, cur_users) >= 0) {
						$("#post-add-collaborator-btn").removeAttr("disabled");
				} else {
						$("#post-add-collaborator-btn").attr("disabled","");
				}
		});

		$('#post-add-collaborator-help').popover({ html      : false
																						 , placement : 'bottom'
																						 , trigger   : 'hover'});

		$("#post-add-collaborator-btn").click( function () {
				var new_c = $("#post-add-collaborator").val();
				if(new_c.length>0 && $.inArray(new_c, cur_users) >= 0) {
				  set_tags("#editPost");
					$("<input>", { type : 'hidden'
											 , name : 'collaborators[]'
											 , value : new_c }).appendTo($("#editPost"));
          $("#post-save-btn").click();
					$("<li>", { id : "collaborator-"+new_c
							      , html : '<a href="#">'+new_c+
											       '<span class="pull-right collaborator-remove"\
                                           data-collaborator="'+new_c+'">\
                                <i class="icon-trash"></i></span></a>'
					          }).appendTo($("#currentCollabs"));
					removeFirst(new_c, cur_users);
				}
		});

		$(".collaborator-remove").map(function() {
				$(this).click(function() {
						var c = $(this).data("collaborator");
						$("input[name='collaborators[]'][value="+c+"]").remove();
            $("#post-save-btn").click();
						$("#collaborator-"+c).remove();
				});
		});

  })();

	(function () { 
  	//
  	// Show
  	//
  
  	$(".raw-active-code").map( function() {
  			var mkController = function(div_id, id) {
					// create the [cog] Execute link
  				return $("<a>",
  					{ href : "#"
  					, click : function() {
								// button in result 
								var btn = '<button type="button" class="close" data-dismiss="alert">&times</button>';
  							// create code executing result <pre>:
  							var res_id = "result-"+id;
  							if($("#"+res_id).length == 0) {
  							  $("<pre>", { id: res_id }).appendTo($("#"+div_id));
  							}
  							$("#"+res_id).html(btn+"<i class=\"icon-repeat\"></i> Executing...")
  							             .attr("class","exec-result alert alert-info");
  							// ask parent to resize iframe
  							window.parent.postMessage("preview-resize","*");
  							// AJAX to execute code
  							$.ajax({ url:'/exec'
  										 , type: 'POST'
  										 , contentType: 'application/json'
  										 , data : JSON.stringify({ "id": id
  																						 , "lang": $("#"+id).data("lang")
  																						 , "source": $("#"+id).text()})
  										 }).done(function(data) {
  												 var result_class = "exec-result alert alert-error";
  												 if (data.code == 0) {
														 result_class = "exec-result alert alert-success";
  											   	 $("#"+res_id).html(btn+"<i class=\"icon-ok\"></i> ");
													 } else {
  											   	 $("#"+res_id).html(btn+"<i class=\"icon-remove\"></i> ");
													 }
  												 $("#"+res_id).attr("class",result_class);
  												 $("<b>", { text : data.result }).appendTo($("#"+res_id))
  												 window.parent.postMessage("preview-resize","*");
  										 }).fail(function(data) {
  												 $("#"+res_id).attr("class","exec-result alert alert-error");
  												 $("#"+res_id).html(btn+"<i class=\"icon-warning-sign\"></i> ");
  												 $("<strong>", { text : "Sorry, this seems like a server error."
													               }).appendTo($("#"+res_id))
											 });
  							return false;
  						}
						, class : "btn btn-inverse btn-mini pull-right"
  				  , html : "<i class=\"icon-cog icon-white\"></i>EXECUTE"})
  			};

				// ----------------------------------------------------------------
				var raw = $(this);
				var id = raw[0].id;
				var div_id = id.replace(/^raw-/,'');
				raw.wrap('<div class="active-code span12" id="'+div_id+'" />');
				//
				var mode = raw.data("lang");
				// code mirror treats C-like languages the same, let's
				// make sure we highlight C/C++ code
				switch(mode) {
					case "c"  : mode = "text/x-csrc"  ; break;
					case "cpp": mode = "text/x-c++src"; break;
				}
				//
						
				// create editor
			  var cm  = CodeMirror.fromTextArea(raw[0], { lineWrapping: true
	                                         	      , theme: "elegant"
	                                                , mode: mode
																									, lineNumbers: true
	                                                });

				// make sure text field (used to exec) is updated:
		    cm.on("change", function(inst,chObj) {
						raw.text(inst.getValue());
				});

				// Create active-code controller
  			var exec = mkController(div_id,id);
				$("<div>", { class : "active-controller-btn"
									 , html : exec }).appendTo($("#"+div_id));
		});


  })();

});
