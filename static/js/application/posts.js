$(document).ready(function() {

	//
	// All
	//
	$("a[data-toggle=tooltip]").tooltip();

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
				set_tags("#editPost");
  			$.ajax({ url  : '/posts'
  		         , type : 'PUT'
  		         , data : $("#editPost").serialize()}
  						).done(function() {
  						    if(do_preview) { refresh_preview(); }
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
  
  	$.map($(".active-code"), function(c_blk) {
  			var mkController = function(id) {
  				return $("<a>",
  					{ href : "#"
  					, click : function() {
  							// create result pre	
  							var res_id = "result-"+id;
  							if($("#"+res_id).length == 0) {
  							  $("<pre>", { id: res_id }).insertAfter($("#"+id));
  							}
  							$("#"+res_id).html("<i class=\"icon-chevron-right\"></i>"+
  													      " Executing...")
  							             .attr("class","alert-info");
  							// ask parent to resize iframe
  							window.parent.postMessage("preview-resize","*");
  							// execute code
  							$.ajax({ url:'/exec'
  										 , type: 'POST'
  										 , contentType: 'application/json'
  										 , data : JSON.stringify({ "id": id
  																						 , "lang": $("#raw-"+id).data("lang")
  																						 , "source": $("#raw-"+id).text()})
  										 }).done(function(data) {
  												 var result_class = "alert-error";
  												 if (data.code == 0) {
														 result_class = "alert-success";
  											   	 $("#"+res_id).html("<i class=\"icon-ok\"></i> ");
													 } else {
  											   	 $("#"+res_id).html("<i class=\"icon-remove\"></i> ");
													 }
  												 $("#"+res_id).attr("class",result_class);
  												 $("<b>", { text : data.result }).appendTo($("#"+res_id))
  												 window.parent.postMessage("preview-resize","*");
  										 }).fail(function(data) {
  												 $("#"+res_id).attr("class","alert-error");
  												 $("#"+res_id).html("<i class=\"icon-warning-sign\"></i> ");
  												 $("<b>", { text : "Sorry, this seems like a server error." }).appendTo($("#"+res_id))
											 });
												
								
  							return false;
  						}
  				  , html : "<i class=\"icon-cog\"></i>Execute"}).attr("class",
  																															"pull-right");
  			};
  			var ctrl = mkController(c_blk.id);
  			ctrl.appendTo(c_blk);
  	});
  })();

});
