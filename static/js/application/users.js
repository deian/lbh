$(document).ready(function() {

	//
	// All
	//
	$("a[data-toggle=tooltip]").tooltip();

	
	(function () { 
   	//
   	// New
   	//

		var error_class = $("#_id-group").attr("class");

		$("#newUser-submit-btn").click( function() {
  			$.ajax({ url  : '/users'
  		         , type : 'POST'
  		         , data : $("#newUser").serialize()}
  						).done(function() {
									window.location="/users/"+$("#_id").val();
  						  }
						  ).fail(function(data) {
								var obj = $.parseJSON(data.responseText);
								$("#_id-group").attr("class",error_class+" error");
								$("#_id-group-help").text(obj.error);
							});
		});

		$("#newUser-reset-btn").click( function() {
			 $("#_id-group").attr("class",error_class);
			 $("#_id-group-help").text("");
		});

	})();


});
