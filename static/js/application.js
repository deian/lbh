$(window).load(function() {
	var cur_user = null;
	if($.cookie("_hails_user") &&
		  $.cookie("_hails_user") !==  "deleted") {
		  cur_user = $.cookie("_hails_user");
	}
  navigator.id.watch({
    loggedInUser: cur_user
  , onlogin: function(assertion) {
      $.ajax({ type: 'POST'
             , url: '_hails_auth'
             , headers: {"X-Hails-Persona-Login": true}
             , data: assertion
             , success: function(res, status, xhr) {
  							 var redir = $.cookie("redirect_to");
  							 if(redir && redir !== "") {
  								 window.location= $.cookie("redirect_to");
  								 $.cookie("redirect_to","");
  							 } else {
  							   window.location.reload();
  							 }
  					   }
  					 , error: function(xhr, status, err) {
  							 $("#main-alert-msg").html(
  									"<i class=\"icon-warning-sign\"></i> "+
  									"<strong>Server error:</strong> Login failed.");
  							 $("#main-alert").show();
  					   }
             });
    }
  , onlogout: function() {
      $.ajax({ type: 'POST'
             , url: '/_hails_auth'
             , headers: {"X-Hails-Persona-Logout": true}
             , success: function(res, status, xhr) {
  							 window.location = "/";
  					   }
             , error: function(xhr, status, err) {
  							 $("#main-alert-msg").html(
  									"<i class=\"icon-warning-sign\"></i> "+
  									"<strong>Server error:</strong> Logout failed.");
  							 $("#main-alert").show();
  					   }
             });
    }
  });
});


$(document).ready(function() {
  $("#login").click(function () {
      navigator.id.request({ siteName : 'Learn By Hacking'
  								         , returnTo : '/users/new'});
  });
  $("#logout").click(function () {
  		navigator.id.logout();
  });

// enable alerts
  $(".alert").alert();

  // enable tooltips
	$("a[data-toggle=tooltip]").tooltip();

});

