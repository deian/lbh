$(document).ready(function() {
		// set the height of the topic elements
		var height = 0;
		$(".main-topic > .caption").map(function() {
				height = Math.max(height, $(this).height());
		});
		$(".main-topic > .caption").height(height);
});
