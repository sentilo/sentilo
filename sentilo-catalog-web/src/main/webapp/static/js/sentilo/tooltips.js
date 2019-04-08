$(document).ready(function() {

	// Foreach field that has tooltip, attach a (?) icon with popover
	$('[tooltip]').each(function() {
	
		var name = $(this).attr('name');
		var header = $('label[for="'+name+'"]').html() || name;
		var tooltip = $(this).attr('tooltip');

		var icon = 'icon-question-sign';
		
		if (name !== '' && header !== '' && tooltip !== '') {
			var tooltipIcon = $('<i class="'+icon+' tooltip-icon" title="'+header+'" data-toggle="popover" data-trigger="hover" data-content="'+tooltip+'"></i>');
			$(this).after(tooltipIcon);
			tooltipIcon.popover();	
		}
		
	});
	
});
 