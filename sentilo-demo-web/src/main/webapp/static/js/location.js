function toggleMobile(value) {
	
	var lat = $('#latitude');
	var lon = $('#longitude');
	
	if (value == 0) {
		lat.attr('disabled', false);
		lon.attr('disabled', false);
	} else {
		lat.attr('disabled', true);
		lon.attr('disabled', true);
	}
	$('#mobile').val(value);
}

function initializeMobile() {
	var mobile = $('#mobile');
	if (mobile.val() == '0' || mobile.val() == 'false') {
		$('#btnStatic').addClass('active');
		toggleMobile(0);
	} else {
		$('#btnMobile').addClass('active');
		toggleMobile(1);
	}
}

function initializeAddressPicker() {
	var addresspickerMap = $("#addresspicker" ).addresspicker({
		elements: {
			map:      "#addresspicker_map",
			lat:      "#latitude",
			lng:      "#longitude",
			locality: '#locality',
			country:  '#country'
		}
	});
	var gmarker = addresspickerMap.addresspicker( "marker");
	gmarker.setVisible(true);
	addresspickerMap.addresspicker( "updatePosition");
}

function initializeMap(latitude, longitude, name) {
	var map = $('#map_canvas');
	var coords = latitude + ',' + longitude; 
	
	var mapOptions = {
		center: coords,
		scrollwheel: false,
		mapTypeId: google.maps.MapTypeId.ROADMAP,
		zoom: 17
	};
	
	var marker = {
		position: coords,
		bounds: false
	};
	
	var infoWindow = {
		content: '<strong>' + name + '<strong><br/>' + latitude + ',' + longitude
	};

	map.gmap(mapOptions);
	map.gmap('addMarker', marker).click(function() {
		map.gmap('openInfoWindow', infoWindow, this);
	});
}
