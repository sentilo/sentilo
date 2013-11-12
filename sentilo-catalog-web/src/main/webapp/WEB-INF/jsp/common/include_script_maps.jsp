<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<script type="text/javascript" src="http://maps.google.com/maps/api/js?sensor=true&libraries=places&language=en"></script>
<spring:url value="/static/js/gmap3.min.js" var="gmap3JS"/>
<script type="text/javascript" src="${gmap3JS}"></script>
<spring:url value="/static/js/infobox.js" var="infoboxJS"/>
<script type="text/javascript" src="${infoboxJS}"></script>

<spring:url value="/component/map/json?componentType=" var="componentMapJSON"/>
<spring:url value="/admin/sensortypes/icons/json" var="sensorTypeIconsJSON"/>

<spring:url value="/static/img/spot.png" var="imgSpot" />

<spring:url value="/static/img/pind" var="iconPrefix" />

<spring:url value="/static/img/pins" var="dropdownIconPrefix" />
<spring:url value="/static/img/pind6.png" var="defaultIcon" />

<script type="text/javascript">
// var defaultMapCenter = [ 41.385064, 2.173404 ];
var defaultMapCenter = [ 41.4001221, 2.172839 ];
var defaultZoomLevel = 14;
var defaultInputLocationZoomLevel = 17;

var geocoder;
var map;
var marker;
var infowindow;

var requestcount = 0;
var requestTimer;
var zoomChanged = false;
var markersArray = [];

var result_map = {}; 

var boxOptions = {
		disableAutoPan: false,
		maxWidth: 0,
		pixelOffset: new google.maps.Size(10, -60),
		zIndex: null,
		alignBottom: true,
		boxStyle: {
			background: "transparent",
			color: "#ffffff",
			opacity: 0.9,
		//	width: "160px",
			padding: "4px",
			borderRadius: "4px",
			fontSize: "11px",
			whiteSpace: "nowrap",
			textAlign: "center"
		},
		closeBoxURL: "",
		infoBoxClearance: new google.maps.Size(1, 1),
		isHidden: false,
		pane: "floatPane",
		enableEventPropagation: false
}; 


function showMapControls() {
	$('#map_controls').show();
};

function hideMapControls() {
	$('#map_controls').hide();
};

function initializeStreetView(map) {
  
	var sv = map.getStreetView();
	var svChangeVisibilityCallback = function () {
        if (sv.getVisible()) {
            hideMapControls();
        } else {
            showMapControls();
        }
    };
    google.maps.event.addListener(sv, 'visible_changed', svChangeVisibilityCallback);
};

function initializeMapControls() {

    var map_pos = $('#map_canvas_1').offset();
    $('#map_controls').css({
    	position:'absolute',
    	top: map_pos.top + 10,
    	left: map_pos.left + 50
    });
};

function initializeGeoCoder() {
    geocoder = new google.maps.Geocoder();
}

function initialize() {

    map = $('#map_canvas_1').gmap3('get');

    initializeGeoCoder();

    marker = new google.maps.Marker({
        map: map,
        draggable: true,
        animation: google.maps.Animation.DROP,
    });

    var image = new google.maps.MarkerImage(
        '${imgSpot}',
        new google.maps.Size(22, 22),
        new google.maps.Point(0, 0),
        new google.maps.Point(11, 11),
        new google.maps.Size(22, 22));
    
    marker.setIcon(image);
    
    initializeStreetView(map);
};

function getPOIImage(data) {
	
	var img = data && data.icon ? '${iconPrefix}' + data.icon + '.png' : '${defaultIcon}';
	
	return new google.maps.MarkerImage(
			img,
			new google.maps.Size(45, 45),
	        new google.maps.Point(0, 0),
	        new google.maps.Point(34, 34),
	        new google.maps.Size(45, 45));
}

function smap_init(loadOnInit) {

	function initializeTypeAheadResults() {
		labels = [];
		result_map = {};
	};

	$(window).resize(function() {
    	initializeMapControls();
    });
    initializeMapControls();
    
    var address = $('#address');
    var locate = $('#locate');
    
    address.tooltip({
        placement: 'bottom',
        title: '<spring:message code="empty.list"/>',
        trigger: 'manual'
    });

    initialize();

    locate.tooltip({
        placement: 'right',
        title: '<spring:message code="locate.me"/>',
        trigger: 'hover'
    });

    locate.click(function () {
        if (navigator.geolocation) {
            navigator.geolocation.getCurrentPosition(
            function (position) {
                var location = new google.maps.LatLng(position.coords.latitude, position.coords.longitude);
                map.setZoom(defaultZoomLevel);
                marker.setPosition(location);
                map.setCenter(location);
                updateMarkers(location);
            },
            function (error) {
            },
            {
                timeout: 10000
            });
        }
    });
    
	address.typeahead({
		source: function (query, process) {
			initializeTypeAheadResults();
			function storeTypeAheadResults(results, status) {
				if (status == google.maps.GeocoderStatus.OK) {
					address.tooltip('hide');
					$.each(results, function (index, item) {
						result_map[item.formatted_address] = item;
						labels.push(item.formatted_address);
					});
					return process(labels);
				} else if (status == google.maps.GeocoderStatus.ZERO_RESULTS) {
					address.tooltip('show');
				}
			}
			geocoder.geocode({ 'address': query }, storeTypeAheadResults);
		},
		updater: function (item) {
			var location = new google.maps.LatLng(result_map[item].geometry.location.lat(), result_map[item].geometry.location.lng());
			marker.setPosition(location);
			map.setCenter(location);
			map.setZoom(defaultZoomLevel);
			updateMarkers(location);
		},
	});

    address.keypress(function (event) {
        switch (event.keyCode) {
            case 9: // tab
            case 13: // enter
                event.preventDefault();
                manualSearch();
                break;
        }
    });

    $('#search').click(function () {
        manualSearch();
    });
    
    function updateMyPosition(result) {
        var location = new google.maps.LatLng(result.geometry.location.lat(), result.geometry.location.lng());
        map.setZoom(defaultZoomLevel);

        var radius = 50000 / Math.pow(2, (map.getZoom() - 10));
        clearOverlay();
        marker.setPosition(location);
        map.setCenter(location);
        getPlaces(location, radius);
        address.val(result.formatted_address);
    };

    function manualSearch() {
        geocoder.geocode({
            'address': address.val()
        }, function (results, status) {
            if (status == google.maps.GeocoderStatus.OK) {
                address.tooltip('hide');
                if (results[0]) {
                	updateMyPosition(results[0]);
                }
            }
            if (status == google.maps.GeocoderStatus.ZERO_RESULTS) {
                address.tooltip('show');
            }
        });
    };

    function getPlaces(location, radius) {
    	
    	var componentType = $('#selectedComponentType').val();
    	jsonGET('${componentMapJSON}' + componentType, [] , function(data) {
    		clearOverlay();
        	for(var i = 0; i < data.length; i++) {
            	createComponentMarker(data[i]);
        	}
    	});
    };
    
    function closeInfoWindow() {
    	if (infowindow) {
    		infowindow.close(map, this);
    	}
    }
    
    function retrieveLastObservations(data, observationDiv) {
    	var url = '<spring:url value="/component/"/>' + data.component.id + '/lastOb/';
		jsonGET(url, [], function(observations) {
			html = '';
			for(var i = 0; i < observations.length; i++) {
				var observation = observations[i];
				if (observation.found) {
					// TODO Traducir
					if (observation.dataType === 'BOOLEAN') {
						observation.value = eval(observation.value) ? '' : 'No';
					}
					
					html += '<h1>' + observation.value + ' ' + observation.unit + '</h1><span class="label label-info">' + observation.sensorType + '</span>';
				} else {
					html += '<h1><spring:message code="component.sensor.observation.not.found"/> ' + observation.unit + '</h1><span class="label label-info">' + observation.sensorType + '</span>';
				}
			}
			var ph = $('#' + observationDiv);
			ph.append(html);
    	});
    }
    
    function fillInfoWindow(data) {
    	var content = '<div class="infobox">';

    	var observationDiv = '' + new Date().getTime();
		content += '<div id="' + observationDiv + '"></div><br/>';
		var detailUrl = '<spring:url value="/component/" />' + data.component.id + '/detail';
		if(window.alternative) detailUrl = detailUrl+"?alternative=true"; 
		
		content += '<a href="' + detailUrl + '">Veure detalls</a>';
    	content += '</div>';    	
    	
		retrieveLastObservations(data, observationDiv);
    	infowindow.setContent(content);
    }

    function createComponentMarker(data) {
    	var poi = new google.maps.Marker({
			componentId: data.component.id,
            map: map,
            position : new google.maps.LatLng(data.component.location.latitude, data.component.location.longitude)
        });
    	
        poi.setIcon(getPOIImage(data));
        markersArray.push(poi);
        closeInfoWindow();
        infowindow = new InfoBox(boxOptions);

        google.maps.event.addListener(map, 'click', function() {
			closeInfoWindow();
        });

        google.maps.event.addListener(poi, 'click', function() {
			closeInfoWindow();
        	fillInfoWindow(data);
           	infowindow.open(map, this);
        });
    };

    function clearOverlay() {
        if (markersArray) {
            for (i in markersArray) {
                markersArray[i].setMap(null);
            }
            markersArray = [];
        }
    };
    
    function geocodeCenterAndUpdatePOI(center) {
    	var radius = 50000;
    	var success = function (results, status) {
            if (status == google.maps.GeocoderStatus.OK && results[0]) {
                address.val(results[0].formatted_address);
                getPlaces(center, radius);
            }
    	};
    	geocoder.geocode({'latLng': center}, success);
    };

    function updateMarkers(center) {
        if (map.getZoom() > 9) {
        	geocodeCenterAndUpdatePOI(center);
        } else {
            clearOverlay();
        }
    };

    google.maps.event.addListener(marker, 'dragend', function () {
        updateMarkers(new google.maps.LatLng(marker.getPosition().lat(), marker.getPosition().lng()));
    });

    google.maps.event.addListener(map, 'zoom_changed', function () {
        zoomChanged = true;
    });

    google.maps.event.addListener(map, 'idle', function () {
        if (marker.getPosition() == undefined) {
            updateMarkers(map.getCenter());
        } else if (zoomChanged) {
            updateMarkers(new google.maps.LatLng(marker.getPosition().lat(), marker.getPosition().lng()));
        }
        zoomChanged = false;
    });
    
    if (loadOnInit) {
    	updateMarkers(map.getCenter());
    }
};

function initializeMap(latitude, longitude, name, icon) {
	var map = $('#map_canvas');

	
	var poiImage = icon ? getPOIImage({ 'icon': icon }) : getPOIImage();
	
	var poi = {
		latLng : [ latitude, longitude ],
		options : { 
			icon : poiImage,
		},
	};

	var options = {
		center : new google.maps.LatLng(latitude, longitude),
		zoom : defaultZoomLevel,
		mapTypeId : google.maps.MapTypeId.ROADMAP,
		mapTypeControl : true,
		panControl:false,
		mapTypeControlOptions : {
			style : google.maps.MapTypeControlStyle.DROPDOWN_MENU
		},
		navigationControl : true,
		scrollwheel : true,
		streetViewControl : true
	};

	map.gmap3({
		marker : poi,
		map : {
			options : options
		}
	});
};

function initializeHomeMap(selector) {

	var visibilityOn = {
		"visibility" : "on"
	};

	var visibilityOff = {
		"visibility" : "off"
	};

	var colorBlue = {
		"color" : "#0088ff"
	};

	var lightness = {
		"lightness" : 10
	};
	
	var saturation = {
		"saturation" : -49
	};

	$(selector).gmap3({
		map : {
			options : {
				center : defaultMapCenter,
				zoom : defaultZoomLevel,
				mapTypeId : google.maps.MapTypeId.ROADMAP,
				mapTypeControl : false,
				navigationControl : false,
				scrollwheel : false,
				streetViewControl : false,
				//JG: Añadido para deshabilitar los controles
				zoomControl:false,
				panControl:false,
				//JG: ends
				styles : [ {
					"stylers" : [ visibilityOn, colorBlue, saturation ]
				}, {
					"featureType" : "road.highway",
					"stylers" : [ colorBlue, saturation, lightness ]
				}, {
					"featureType" : "road.arterial",
					"stylers" : [ colorBlue, saturation, lightness, visibilityOn ]
				}, {
					"featureType" : "road.local",
					"stylers" : [ colorBlue, saturation, lightness ]
				}, {
					"featureType" : "poi",
					"stylers" : [ visibilityOff ]
				}, {
					"elementType" : "labels",
					"stylers" : [ visibilityOff ]
				} ]
			}
		}
	});
};

function initializeComponentMap(selector) {
    $(selector).gmap3({
        map: {
            options: {
                center: defaultMapCenter,
                zoom: defaultZoomLevel,
                mapTypeId: google.maps.MapTypeId.STREET,
                mapTypeControl: true,
                navigationControl: false,
                panControl:false,
                scrollwheel: true,
                streetViewControl: true,
                styles: [{
                    "featureType": "poi",
                    "stylers": [{
                        "visibility": "simplified"
                    }]
                }, {
                    "elementType": "labels.icon",
                    "stylers": [{
                        "visibility": "off"
                    }]
                }]
            }
        }
    });

    smap_init();
};

// **************************************************************
// Input location map
// **************************************************************

function clearOverlay() {
    if (markersArray) {
        for (i in markersArray) {
            markersArray[i].setMap(null);
        }
        markersArray = [];
    }
};

function geocodeAddress(location) {
	var success = function (results, status) {
        if (status == google.maps.GeocoderStatus.OK && results[0]) {
            $('#locationaddress').val(results[0].formatted_address);
        }
	};
	geocoder.geocode({'latLng': location }, success);

}

function updateSearchedPosition(location) {
	if (marker) {
		marker.setMap(null);
	}
	
	var markerOptions = {
		map: map,
		draggable: true,
		animation: google.maps.Animation.DROP,
	};
	
    marker = new google.maps.Marker({
    	options: markerOptions,
    });

    google.maps.event.addListener(marker, 'dragend', function () {
    	updateSearchedPosition(marker.getPosition());
    	geocodeAddress(marker.getPosition());
    });    
    
    var image = new google.maps.MarkerImage(
            '${imgSpot}',
            new google.maps.Size(22, 22),
            new google.maps.Point(0, 0),
            new google.maps.Point(11, 11),
            new google.maps.Size(22, 22));
        
    marker.setIcon(image);
    
    map.setZoom(defaultInputLocationZoomLevel);
    clearOverlay();
    marker.setPosition(location);
    map.setCenter(location);
    
	$('#latitude').val(location.lat());
	$('#longitude').val(location.lng())

};

function initializeAddressTypeAhead(address) {
	
    map = $('#input_location_map_canvas').gmap3('get');
    
    function manualSearch() {
    	if (address.val()) {
    		geocoder.geocode({ 'address': address.val() },  function delegate(results, status) {
    			if (status == google.maps.GeocoderStatus.OK && results[0]) {
    		        var location = new google.maps.LatLng(results[0].geometry.location.lat(), results[0].geometry.location.lng());
    		        updateSearchedPosition(location);
    			}
    	    });	
    	}
    };
	
	function initializeTypeAheadResults() {
		labels = [];
		result_map = {};
	}
	
    initializeGeoCoder();

	address.typeahead({
		source: function (query, process) {
			initializeTypeAheadResults();
			function storeTypeAheadResults(results, status) {
				if (status == google.maps.GeocoderStatus.OK) {
					$.each(results, function (index, item) {
						result_map[item.formatted_address] = item;
						labels.push(item.formatted_address);
					});
					return process(labels);
				}
			}
			geocoder.geocode({ 'address': query }, storeTypeAheadResults);
		},
		updater: function (item) {
			var location = new google.maps.LatLng(result_map[item].geometry.location.lat(), result_map[item].geometry.location.lng());
			map.setCenter(location);
			map.setZoom(defaultInputLocationZoomLevel);
			updateSearchedPosition(location);
		    return item;
		},
	});

    address.keypress(function (event) {
        switch (event.keyCode) {
            case 9: // tab
            case 13: // enter
                event.preventDefault();
                manualSearch();
                break;
        }
    });
}

function initializeInputLocationMap() {
	
    $('#input_location_map_canvas').gmap3({
        map: {
            options: {
                center: defaultMapCenter,
                zoom: defaultInputLocationZoomLevel,
                mapTypeId: google.maps.MapTypeId.STREET,
                mapTypeControl: true,
                navigationControl: false,
                scrollwheel: true,
                streetViewControl: true,
            }
        }
    });
    map = $('#input_location_map_canvas').gmap3('get');
    
    
    var locate = $('#locate');
    locate.tooltip({
        placement: 'right',
        title: '<spring:message code="locate.me"/>',
        trigger: 'hover'
    });

    locate.click(function () {
        if (navigator.geolocation) {
            navigator.geolocation.getCurrentPosition(
            function (position) {
	            var location = new google.maps.LatLng(position.coords.latitude, position.coords.longitude);
	    		geocodeAddress(location);
	    		updateSearchedPosition(location);
            },
            function (error) {
            },
            {
                timeout: 10000
            });
        }
    });
    
    initializeAddressTypeAhead($('#locationaddress'));
}
</script>