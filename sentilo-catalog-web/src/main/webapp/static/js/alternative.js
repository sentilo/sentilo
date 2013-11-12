var alternativeComponentId = 'URBIOTICA.12259';
var alternativeZoom = 21;
var alternativeComponentMarker = null;
var alternativeTransitionTimeout = 6000;

var alternativeLat = 41.3862329648053;
var alternativeLng = 2.1824466681160857;

$(document).ready(function() {
	if (window.alternative) {
		if (window.testPage) {
			if (window.testPage == 'home') {
				startAlternativeHomeScript();	
			} else if (window.testPage == 'map') {
				startAlternativeMapScript();	
			} else if (window.testPage == 'detail') {
				startAlternativeDetailScript();	
			}
		}
	}
});

/**
 * Test script for home page (?alternative=true&testPage=home)
 */
function startAlternativeHomeScript() {
	
}

/**
 * Test script for map page (?alternative=true&testPage=map)
 */
function startAlternativeMapScript() {
	setTimeout(function() {
		google.maps.event.addListenerOnce(map, 'tilesloaded', function() {
			setTimeout(function() {
				// Centrar mapa
				alternativeMapCenter();
				setTimeout(function() {
					// Zoom al component
					alternativeCenterZoom();
					setTimeout(function() {
						// Click al marcador per a obrir el infowindow
						markerClick();
						setTimeout(function() {
							// Click al detall del marcador i 
							// redireccionament a la pantalla de detalls
							infowindowClick();
						}, alternativeTransitionTimeout);
					}, alternativeTransitionTimeout);
				}, alternativeTransitionTimeout);
			}, alternativeTransitionTimeout);
		});
	}, 100);
}

/**
 * Test script for component details page (?alternative=true&testPage=detail)
 */
function startAlternativeDetailScript() {
	setTimeout(function() {
		setTimeout(function() {
			clickCloseDetails();
		}, alternativeTransitionTimeout);
	}, 100);
}

function alternativeMapCenter() {
	map.setCenter(new google.maps.LatLng(alternativeLat,alternativeLng));	
}

function alternativeCenterZoom() {
	map.setZoom(alternativeZoom);
}

function markerClick() {
	console.debug('antes');
	
	for(var m=0;m<markersArray.length;m++) {
		if (markersArray[m].componentId == alternativeComponentId) {
			google.maps.event.trigger(markersArray[m], 'click');
		}
	}
	
	console.debug('despues');
}

function infowindowClick() {
	window.location.href = window.contextPath + '/component/' + alternativeComponentId + '/detail?alternative=true&testPage=detail';
}

function clickCloseDetails() {
	window.location.href = window.contextPath + '/component/map?alternative=true&testPage=map';	
}
