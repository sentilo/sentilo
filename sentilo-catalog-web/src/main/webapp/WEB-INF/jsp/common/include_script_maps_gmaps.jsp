<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<spring:eval expression="@catalogConfigProperties.getProperty('catalog.map.google.key')" var="apiKey"/>
<c:choose>
	<c:when test="${not empty apiKey}">
		<c:set var="mapsAPIUrl" value="${currentSchema}://maps.google.com/maps/api/js?v=3.41&libraries=places&language=en&key=${apiKey}"/>
	</c:when>
	<c:otherwise>
		<c:set var="mapsAPIUrl" value="${currentSchema}://maps.google.com/maps/api/js?v=3.41&libraries=places&language=en"/>
	</c:otherwise>
</c:choose>


<script type="text/javascript" src="${mapsAPIUrl}"></script>
<spring:url value="/static/js/gmap3.min.js" var="gmap3JS" />
<script type="text/javascript" src="${gmap3JS}"></script>
<spring:url value="/static/js/infobox.js" var="infoboxJS" />
<script type="text/javascript" src="${infoboxJS}"></script>
<spring:url value="/static/js/markerclusterer_packed.js" var="marketclusterJS" />
<script type="text/javascript" src="${marketclusterJS}"></script>
<spring:url value="/static/js/oms.min.js" var="omsJS" />
<script type="text/javascript" src="${omsJS}"></script>

<script type="text/javascript">
	// Default infobox pixel offsets for detailed view cases
	var defaultInfoboxPixelOffset_Y_A = -265;
	var defaultInfoboxPixelOffset_Y_B = -330;
	var defaultInfoboxPixelOffset_Y_C = -390;

	var mc;
	var oms;
	
	var lineSymbol = {
	  path: 'M 0,-0.5 0,0.5',
	  scale: 6,
	  strokeWeight: 3,
	  strokeColor: '#FFFFFF'
	};
			
	var polyOptions = {		
		icons: [{
		  icon: lineSymbol,
		  offset: '100%'
		}],				
		strokeColor: "#25adee",				
		strokeOpacity: 0.5
	};
					
	var zoomChanged = false;
	var result_map = {};

	var boxOptions = {
			maxWidth: 0,
			pixelOffset: new google.maps.Size(-140, -420), // offset for infobox without scroll: default
			zIndex: null,
			alignBottom: false,
			boxStyle: {
				background: "transparent",
				color: "#ffffff",
				opacity: 0.9,
				width: "600px",
				padding: "4px",
				borderRadius: "4px",
				fontSize: "11px",
				whiteSpace: "nowrap",
				textAlign: "center"
			},
			closeBoxURL: "",
			infoBoxClearance: new google.maps.Size(50, 50),
			isHidden: false,
			pane: "floatPane",
			disableAutoPan:false, //Infobox must be displayed entirely within the map's visible area
			enableEventPropagation: false
	};

	var clusterStyles = [[{
        url: clusterImgsPath+'poi-group.png',
        width: 53,
        height: 53,
		anchorText: [-12, -12],
        textColor: '#ffffff'
      }, {
        url: clusterImgsPath+'poi-group2.png',
        width: 56,
        height: 56,
		anchorText: [-11, -11],
        textColor: '#ffffff'        
      }, {
        url: clusterImgsPath+'poi-group3.png',
        width: 66,
        height: 66,
		anchorText: [-14, -14],
        textColor: '#ffffff'        
      }, {
        url: clusterImgsPath+'poi-group4.png',
        width: 78,
        height: 78,
        anchorText: [-18, -18],        
        textColor: '#ffffff'        
      }, {
        url: clusterImgsPath+'poi-group5.png',
        width: 90,
        height: 90,
        anchorText: [-18, -18],        
        textColor: '#ffffff'        
      }]];
	
	function setInfowindowMustBeClosed(close) {
		infowindowMustBeClosed = close;
	}
	
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
	    	top: map_pos.top + 10
	    });
	};
	
	function initializeGeoCoder() {
	    geocoder = new google.maps.Geocoder();
	};
	
	function initialize() {
					
	    map = $('#map_canvas_1').gmap3('get');
	
	    initializeGeoCoder();
	
	    marker = new google.maps.Marker({
	        map: map,
	        draggable: true,
	        animation: google.maps.Animation.DROP,
	    });
	    
	    var image = {
		    url: '${imgSpot}',
		    size: new google.maps.Size(22, 22),
		    origin: new google.maps.Point(0, 0),
		    anchor: new google.maps.Point(0, 22)
		};
	    
	    marker.setIcon(image);	    	    
	    
	    initializeStreetView(map);
	
		// La funcion initialize se llama tanto al recargar la página como al hacer un filtrado por tipo de componente
		// Esto implica que en caso de existir un mc anterior, debemos o bien eliminar los markers que ya tiene fijados
		// antes de crear un MarkerClustered nuevo, o bien reutilizar el ya existente.
	    if(!infowindow){
	    	infowindow = new InfoBox(boxOptions);
		}
	    
	    if(!mc){			
	    	mc = new MarkerClusterer(map,[],{ maxZoom: defaultMaxZoomCluster , styles: clusterStyles[0]});	    	
		}	

		if(!oms){				    	
	    	oms = new OverlappingMarkerSpiderfier(map,{markersWontMove: true, markersWontHide: true, keepSpiderfied: true, legWeight: 0});	    	    	 
		}

		google.maps.event.addListener(map, 'click', function(e) {	
			if (e.placeId) {
				// Since we don't want show Google Maps Place Poi Infowindows
				// Lets stop the event!
				e.stop();
			}
			
			closeInfoWindow();
			
			if (isModalLayerVisible()) {
				hideModalLayer();	
			}
        });	
		
		// These two controls change map options to turn off panning/zooming when the mouse enters the infobox
		// and so the infobox scrollbar could work. 
		$(document).delegate("div#infobox", "mouseenter", function() {
		    map.setOptions({
		       draggable: false,
		       scrollwheel: false
		    });			    
		});

		$(document).delegate("div#infobox", "mouseleave", function() {
		    map.setOptions({
		        draggable: true,
		        scrollwheel: true
		    });			   
		});
	}

	function createComponentMarker(component) {
		const position = new google.maps.LatLng(component.centroid.latitude, component.centroid.longitude);

		var poi = new google.maps.Marker({
            map: (component.options?null:map),
			position: position
        });

		poi.options = {
			id: component.id,
			type: component.type,
			iconName: component.icon,
			title: component.icon
		};

		poi.setPosition(position);
        poi.setIcon(getPOIImage(component));

        if(component.route){
        	poi.options.route = component.route;
        }

		return poi;
    };

    function getPOIImage(data) {		
		return {
		    url: data && data.icon ? '${iconsPath}/' + data.icon + '-poi.png' : '${defaultIcon}',
		    size: new google.maps.Size(28, 30),
		    origin: new google.maps.Point(0, 0),
		    anchor: new google.maps.Point(0, 30)
		};
	};
			
    function printMobileComponent(poi){
    	if(poi.options.route){
			var latLngPoints = [];
											
    		for(var i = 0; i < poi.options.route.length; i++) {
            	var location = poi.options.route[i].location;
				var latLng = new google.maps.LatLng(location.latitude, location.longitude);	            						
            	
				var polylineMarker = new google.maps.Marker({
            		position: latLng,
            		icon: getMobilePOIImage(i, poi),	            		
            		map: map
            	});
				polylineMarker.options = {
					id: poi.options.id,
					iconName: poi.options.iconName,
					number: i,
					routeMarker: true,
					type: poi.options.type,
					to:poi.options.route[i].toTime,
					from:poi.options.route[i].fromTime
				}
				polylineMarkers.push(polylineMarker);	
			    addPolylineMarkerListener(polylineMarker);
			    latLngPoints.push(latLng);				    
    		}	                    	    														
				    		
    		var colors = ["#00C8A9"];
    		
    		for(var i = 0; i < latLngPoints.length - 1; i++){
				polylines[polylines.length] = new google.maps.Polyline({
    			    path: [latLngPoints[i], latLngPoints[i+1]],
    			    strokeColor: colors[i%colors.length],
    			    strokeOpacity: 0.5,
    			    strokeWeight: 4,
    			    map: map
    			});
    		}	    			    			    						
    	}	    	
    };
    
    function addPolylineMarkerListener(polylineMarker){
		google.maps.event.addListener(polylineMarker,'click', function(event) {
			closeInfoWindow();															
			fillInfoWindow(polylineMarker, event);						    			    
		});
	};
 	
    function getMobilePOIImage(i, poi) {	
    	var total = poi.options.route.length;
    	var mobileEndIcon = '${iconsPath}/' + poi.options.iconName + '-poi.png';
    	var mobileBeginningIcon = '${iconsPath}/' + 'mobile-poi.png';
    	var imgUrl = i == total-1 ? mobileEndIcon : mobileBeginningIcon;
		var size = i == total-1 ? 25 : 12;
		var anchorx = i == total-1 ? 0 : 7;
		var anchory = i == total-1 ? 25 : 7;
		
		var image = {
			  url: imgUrl,
			  size: new google.maps.Size(size,size),
			  origin: new google.maps.Point(0, 0),
			  anchor: new google.maps.Point(anchorx, anchory) ,
			  scaledSize: new google.maps.Size(size, size)
		};
		
		return image;
	}
	
	function buildPopup(content, poi) {
		if(!infowindow){
			infowindow = new InfoBox(boxOptions);
		}

		infowindow.setContent(content);
		infowindow.open(map, poi);
	}

    function fillInfoWindowPixelOffset(elementsCount, poi, boxOptions) {
    	var numRows = parseInt((elementsCount / 2) + (elementsCount%2));
    	var pixelOffset_x = (poi.multiCoordinates) ? -300 : -290;
		var pixelOffset_y = -290;
		
		if (numRows <= 1) {
			pixelOffset_y = defaultInfoboxPixelOffset_Y_A;
		} else if (numRows == 2) {
			pixelOffset_y = defaultInfoboxPixelOffset_Y_B;
		} else if (numRows >= 3) {
			pixelOffset_y = defaultInfoboxPixelOffset_Y_C;
		}
		
		if (poi.routeMarker) {
			var iconSize = poi.icon.size;
			if (iconSize.width === 12) {
				pixelOffset_x = pixelOffset_x - 15;
				pixelOffset_y = pixelOffset_y + iconSize.width;
			}
		}
		
		if (poi.multiCoordinates) {
			pixelOffset_y = pixelOffset_y + 25;
		}
		
		boxOptions.pixelOffset = new google.maps.Size(pixelOffset_x, pixelOffset_y);
    }
        
    
    function initClusterElements(){			
    	// Can only be one listener registered and associated with the click event, therefore, before registering
        // a new listener, we must do a call to remove all previous listeners on the specified event.
    	oms.clearListeners('click');
    	
    	oms.addListener('click', function(poi, event) {

    		infowindowMustBeClosed = true;
    		
    		if (infowindow && isModalLayerVisible()) {
    			updateModalLayer(poi.options.id);
    		}
    		
			closeInfoWindow();							
			poiClicked = true;		    	
	    	fillInfoWindow(poi, event);
   		});

		mc.repaint();
	};
	
	
    // Init smap_init function
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
	        title: emptyListMsg,
	        trigger: 'manual'
	    });
	
	    initialize();
	
	    locate.tooltip({
	        placement: 'right',
	        title: locateMeMsg,
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
	        marker.setPosition(location);
	        map.setCenter(location);
	        getPlaces();
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
		
		    	
	    google.maps.event.addListener(map, 'zoom_changed', function () {	    	
	        zoomChanged = true;
	    });
	
	    google.maps.event.addListener(map, 'idle', function () {
	    	infowindowMustBeClosed = false;
	        if(!poiClicked){
		    	if(marker.getPosition() == undefined) {
					updateMarkers(map.getCenter(),true);
				} else if (zoomChanged) {
					updateMarkers(new google.maps.LatLng(marker.getPosition().lat(), marker.getPosition().lng()));
				}
	        }
			zoomChanged = false;
			poiClicked = false;
		});
	    	
		if(loadOnInit) {
			updateMarkers(map.getCenter(), true);
		}
		
	};
	// End smap_init function
	

	/**
	 * Function used by the component detail page to display it on map (public and admin page).
	 * @param latitude: map center latitude
	 * @param longitude: map center longitude
	 * @param coordinates: component coordinates array where each element has the format latK lngK. 
	 * Its length, and if it is closed, dictates how to display the component (polygon, polyline or marker) 
	 * @param icon: icon to use when coordinates represents a POI
	 */
	function initializeMap(mapOptions) {
		var map = $('#map_canvas');

		var options = {
			center : new google.maps.LatLng(mapOptions.latitude, mapOptions.longitude),
			zoom : defaultZoomLevel,
			mapTypeId : google.maps.MapTypeId.ROADMAP,
			mapTypeControl : true,
			panControl : false,
			mapTypeControlOptions : {
				style : google.maps.MapTypeControlStyle.DROPDOWN_MENU
			},
			navigationControl : true,
			scrollwheel : true,
			fullscreenControl : false,
			streetViewControl : true,
			styles: [{
				"featureType": "poi",
				"stylers": [{ "visibility": "simplified" }]
			}]
		};
	
		addGeometricElementToMap(map, mapOptions.latitude, mapOptions.longitude, mapOptions.coordinates, mapOptions.icon, options);
		
	};
	
	function addGeometricElementToMap(map, latitude, longitude, coordinates, icon, options){
		// If coordinates length equals to 1, it represents a POI
		// Otherwise it is a polyline or polygon 
		
		var mapOptions = {
			map : {	options : options }	
		};
		
		var polyOptions = {			
		  strokeColor: "#0000FF",
	      strokeOpacity: 0.8,
	      strokeWeight: 2,
	      fillColor: "#0000FF",
	      fillOpacity: 0.35			
		};
		
		if(coordinates.length == 1){					
			var poiImage = icon ? getPOIImage({'icon' : icon}) : getPOIImage();	
			var element = {
				latLng : [ latitude, longitude ],
				options : {	icon : poiImage }
			};
	
			$.extend( mapOptions, {	marker : element} );
			
		}else{			          				
			var elementPath = [];
			for (var i = 0; i < coordinates.length; i++) {
				var vertexCoord = coordinates[i].split(' ');						 
				elementPath.push(new google.maps.LatLng(vertexCoord[0], vertexCoord[1]));    
			}
			
			$.extend( polyOptions, {path: elementPath} );			
			var element = {options:polyOptions};			
			
			if(coordinates[0] == coordinates[coordinates.length-1]){
				$.extend( mapOptions, {	polygon : element} );					
			}else{
				$.extend( mapOptions, {	polyline : element} );	
			}			
		}
		
		map.gmap3(mapOptions);
	}

	/**
	 * Function used by the home page to initialize and display the initial map
	 * @ selector: selector to retrieve via jQuery the map div
	 */
	function initializeHomeMap(selector) {

		$('#bluescreen').hide();

		var visibilityOn = { "visibility" : "on"};
		var visibilityOff = {"visibility" : "off"};
		var bgColor = defaultBgHomeMapColor;
		var lightness = {"lightness" : 10};
		var saturation = {"saturation" : -49};

		$(selector).gmap3(
				{
					map : {
						options : {
							center : defaultMapCenter,
							zoom : defaultZoomLevel,
							mapTypeId : google.maps.MapTypeId.ROADMAP,
							mapTypeControl : false,
							navigationControl : false,
							scrollwheel : false,
							fullscreenControl : false,
							streetViewControl : false,
							//Added to disable controls
							zoomControl : false,
							panControl : false,
							//ends
							styles : [ {
										"stylers" : [ visibilityOn, bgColor, saturation ]
									}, {
										"featureType" : "road.highway",
										"stylers" : [ bgColor, saturation, lightness ]
									}, {
										"featureType" : "road.arterial",
										"stylers" : [ bgColor, saturation, lightness, visibilityOn ]
									}, {
										"featureType" : "road.local",
										"stylers" : [ bgColor, saturation, lightness ]
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

	/**
	 * Function used by the component map page to initialize and display the map
	 * @ selector: selector to retrieve via jQuery the component map div
	 * @ center: initial center to display the map
	 * @ zoom: initial zoom to display the map
	 */
	function initializeComponentMap(mapProperties) {
		 
		 const mapCenter = (mapProperties.center?mapProperties.center:defaultMapCenter);
	     const zoomLevel = (mapProperties.zoom?mapProperties.zoom:defaultZoomLevel);
	     
		 $(mapProperties.selector).gmap3({
			map : {
				options : {
					center : mapCenter,
					zoom : zoomLevel,
					mapTypeId : google.maps.MapTypeId.STREET,
					mapTypeControl : true,
					navigationControl : false,
					panControl : false,
					scrollwheel : true,
					fullscreenControl : false,
					streetViewControl : true,
					styles : [ {
						"featureType" : "poi",
						"stylers" : [ {	"visibility" : "simplified"} ]
					}, {
						"elementType" : "labels.icon",
						"stylers" : [ {	"visibility" : "off"} ]
					} ]
				}
			}
		});

		smap_init();
		showMapControls();
	};

	// **************************************************************
	// Input location map
	// **************************************************************

	function clearOverlay() {		
		if (mc) {
			mc.clearMarkers();
		}

		if(oms){
        	oms.clearMarkers();        	
		}
		
		
		for (var i = 0; i < polylineMarkers.length; i++) {
		   polylineMarkers[i].setMap(null);
		}
		polylineMarkers = [];	
		
		for (var i = 0; i < polylines.length; i++) {
			polylines[i].setMap(null);		
			polylines[i].setPath(polylineMarkers);
		}
		
		polylines = [];
	}

	function updateSearchedPosition(location) {
		if (marker) {
			marker.setMap(null);
		}

		var markerOptions = {
			map : map,
			draggable : true,
			animation : google.maps.Animation.DROP,
		};

		marker = new google.maps.Marker({
			options : markerOptions,
		});

		google.maps.event.addListener(marker, 'dragend', function() {
			updateSearchedPosition(marker.getPosition());
			geocodeAddress(marker.getPosition());
		});
		
		var image = {
		    url: '${imgSpot}',
		    size: new google.maps.Size(22, 22),
		    origin: new google.maps.Point(0, 0),
		    anchor: new google.maps.Point(0, 22)
		};

		marker.setIcon(image);

		map.setZoom(defaultInputLocationZoomLevel);		
		marker.setPosition(location);
		map.setCenter(location);

		$('#latitude').val(location.lat());
		$('#longitude').val(location.lng())

	};

	function initializeAddressTypeAhead(address) {

		map = $('#input_location_map_canvas').gmap3('get');

		function manualSearch() {
			if (address.val()) {
				geocoder.geocode({'address' : address.val() }, 
				  function delegate(results, status) {
					if (status == google.maps.GeocoderStatus.OK && results[0]) {
						var location = buildLatLng(results[0].geometry.location.lat(), results[0].geometry.location.lng());
						updateSearchedPosition(location);
					}
				});
			}
		};
		

		function initializeTypeAheadResults() {
			labels = [];
			result_map = {};
		}

		if(!geocoder) {
			initializeGeoCoder();
		}

		address.typeahead({
			source : function(query, process) {
				initializeTypeAheadResults();
				function storeTypeAheadResults(results, status) {
					if (status == google.maps.GeocoderStatus.OK) {
						$.each(results, function(index, item) {
							result_map[item.formatted_address] = item;
							labels.push(item.formatted_address);
						});
						return process(labels);
					}
				}
				geocoder.geocode({'address' : query}, storeTypeAheadResults);
			},
			updater : function(item) {
				var location = buildLatLng(result_map[item].geometry.location.lat(),	result_map[item].geometry.location.lng());
				map.setCenter(location);
				map.setZoom(defaultInputLocationZoomLevel);
				updateSearchedPosition(location);
				return item;
			},
		});

		address.keypress(function(event) {
			switch (event.keyCode) {
			case 9: // tab
			case 13: // enter
				event.preventDefault();
				manualSearch();
				break;
			}
		});
	}

	/**
	 * Function used to initialize and show map on the component pages (public and admin).	 
	 */
	function initializeInputLocationMap() {

		$('#input_location_map_canvas').gmap3({
			map : {
				options : {
					center : defaultMapCenter,
					zoom : defaultInputLocationZoomLevel,
					mapTypeId : google.maps.MapTypeId.STREET,
					mapTypeControl : true,
					navigationControl : false,
					scrollwheel : true,
					fullscreenControl : false,
					streetViewControl : true
				}
			}
		});
		map = $('#input_location_map_canvas').gmap3('get');

		var locate = $('#locate');
		locate.tooltip({
			placement : 'right',
			title : locateMeMsg,
			trigger : 'hover'
		});

		locate.click(function() {
			if (navigator.geolocation) {
				navigator.geolocation.getCurrentPosition(
						function(position) {
							var location = buildLatLng(position.coords.latitude, position.coords.longitude);
							geocodeAddress(location);
							updateSearchedPosition(location);
						}, function(error) {
						}, {
							timeout : 10000
						});
			}
		});

		initializeAddressTypeAhead($('#locationaddress'));
	}
	
	//Funcions globals
	
	function buildPublicComponentDetailUrl(poi) {
        var detailUrl = '${publicComponentDetailUrl}/' + poi.options.id + '/detail';
        detailUrl = addParamToUrl(detailUrl, 'lat', map.getCenter().lat());
        detailUrl = addParamToUrl(detailUrl, 'lng', map.getCenter().lng());
        detailUrl = addParamToUrl(detailUrl, 'zoom', map.getZoom());
        detailUrl = addParamToUrl(detailUrl, 'filter', $('#selectedComponentType').val());

        if (window.alternative) {
            detailUrl = addParamToUrl(detailUrl, 'alternative', 'true');
        }

        var mapType = '${mapType}';
        if (mapType != '') {
            detailUrl = addParamToUrl(detailUrl, 'mapType', mapType);
        }

        return detailUrl;
    }
	
	function buildLatLng(latitude, longitude) {
        return new google.maps.LatLng(latitude, longitude);
    }

    function centerMap(center_location) {
        map.setCenter(center_location);
    }
    
    function isGeocodeSuccess(results, status) {
		return status == google.maps.GeocoderStatus.OK && results[0];
	}
	
	function formatGeocodeAddress(results, headerResume) {
		return results[0].formatted_address;
	}
	
	function buildGeocoder() {
		return new google.maps.Geocoder();
	}
	
	function getBoundsForSearchElements(){    		
    	// offsetx is the distance we want to expand the bounds on the x-axis, in pixels. 
		// offsety is the distance  we want to expand the bounds on the y-axis, in pixels.     	
		var factor = getExtendFactor();
		var offsetx = factor * $(".map2").width();
    	var offsety = factor * $(".map2").height();
    	
    	var scale = Math.pow(2, map.getZoom());
    	var pixelOffset = new google.maps.Point((offsetx/scale) || 0,(offsety/scale) ||0);
    	
    	var projection = map.getProjection();
    	var bounds = map.getBounds();
    	var swPoint = bounds.getSouthWest();
    	var nePoint = bounds.getNorthEast();
    	
    	var swPointPixels = projection.fromLatLngToPoint(swPoint);
    	var nePointPixels = projection.fromLatLngToPoint(nePoint);
    	
    	var newSwPointPixels = new google.maps.Point(swPointPixels.x - pixelOffset.x, swPointPixels.y + pixelOffset.y);
    	var newNePointPixels = new google.maps.Point(nePointPixels.x + pixelOffset.x, nePointPixels.y - pixelOffset.y);
    	var newSwPoint = projection.fromPointToLatLng(newSwPointPixels);
    	var newNePoint = projection.fromPointToLatLng(newNePointPixels);
    	
    	var newBounds = new google.maps.LatLngBounds(newSwPoint, newNePoint);    	
    	return newBounds.toUrlValue();
    }
	
	function getPlaces() {
	    	// Method that retrieves all components that fit into map bounds and add all them into
	    	// map as markers.
	    	var urlToCall = buildJsonComponentsUrl();

	    	var additionalData = {};

	    	var componentType = $('#selectedComponentType').val();
	    	if(componentType){
	    		additionalData.componentType = componentType;
	    	}

	    	if(filterByBounds && map.getBounds()){
	    		additionalData.bounds = getBoundsForSearchElements();
	    	}


	    	jsonGET(urlToCall, additionalData , function(data) {
	    		clearOverlay();
				if(data.components.length > 0){
		        	for(var i = 0; i < data.components.length; i++) {
		            	// Component could have zero, one or many coordinates. Depending on whether it have 1 or N coordinates,
		            	// it will be displayed on the map as a marker or as a polyline/polygon
		            	var component = data.components[i];

		            	if(!component.coordinates){
		                    continue;
		                }

		            	if(component.coordinates.length == 1){
		            		// marker case
			        		var poi = createComponentMarker(component);

		            		var showPoiRoute = showRoutes && (map.getZoom() >= minRouteZoomLevel);

			            	if(!poi.options.route || !poi.options.route.length || !showPoiRoute){
								oms.addMarker(poi);
								mc.addMarker(poi);
							}else{
								printMobileComponent(poi);
							}
		            	}else{
		            		// polyline/polygon case
		            		// For the time being, these resources will be displayed as markers on the universal viewer.
		            		if(showMultiCoordinates && multiCoordinatesAsPois){
			            		var poi = createComponentMarker(component);

				            	if(!poi.options.route || !poi.options.route.length){
									oms.addMarker(poi);
									mc.addMarker(poi);
								}
		            		}
		            	}
		        	}
				}

				initClusterElements();
	    	});
	    };
	
	// -->
</script>
