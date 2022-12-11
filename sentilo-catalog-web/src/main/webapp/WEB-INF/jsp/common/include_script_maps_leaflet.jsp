<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8" %>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp" %>

<spring:eval expression="@sentiloConfigProperties.getProperty('sentilo.catalog.map.wms.layers')" var="wmsLayers" />
<spring:eval expression="@sentiloConfigProperties.getProperty('sentilo.master.application.id')" var="catalogAppID"/>

<spring:url value="/static/css/leaflet-custom.css" var="leafletCustomCSS"/>
<spring:url value="/static/js/sentilo/geocode.js" var="geocoder"/>
<spring:url value="/static/js/sentilo/leaflet.canvas-markers.js" var="leafletCanvasMarkers"/>

<link rel="stylesheet" href="https://unpkg.com/leaflet@1.6.0/dist/leaflet.css" crossorigin=""/>
<link rel="stylesheet" href="${leafletCustomCSS}" crossorigin="">

<script src="https://unpkg.com/leaflet@1.6.0/dist/leaflet.js" crossorigin=""></script>
<script src="${geocoder}" type="text/javascript" crossorigin=""></script>
<script src="${leafletCanvasMarkers}" type="text/javascript" crossorigin=""></script>

<script type="text/javascript">

    let boxOptions = {
        keepInView: true,
        closeButton: false,
        closeOnEscapeKey: true,
        maxWidth: "auto",
        offset: new L.point(14, -30)
    };

    const localStorageLayerKey = 'sentilo-${catalogAppID}-layer';
    const overlays = {};
    let canvasLayer;

    function initialize() {

        if (!geocoder) {
            initializeGeoCoder();

        }

        map.addEventListener('click', function (e) {
            if (e.placeId) {
                // Lets stop the event!
                e.stop();
            }
            poiClicked = false;
            closeInfoWindow();

            if (isModalLayerVisible()) {
                hideModalLayer();
            }
        });
    }

    function initializeGeoCoder() {
        geocoder = new Geocode();
    }

    function createComponentMarker(component) {
        const marker = L.marker(
            [component.centroid.latitude, component.centroid.longitude],
            {
                id: component.id,
                name: component.name,
                type: component.type,
                icon: getPOIImage(component),
                title: component.icon
            });
        if (component.route !== undefined) {
            marker.options.route = component.route;
        }
        return marker;
    }

    function fillInfoWindowPixelOffset(elementsCount, poi, boxOptions) {
        boxOptions.autoPanPadding = L.point(50, 50);
    }

    function getPOIImage(data) {
        return L.icon({
            iconUrl: data && data.icon ? '${iconsPath}/' + data.icon + '-poi.png' : '${defaultIcon}',
            iconSize: [28, 30],
            iconAnchor: [15, 14],
            popupAnchor: [-15, 7]
        });
    }

    function printMobileComponent(poi) {
        if (!poi.options.route) {
            return;
        }
        let latLngPoints = [];

        for (let i = 0; i < poi.options.route.length; i++) {
            const location = poi.options.route[i].location;
            const latLng = L.latLng(location.latitude, location.longitude);

            const polylineMarker = L.marker(latLng,
                {
                    id: poi.options.id,
                    name: poi.options.name,
                    type: poi.options.type,
                    icon: getMobilePOIImage(i, poi),
                    title: poi.options.title,
                    iconName: poi.options.icon,
                    to: poi.options.route[i].toTime,
                    from: poi.options.route[i].fromTime,
                    spiderfy: false
                });
            polylineMarkers.push(polylineMarker);
            canvasLayer.addMarker(polylineMarker);
            addPolylineMarkerListener(polylineMarker);
            latLngPoints.push(latLng);
        }

        const colors = ["#00C8A9"];

        for (let i = 0; i < latLngPoints.length - 1; i++) {
            polylines[polylines.length] = L.polyline(latLngPoints, {
                stroke: true,
                color: colors[i % colors.length],
                opacity: 0.5,
                weight: 4,
            }).addTo(map);
        }
    }

    function addPolylineMarkerListener(polylineMarker) {
        polylineMarker.on('click', function (event) {
            closeInfoWindow();
            fillInfoWindow(polylineMarker, event);
        });
    }

    function getMobilePOIImage(i, poi) {
        const total = poi.options.route.length;
        const mobileEndIcon = '${iconsPath}/' + poi.options.title + '-poi.png';
        const mobileBeginningIcon = '${iconsPath}/' + 'mobile-poi.png';
        const imgUrl = i == total - 1 ? mobileEndIcon : mobileBeginningIcon;
        const size = i == total - 1 ? 25 : 12;
        const anchorx = i == total - 1 ? 0 : 7;
        const anchory = i == total - 1 ? 25 : 7;

        return L.icon({
            iconUrl: imgUrl,
            iconSize: [size, size],
            iconAnchor: [anchorx, anchory],
        });
    }

    function buildPopup(content, poi) {
    	infowindow = poi.bindPopup(content, boxOptions).openPopup();
    }
    
    function closePopup() {				
    	infowindow.closePopup();    		
    }

    // Init smap_init function
    function smap_init(loadOnInit) {
    	
        $(window).resize(function () {
            initializeMapControls();
        });
        initializeMapControls();

        let address = $('#address');
        let locate = $('#locate');

        address.tooltip({
            placement: 'bottom',
            title: emptyListMsg,
            trigger: 'manual'
        });

        initialize();

        /*
        locate.tooltip({
            placement: 'right',
            title: locateMeMsg,
            trigger: 'hover'
        });
        */

        locate.click(function () {
            if (navigator.geolocation) {
                navigator.geolocation.getCurrentPosition(
                    function (position) {
                        const location = buildLatLng(position.coords.latitude, position.coords.longitude);
                        updateSearchedPosition(location);
                    }, function (error) {
                    }, {timeout: 10000});
            }
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
            const location = L.latLng(result.lat, result.lon);
            map.setZoom(defaultSearchZoom);
            map.panTo(location);
            getPlaces();
            address.val(result.display_name);
            if (searchMarker) {
            	map.removeLayer(searchMarker);
            }
            searchMarker = L.marker(location).addTo(map);
        }

        function manualSearch() {
            geocoder.geocode({'address': address.val()}, function (results, status) {
                if (address.length == 0) {
                    address.tooltip('show');
                } else {
                    address.tooltip('hide');
                    if (results[0]) {
                        updateMyPosition(results[0]);
                    }
                }
            });
        }


        map.on('moveend', function (e) {
            infowindowMustBeClosed = false;
            if (!poiClicked) {
                updateMarkers(map.getCenter(), true);
            }
            canvasLayer.unspiderfy();
        });

        if (loadOnInit) {
            updateMarkers(map.getCenter(), true);
        }

    }

    // End smap_init function

    /**
     * Function used by the component detail page to display it on map (public and admin page).
     * @param mapOptions: object that contains
     * @attribute latitude: map center latitude
     * @attribute longitude: map center longitude
     * @attribute coordinates: component coordinates array where each element has the format latK lngK.
     * Its length, and if it is closed, dictates how to display the component (polygon, polyline or marker)
     * @attribute icon: icon to use when coordinates represents a POI
     */
    function initializeMap(mapOptions) {        
        initializeMapBy('map_canvas');

        map.setView(new L.latLng(mapOptions.latitude, mapOptions.longitude), defaultZoomLevel);

        addGeometricElementToMap(
            map,
            mapOptions.latitude,
            mapOptions.longitude,
            mapOptions.coordinates,
            mapOptions.icon);

    }

    function initializeMapControls() {        
        const map_pos = $('#map_canvas_1').offset();
        $('#map_controls').css({
            position: 'absolute',
            top: map_pos.top + 10
        });
    }

    function addGeometricElementToMap(map, latitude, longitude, coordinates, icon, options) {
        // If coordinates length equals to 1, it represents a POI
        // Otherwise it is a polyline or polygon

        const polyOptions = {
            color: "#0000FF",
            opacity: 0.8,
            weight: 2,
            fillColor: "#0000FF",
            fillOpacity: 0.35
        };

        function isPoi() {
            return coordinates.length == 1;
        }

        if (isPoi()) {
            const poiImage = icon ? getPOIImage({'icon': icon}) : getPOIImage();
            canvasLayer.addMarker(L.marker([latitude, longitude], {icon: poiImage}));
        } else {
            let elementPath = [];
            coordinates.forEach(coordinate => {
                const vertexCoordinate = coordinate.split(' ');
                elementPath.push(new L.latLng(vertexCoordinate[0], vertexCoordinate[1]));
            });

            if (coordinates[0] == coordinates[coordinates.length - 1]) {
                L.polygon(elementPath, polyOptions).addTo(map);
            } else {
                L.polyline(elementPath, polyOptions).addTo(map);
            }
        }
    }

    /**
     * Function used by the component map page to initialize and display the map
     * @mapProperties an object with properties that initialize the map
     */
    function initializeComponentMap(mapProperties) {
        const {selector, center, zoom, controls} = initializeMapProperties(mapProperties);

        initializeMapBy(selector);

        smap_init();

        map.setView(center, zoom, {maxZoom: 18});

        if (controls) {
            showMapControls();
        } else {
            hideMapControls();
        }


        if ($("#bgHomeScreen").length) {
            $("#bgHomeScreen").css("background-color", defaultBgHomeMapColor.color);
        }

        function initializeMapProperties(mapProperties) {
            const selector = parseSelector(mapProperties.selector);
            const center = mapProperties.center ? L.latLng(mapProperties.center) : L.latLng(defaultMapCenter);
            const zoom = mapProperties.zoom ? mapProperties.zoom : defaultZoomLevel;
            let controls;
            if (mapProperties.controls !== undefined) {
                controls = mapProperties.controls;
            } else {
                controls = true;
            }
            return {selector, center, zoom, controls};
        }
    }

    function parseSelector(selector) {
        if (!selector) {
            return 'map_canvas_1';
        }
        if (selector.startsWith('#')) {
            return selector.split('#')[1];
        }
        return mapProperties.selector;
    }

    function setLayer(layerName) {
        overlays[layerName].addTo(map);
    }

    function initializeMapBy(selector) {
        map = L.map(selector);


        for (let layer of ${wmsLayers}) {
            overlays[layer.name] = L.tileLayer.wms(layer.url, {
                id: layer.name,
                layers: layer.layer,
                format: layer.format,
                version: layer.version,
                attribution: layer.attribution,
            });
        }

        if (overlays[Object.keys(overlays)[1]] !== undefined) {
            L.control.layers(overlays, null, {position: 'bottomright'}).addTo(map);
        }

        // Remove default zoom control from its position and add to bootm right
        map.zoomControl.remove();
        map.addControl(L.control.zoom({ position: 'bottomright' }));
                
        // Add change canvas button layer control
        canvasLayer = L.canvasIconLayer({}).addTo(map);
        
        // Add custom fullscreen button
        initFullscreenButton();

        const storedLayerName = localStorage.getItem(localStorageLayerKey);
        if (storedLayerName) {
            setLayer(storedLayerName);
        } else {
            setLayer(Object.keys(overlays)[0]);
        }

        canvasLayer.addOnClickListener(function (e, data) {
            canvasLayer.spiderfy(data[0].data);
        });

        canvasLayer.addListener('popup', (marker) => {
            fillInfoWindow(marker);
            poiClicked = true;
            if (poiClicked && isModalLayerVisible()) {
                updateModalLayer(component.id);
            }
        });

        map.on('baselayerchange', (layerControlEvent) => {            
            populateStorage(layerControlEvent.name);
        })
    }

    function populateStorage(layerKey) {
        localStorage.setItem(localStorageLayerKey, layerKey);
        setLayer(layerKey);
    }

    function showMapControls() {
        $('#map_controls').show();
        $('.leaflet-control-container').show();
    }

    function hideMapControls() {
        $('#map_controls').hide();
        $('.leaflet-control-container').hide();
    }

    // **************************************************************
    // Input location map
    // **************************************************************

    function clearOverlay() {
        if (canvasLayer) {
            canvasLayer.clearLayers();
        }
        if (map) {
        	clearMapPolylines();
        }
    }


    function updateSearchedPosition(location) {
        map.setView(location, defaultInputLocationZoomLevel);
    }

    /**
     * Function used to initialize and show map on the component pages (public and admin).
     */
    function initializeInputLocationMap() {

        initializeMapBy('input_location_map_canvas');

        map.setView(defaultMapCenter, defaultZoomLevel);

        let locate = $('#locate');
        
        /*
        locate.tooltip({
            placement: 'right',
            title: locateMeMsg,
            trigger: 'hover'
        });
        */

        locate.click(function () {
            if (navigator.geolocation) {
                navigator.geolocation.getCurrentPosition(
                    function (position) {
                        const location = buildLatLng(position.coords.latitude, position.coords.longitude);
                        geocodeAddress(location);
                        updateSearchedPosition(location);
                    }, function (error) {
                    }, {timeout: 10000});
            }
        });


        if (!geocoder) {
            initializeGeoCoder();

        }
    }


    //Funcions globals
	
    function getCenterCoordinates(){
		return [map.getCenter().lat, map.getCenter().lng];
	}

    function buildLatLng(latitude, longitude) {
        return L.latLng(latitude, longitude);
    }

    function centerMap(center_location) {
        map.panTo(center_location);
    }


    function isGeocodeSuccess(results, status) {
        return status === 'success';
    }


    function formatGeocodeAddress(results, headerResume) {         
    	// Default value
        var formatted_address = results.display_name;
        
    	if (headerResume) {
    		// If headerResume, returns an address resume instance of the full address
        	// But as some address haven't neither city nor village fields filled in., in these cases, display_name is displayed        	
            let address = results.address;
            if(address.city || address.town){
            	let town = address.city ? address.city : address.village; 
                let formatted_address = address.road + ', ' + address.postcode + ' ' + town + ', ' + address.country;
            }
            
        }
    	return results.display_name;
    }


    function buildGeocoder() {
        return new Geocode();


    }

    function getBoundsForSearchElements() {
        // offsetx is the distance we want to expand the bounds on the x-axis, in pixels.
        // offsety is the distance  we want to expand the bounds on the y-axis, in pixels.
        var factor = getExtendFactor();
        var offsetx = factor * $(".map2").width();
        var offsety = factor * $(".map2").height();

        var scale = Math.pow(2, map.getZoom());
        var pixelOffset = L.point((offsetx / scale) || 0, (offsety / scale) || 0);

        var bounds = map.getBounds();
        var swPoint = bounds.getSouthWest();
        var nePoint = bounds.getNorthEast();

        var swPointPixels = map.latLngToLayerPoint(swPoint);
        var nePointPixels = map.latLngToLayerPoint(nePoint);

        var newSwPointPixels = L.point(swPointPixels.x - pixelOffset.x, swPointPixels.y + pixelOffset.y);
        var newNePointPixels = L.point(nePointPixels.x + pixelOffset.x, nePointPixels.y - pixelOffset.y);
        var newSwPoint = map.layerPointToLatLng(newSwPointPixels);
        var newNePoint = map.layerPointToLatLng(newNePointPixels);

        return newSwPoint.lat + ',' + newSwPoint.lng + ',' + newNePoint.lat + ',' + newNePoint.lng;
    }

    function addMarkerToMap(poi) {
    	canvasLayer.addMarker(poi);
    }
    
    function doAfterAddMarkersToMap(){
    	//Nothing to do
    }
    
	function clearMapPolylines() {
		// Removes all polylines from map
		if (polylines) {
	    	polylines.forEach(function(polyline) {
	    		map.removeLayer(polyline);
	    	});
	    }
	    polylines = [];
	    polylineMarkers = [];
	}

	function initFullscreenButton() {
		var button = '<div id="map-fullscreen-button" class="leaflet-control-fullscreen leaflet-bar leaflet-control">';
		button +=		'<a class="leaflet-control-fullscreen-button leaflet-bar-part" onclick="toggleFullScreen(document.body)" href="#" title="View Fullscreen">';
		button += 			'<i class="icon-resize-full"></i>';
		button += 		'</a>';
		button +=	'</div>';
		$('.leaflet-bottom.leaflet-right').prepend(button);
				
		window.onresize = function (event) {
			setTimeout(function(){				
				if(fullscreenActive){
					$('#map-fullscreen-button i').removeClass('icon-resize-full').addClass('icon-resize-small');
					$('#map-fullscreen-button a').prop('title', 'Exit Fullscreen');
				}else{
					$('#map-fullscreen-button i').removeClass('icon-resize-small').addClass('icon-resize-full');
					$('#map-fullscreen-button a').prop('title', 'View Fullscreen');
				}				
			}, 100);
		}
	}
	
	
	
    // -->
</script>

