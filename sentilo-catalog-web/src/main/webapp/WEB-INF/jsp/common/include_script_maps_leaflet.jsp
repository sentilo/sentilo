<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8" %>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp" %>

<spring:eval expression="@catalogConfigProperties.getProperty('catalog.map.wms.url')" var="wmsUrl" />
<spring:eval expression="@catalogConfigProperties.getProperty('catalog.map.wms.layers')" var="wmsLayers" />
<spring:eval expression="@catalogConfigProperties.getProperty('catalog.map.wms.version')" var="wmsVersion" />
<spring:eval expression="@catalogConfigProperties.getProperty('catalog.map.wms.format')" var="wmsFormat" />
<spring:eval expression="@catalogConfigProperties.getProperty('catalog.map.wms.attribution')" var="wmsAttribution" />
<spring:eval expression="@catalogConfigProperties.getProperty('catalog.map.wms.styles')" var="wmsStyles" />

<spring:url value="/static/css/leaflet-custom.css" var="leafletCustomCSS"/>
<spring:url value="/static/js/sentilo/geocode.js" var="geocoder" />

<link rel="stylesheet" href="https://unpkg.com/leaflet@1.6.0/dist/leaflet.css" crossorigin=""/>
<link rel="stylesheet" href="https://unpkg.com/leaflet.markercluster@1.4.1/dist/MarkerCluster.Default.css" crossorigin="">
<link rel="stylesheet" href="${leafletCustomCSS}" crossorigin="">

<script src="https://unpkg.com/leaflet@1.6.0/dist/leaflet.js" crossorigin=""></script>
<script src="https://unpkg.com/leaflet.markercluster@1.4.1/dist/leaflet.markercluster.js" crossorigin=""></script>
<script src="${geocoder}" type="text/javascript" crossorigin=""></script>

<script type="text/javascript">

    let boxOptions = {
        keepInView: true,
        closeButton: false,
        closeOnEscapeKey: true,
        maxWidth: "auto",
        offset: new L.point(14, -30)
    };

    function initialize() {

        if(!geocoder) {
			initializeGeoCoder();
		}

        if (!markerCluster) {
            //markerCluster = new MarkerClusterer(map, [], {maxZoom: defaultMaxZoomCluster, styles: clusterStyles[0]});
            markerCluster = L.markerClusterGroup({
                iconCreateFunction: function (cluster) {
                    const backgroundUrl = clusterImgsPath + 'poi-group.png';
                    return new L.DivIcon({
                        html: '<div style="background: url(' + backgroundUrl + ')"><span>' + cluster.getChildCount() + '</span></div>',
                        className: 'marker-cluster',
                        iconSize: new L.Point(40, 40)
                    });
                }
            , disableClusteringAtZoom: defaultMaxZoomCluster });
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
                type: component.type,
                icon: getPOIImage(component),
                title: component.icon
            });
        if (component.route !== undefined) {
            marker.options.route = component.route;
        }
        marker.on('click', function (e) {
            fillInfoWindow(e.target);
            poiClicked = true;
            if (poiClicked && isModalLayerVisible()) {
                updateModalLayer(component.id);
            }
        });
        return marker;
    }

    function fillInfoWindowPixelOffset(elementsCount, poi, boxOptions) {
        boxOptions.autoPanPadding = L.point(50, 50);
    }

    function getPOIImage(data) {
        return L.icon({
            iconUrl: data && data.icon ? '${iconsPath}/' + data.icon + '-poi.png' : '${defaultIcon}',
            iconSize: [28, 30],
            iconAnchor: [0, 22]
        });
    }

    function printMobileComponent(poi) {
        if (poi.options.route) {
            let latLngPoints = [];

            for (let i = 0; i < poi.options.route.length; i++) {
                const location = poi.options.route[i].location;
                const latLng = L.latLng(location.latitude, location.longitude);

                const polylineMarker = L.marker(latLng,
                    {
                        id: poi.options.id,
                        type: poi.options.type,
                        icon: getMobilePOIImage(i, poi),
                        title: poi.options.title,
                        iconName: poi.options.icon,
                        to: poi.options.route[i].toTime,
                        from: poi.options.route[i].fromTime
                    });
                polylineMarkers.push(polylineMarker);
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
        poi.bindPopup(content,boxOptions).openPopup();
    }

    function initClusterElements() {
        markerCluster.refreshClusters();
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

        locate.tooltip({
            placement: 'right',
            title: locateMeMsg,
            trigger: 'hover'
        })

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
            map.setZoom(defaultZoomLevel);

            L.marker(location).addTo(map);
            map.panTo(location);
            getPlaces();
            address.val(result.display_name);
        }

        function manualSearch() {
            geocoder.geocode({'address' :address.val()}, function (results, status) {
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
        });

        //map.on('load', function (e) {
        //    updateMarkers(map.getCenter(), true);
        //});

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
        console.log('Init map for component detail');

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
        console.log('Initializing map controls')
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
            L.marker([latitude, longitude], {icon: poiImage})
                .addTo(map);
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

        map.setView(center, zoom, {maxZoom:18});        

        if (controls) {
            showMapControls();
        } else {
            hideMapControls();
        }
        
        if ($("#bgHomeScreen").length){
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

    function initializeMapBy(selector) {
        map = L.map(selector);
        let overlays = {};
        for (let layerKey of '${wmsLayers}'.split(',')) {
            overlays[layerKey] = L.tileLayer.wms('${wmsUrl}', {
                layers: layerKey,
                format: '${wmsFormat}',
                version: '${wmsVersion}',
                attribution: '${wmsAttribution}',
            })
        }

        if (overlays[Object.keys(overlays)[1]] !== undefined) {
            L.control.layers(overlays, null, {position: 'bottomright'}).addTo(map);
        }
        overlays[Object.keys(overlays)[0]].addTo(map);
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
        if (markerCluster) {
            markerCluster.clearLayers();
        }

        for (let i = 0; i < polylineMarkers.length; i++) {
            polylineMarkers[i].addTo(map);
        }
        polylineMarkers = [];

        for (var i = 0; i < polylines.length; i++) {
            polylines[i].addTo(map);
        }

        polylines = [];
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
        locate.tooltip({
            placement: 'right',
            title: locateMeMsg,
            trigger: 'hover'
        });

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
        
		if(!geocoder) {
			initializeGeoCoder();
		}
    }
    
  //Funcions globals
    
    function buildPublicComponentDetailUrl(poi) {
        var detailUrl = '${publicComponentDetailUrl}/' + poi.options.id + '/detail';
        detailUrl = addParamToUrl(detailUrl, 'lat', map.getCenter().lat);
        detailUrl = addParamToUrl(detailUrl, 'lng', map.getCenter().lng);
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
        return L.latLng(latitude, longitude);
    }

    function centerMap(center_location) {
        map.panTo(center_location);
    }
    
	function isGeocodeSuccess(results, status) {
		return status === 'success';
	}
	
	function formatGeocodeAddress(results, headerResume) {
		if(headerResume){
		  // returns an address	resume instance of the full address
		  var address = results.address;
          var formatted_address = address.road + ', ' + address.postcode  + ' ' + address.city  + ', ' + address.country;
          return formatted_address; 		  
		}else{
		  return results.display_name;
		}
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
	
	function getPlaces() {
            // Method that retrieves all components that fit into map bounds and add all them into
            // map as markers.
            var urlToCall = buildJsonComponentsUrl();

            var additionalData = {};

            var componentType = $('#selectedComponentType').val();
            if (componentType) {
                additionalData.componentType = componentType;
            }
			
			if(filterByBounds && map.getBounds()){
	    		additionalData.bounds = getBoundsForSearchElements();
	    	}

            jsonGET(urlToCall, additionalData, function (data) {
                clearOverlay();
                if (data.components.length > 0) {
                    for (let i = 0; i < data.components.length; i++) {
                        // Component could have zero, one or many coordinates. Depending on whether it have 1 or N coordinates,
                        // it will be displayed on the map as a marker or as a polyline/polygon
                        const component = data.components[i];

                        if (!component.coordinates) {
                            continue;
                        }

                        if (component.coordinates.length == 1) {
                            // marker case
                            const poi = createComponentMarker(component);

                            var showPoiRoute = showRoutes && (map.getZoom() >= minRouteZoomLevel);

                            if (!poi.options.route || !poi.options.route.length || !showPoiRoute) {
                                markerCluster.addLayer(poi);
                            } else {
                                printMobileComponent(poi);
                            }
                        } else {
                            // polyline/polygon case
                            // For the time being, these resources will be displayed as markers on the universal viewer.
                            if (showMultiCoordinates && multiCoordinatesAsPois) {
                                const poi = createComponentMarker(component);

                                if (!poi.route || !poi.route.length) {
                                    markerCluster.addLayer(poi);
                                }
                            }
                        }
                        map.addLayer(markerCluster);
                    }
                }
            });
        }

    // -->
</script>
