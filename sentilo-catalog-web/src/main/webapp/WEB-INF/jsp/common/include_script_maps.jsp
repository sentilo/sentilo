<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@ include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<%@ include file="/WEB-INF/jsp/common/include_common_messages.jsp"%>

<spring:eval expression="@sentiloConfigProperties.getProperty('sentilo.catalog.map.provider')" var="provider_map" />
<spring:eval expression="@sentiloConfigProperties.getProperty('sentilo.catalog.map.mapbox.home.style')" var="mapboxStyle" />
<spring:eval expression="@sentiloConfigProperties.getProperty('sentilo.catalog.map.geocode.onMapMove','false')" var="geocodeOnMapMove" />

<spring:url value="/static/css/treejs.css" var="treejsCSS" />
<spring:url value="/static/css/map-sidebar.css" var="mapSidebarCSS" />
<spring:url value="/static/js/treejs.js" var="treeJS" />
<spring:url value="/static/js/sentilo/map-sidebar.js" var="mapSidebarJS" />
<spring:url value="/component" var="publicComponentDetailUrl" />
<spring:url value="/component/map" var="componentMap" />
<spring:url value="/static/img/icons" var="iconsPath" />
<spring:url value="/static/img/spot.png" var="imgSpot" />
<spring:url value="/static/img/" var="imgsPath" />
<spring:url value="/static/img/icons/pins6-poi.png" var="defaultIcon" />

<link rel="stylesheet" href="${treejsCSS}" media="all">
<link rel="stylesheet" href="${mapSidebarCSS}" media="all">
<script type="text/javascript" src="${treeJS}"></script>
<script type="text/javascript" src="${mapSidebarJS}"></script>

<script>
    window.messages = {
        boolValues: {
            falseValue: falseValueMsg,
            trueValue: trueValueMsg
        }
    };

    // Show detailed modal view on component map
    var useDetailedFrontView = true;

    // the initial map center (Barcelona city)
    //var defaultMapCenter = [41.4001221, 2.172839];
    var defaultMapCenter = [41.5667, 2.0167];
    var defaultZoomLevel = 14;
	var defaultInputLocationZoomLevel = 17;
	var defaultSearchZoom = 17;
	var searchMarker = null;
	
	// the maximum zoom level beyond which pois are not grouped into clusters
	var defaultMaxZoomCluster = 13;
	
    // flag for control if routes must be displayed or no on the current map
    var showRoutes = false;

    // flag for control if only components that fit into bounds's map must be searched on the server
    var filterByBounds = true;

    // the minimum zoom level beyond which routes are displayed
    var minRouteZoomLevel = 15;
    // flag for control if multicoordinates components must be displayed on the universal viewer (true by default)
    var showMultiCoordinates = true;
    // flag for control if components with multiple coordinates must be displayed as POIs or as polylines/polygons
    // on the universal viewer (by default as POIs)
    var multiCoordinatesAsPois = true;

    // Small devices width
    var smallDevicesWidth = 767;

    // Home map config
    var defaultBgHomeMapColor = {"color": "#0088ff"};

    var geocoder;
    let map;
    var marker;
    var infowindow;
    var infowindowMustBeClosed;
    var markerCluster;

    var polylines = [];
    var polylineMarkers = [];

    var poiClicked = false;

    var clusterImgsPath = '${iconsPath}/';

    const geocodeOnMapMove = ${geocodeOnMapMove};

    function buildJsonComponentsUrl() {
        var urlToCall = '${componentMap}';
        var mapType = '${mapType}';
        if (mapType != '') {
            urlToCall += '/' + mapType;
        }

        urlToCall += '/json';

        return urlToCall;
    }
	
	function initializeGeoCoder() {
	    geocoder = buildGeocoder();
	}	
		
	function createComponentHeader(latitude, longitude, target) {
    	var geo = buildGeocoder();
    	var componentDescription = '<spring:escapeBody htmlEscape="false" javaScriptEscape="true">${component.description}</spring:escapeBody>';
    	const success = function (results, status) {
            if (isGeocodeSuccess(results, status)) {            	
            	$(target).html(componentDescription + ' / ' + formatGeocodeAddress(results, true));            	                
            }
        };
        var center = buildLatLng(latitude, longitude);
        geo.geocode({'latLng': center}, success);    	
    }
	
	function updateMarkers(center, load) {
		closeInfoWindow();
		if(geocodeOnMapMove) {
			geocodeCenterAndUpdatePOI(center);
		}
		if(load) {
			getPlaces();
		}
	}
	
	function closeInfoWindow() {
		if (infowindow && infowindowMustBeClosed === true) {
			closePopup();			
    		infowindow = null;
    	}
    	infowindowMustBeClosed = true;
    }
	
	function geocodeCenterAndUpdatePOI(center, load) {		
		const success = function (results, status) {
			if (isGeocodeSuccess(results, status)) {
				let address = $('#address');
				address.val(formatGeocodeAddress(results));											               
			}
		};
		geocoder.geocode({'latLng': center}, success);
	}
	
	function geocodeAddress(location) {
		var success = function(results, status) {
			if (isGeocodeSuccess(results, status)) {
				$('#locationaddress').val(formatGeocodeAddress(results));
			}
		};
		geocoder.geocode({'latLng' : location}, success);
	}
	
   /**
	* Method that retrieves all components that fit into map bounds and add all them into
	* map as markers.
	*/ 
	function getPlaces() {        
        var urlToCall = buildJsonComponentsUrl();
        var additionalData = {};
        var ct = getTreeSelectedIds();
        
        if (ct) {
        	// Set ct param value only if filters are defined
        	// ct is array of strings (types)
            additionalData.ct = ct.toString();
        }

        if (filterByBounds && map.getBounds()) {
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
                            addMarkerToMap(poi);
                        } else {
                            printMobileComponent(poi);
                        }
                    } else {
                        // polyline/polygon case
                        // For the time being, these resources will be displayed as markers on the universal viewer.
                        if (showMultiCoordinates && multiCoordinatesAsPois) {
                            const poi = createComponentMarker(component);
                            if (!poi.route || !poi.route.length) {
                            	addMarkerToMap(poi);                     
                            }
                        }
                    }             
                }
            }
            
            doAfterAddMarkersToMap();            
        });
    }

	function retrieveLastObservations(poi, observationDiv, footerDiv) {

		var url = '${componentMap}/' + poi.options.id + '/lastOb/';
		var additionalData = {};

		if (poi.options.to) {
			additionalData.to = poi.options.to;
		}

		if (poi.options.from) {
			additionalData.from = poi.options.from;
		}

		jsonGET(url, additionalData, function (data) {
			bodyContent = '';
			var divIsOpen = false;

			// the sensor observations are displayed in two columns, so  the number of elements of each column
			// is fixed by the the number of sensor observations
			var elementsCount = data.sensorLastObservations.length;
			var numObsCol = (elementsCount % 2 == 0) ? parseInt(elementsCount / 2) : (parseInt(elementsCount / 2) + 1);
			var bodyStyle;

			// Fixs the infowindow location basen on pixelOffset
			fillInfoWindowPixelOffset(elementsCount, poi, boxOptions);

			if (elementsCount > 4) {
				bodyStyle = "overflow-y: scroll";
			}

			if (elementsCount > 0) {
				for (var i = 0; i < elementsCount; i++) {
					if (i % numObsCol == 0) {
						if (divIsOpen) {
							bodyContent += '</div>';
							vidIsOpen = false;
						}
						bodyContent += '<div class="infobox-body-column">';
						divIsOpen = true;
					}
					var observation = data.sensorLastObservations[i];
					if (observation.found) {
						if (observation.dataType === 'BOOLEAN') {
							observation.value = eval(observation.value) ? window.messages.boolValues.trueValue.toUpperCase() : window.messages.boolValues.falseValue.toUpperCase();
						}
						var offlineClass = (observation.sensorState === 'offline') ? 'class="offline"' : '';
						var tooltipText = '';
						if (observation.sensorSubState != null) {
							tooltipText += observation.sensorSubstateDesc;

						}

						bodyContent += '<p ' + offlineClass + 'data-toggle="tooltip" title="' + tooltipText + '">' + observation.value + ' ' + observation.unit + '</p><span class="label label-info">' + observation.sensorType + '</span>';
					} else {
						bodyContent += '<p>'+sensorObsNotFoungMsg+'</p><span class="label label-info">' + observation.sensorType + '</span>';
					}
				}
				bodyContent += '</div>';
			} else {
				// Add a ghost column to fix the gap when component has not sensors
				bodyContent = '<div class="infobox-body-column">';
				bodyContent += '	<p>&nbsp;</p>';
				bodyContent += '	<span class="label label-info">&nbsp;</span>';
				bodyContent += '</div>';
			}

			bodyContent += '<div style="clear: both;"></div>';

			var detailUrl = buildPublicComponentDetailUrl(poi);
			if(data.extendedDetailUrl) {
				setComponentDetailUrl(detailUrl+'&extendedDetailUrl=true');
			} else {
				setComponentDetailUrl(detailUrl);
			}
			
			

			var numRows = parseInt((elementsCount / 2) + (elementsCount % 2));
			var altClass = (numRows <= 1) ? "countA" : (numRows === 2) ? "countB" : "countC";

			var componentId = poi.options.id;
			let content = '';
			if(data.extendedDetailUrl) {
				content = '<div class="pointer" onclick="gotoComponentDetail(); return false;">';
			} else {
				content = '<div class="pointer" onclick="$(\'#componentSeeMoreLink\').hide(); updateModalLayer(\'' + componentId + '\'); infowindowMustBeClosed=false;">';
			}
			

			// Decide witch detail to show
			// For small devices, or not desired cases, uses old detail page
			if (window.innerWidth < smallDevicesWidth || useDetailedFrontView !== undefined && useDetailedFrontView === false) {
				content = '<a href="' + detailUrl + '">';
			}

			content += '<div class="infobox ' + altClass + '" id="infobox">';
			content += '<div class="infobox-content" style="display:block">';
			content += fillInfoWindowHeader(poi);
			content += fillInfoWindowBody(poi, bodyContent, observationDiv, bodyStyle);
			content += fillInfoWindowFooter(poi, data.lastUpdateTimeMessage, data.extendedDetailUrl, footerDiv);
			content += '</div>';
			content += '<div class="poi-infowindow-arrow"></div>';
			content += '</div>';

			if (window.innerWidth < smallDevicesWidth || useDetailedFrontView !== undefined && useDetailedFrontView === false) {
				content += '</div>';
			}

			buildPopup(content, poi);

		});
	}
	
	function buildPublicComponentDetailUrl(poi) {
        let centerCoordinates = getCenterCoordinates();
        let ct = getTreeSelectedIds();
        
        var detailUrl = '${publicComponentDetailUrl}/' + poi.options.id + '/detail';
        detailUrl = addParamToUrl(detailUrl, 'lat', centerCoordinates[0]);
        detailUrl = addParamToUrl(detailUrl, 'lng', centerCoordinates[1]);
        detailUrl = addParamToUrl(detailUrl, 'zoom', map.getZoom());
                
        if (ct) {
        	detailUrl = addParamToUrl(detailUrl, 'ct', ct.toString());            
        }
        
        if (window.alternative) {
            detailUrl = addParamToUrl(detailUrl, 'alternative', 'true');
        }

        var mapType = '${mapType}';
        if (mapType != '') {
            detailUrl = addParamToUrl(detailUrl, 'mapType', mapType);
        }

        return detailUrl;
    }
		

	function fillInfoWindow(poi, clickEvent) {
		const observationDiv = '' + new Date().getTime();
		const footerDiv = 'footer' + observationDiv;
		retrieveLastObservations(poi, observationDiv, footerDiv);
	}

	function fillInfoWindowHeader(poi) {
		const provider = poi.options.id.split('.')[0];
		const name = poi.options.name;
		const poiIcon = '${iconsPath}/' + poi.options.title + '.png';
		const headerTypeStyle = (poi.options.type.length > 12 ? "min-width:250px" : '');
		let poiTypeName = componentTypes[poi.options.type].name.trim(); 
		

		if (!poiTypeName) {
			// Workaround for those cases where the componentType has not i18n literal
			poiTypeName = poi.type;
		}

		let headerContent = '<div class="infobox-header">';
		headerContent += '<div class="infobox-header-icon"><img src="' + poiIcon + '"/></div>';
		headerContent += '<div class="infobox-header-name"><p>' + name + '</p></div>';
		headerContent += '<div class="infobox-header-type" style="' + headerTypeStyle + '"><p>' + poiTypeName + '</p></div>';
		headerContent += '<h3>' + provider + '</h3>';
		headerContent += '</div>';

		return headerContent;
	}

	function fillInfoWindowBody(poi, bodyContent, observationDiv, bodyStyle) {
		return '<div class="infobox-body" id="' + observationDiv + '" style="' + bodyStyle + '">' + bodyContent + '</div>';

	}

	function fillInfoWindowFooter(poi, lastUpdateText, extendedUrl, footerDiv) {
		let footerContent = '<div class="infobox-footer" id="' + footerDiv + '">';
		footerContent += '<span class="pull-left">'+ lastUpdateText +'</span>';
		footerContent += '<span>&nbsp;</span><span class="pull-right"><a href="#" id="componentSeeMoreLink">'+componentSeeMore+'</a></span>';
		footerContent += '</div>';
		return footerContent;
		
	}

	/**
	 * Return the number used to extend map bounds when server search must be done.
	 * This number depends on the map context:
	 *	1. If routes must be displayed or if components with multi coordinates must be displayed as
	 * polylines or polygons, then map bounds must be extended to not hide polylines or routes that have one part
	 * on the map and the other out it (number == 2).
	 *  2. Otherwise, when only pois are displayed on the map, map bounds are not changed (number == 1).
	 */
	function getExtendFactor(){
		return (showRoutes || (showMultiCoordinates && !multiCoordinatesAsPois) ? 2 : 1);
	}
</script>

<%--  include_script_map_sidebar.jsp only must be included in both main visor page and route page --%>
<c:if test = "${not empty currentRequestMapping and ((fn:endsWith(currentRequestMapping, '/component/map')) || (fn:endsWith(currentRequestMapping, '/component/map/route')))}">
  <%@include file="/WEB-INF/jsp/common/include_script_map_sidebar.jsp" %>
</c:if>

<c:choose>
	<c:when test="${(provider_map == 'gmaps') || (forceGMapsProvider == 'true')}">
		<%@include file="/WEB-INF/jsp/common/include_script_maps_gmaps.jsp" %>
	</c:when>
	<c:otherwise>
		<%@include file="/WEB-INF/jsp/common/include_script_maps_leaflet.jsp" %>        
	</c:otherwise>
</c:choose>

<%@include file="/WEB-INF/jsp/common/include_script_maps_config.jsp"%>
