<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@ include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<%@ include file="/WEB-INF/jsp/common/include_common_messages.jsp"%>

<spring:eval expression="@catalogConfigProperties.getProperty('catalog.map.provider')" var="provider_map" />
<spring:eval expression="@catalogConfigProperties.getProperty('catalog.map.mapbox.home.style')" var="mapboxStyle" />
<spring:eval expression="@catalogConfigProperties.getProperty('catalog.map.geocode.onMapMove','true')" var="geocodeOnMapMove" />


<spring:url value="/component" var="publicComponentDetailUrl" />
<spring:url value="/component/map" var="componentMap" />
<spring:url value="/static/img/icons" var="iconsPath" />
<spring:url value="/static/img/spot.png" var="imgSpot" />
<spring:url value="/static/img/" var="imgsPath" />
<spring:url value="/static/img/icons/pins6-poi.png" var="defaultIcon" />

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
    	var componentDescription = "${component.description}";
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
    		infowindow.close(map, this);
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
			if (status == isGeocodeSuccess(results, status)) {
				$('#locationaddress').val(formatGeocodeAddress(results));
			}
		};
		geocoder.geocode({'latLng' : location}, success);
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
			setComponentDetailUrl(detailUrl);

			var numRows = parseInt((elementsCount / 2) + (elementsCount % 2));
			var altClass = (numRows <= 1) ? "countA" : (numRows == 2) ? "countB" : "countC";

			var componentId = poi.options.id;
			var content = '<a href="#" onclick="updateModalLayer(\'' + componentId + '\'); infowindowMustBeClosed=false;">';

			// Decide witch detail to show
			// For small devices, or not desired cases, uses old detail page
			if (window.innerWidth < smallDevicesWidth || useDetailedFrontView !== undefined && useDetailedFrontView === false) {
				content = '<a href="' + detailUrl + '">';
			}

			content += '<div class="infobox ' + altClass + '" id="infobox">';
			content += '<div class="infobox-content" style="display:block">';
			content += fillInfoWindowHeader(poi);
			content += fillInfoWindowBody(poi, bodyContent, observationDiv, bodyStyle);
			content += fillInfoWindowFooter(poi, data.lastUpdateTimeMessage, footerDiv);
			content += '</div>';
			content += '<div class="poi-infowindow-arrow"></div>';
			content += '</div>';

			if (window.innerWidth < smallDevicesWidth || useDetailedFrontView !== undefined && useDetailedFrontView === false) {
				content += '</a>';
			}

			buildPopup(content, poi);

		});
	};

	function fillInfoWindow(poi, clickEvent) {
		const observationDiv = '' + new Date().getTime();
		const footerDiv = 'footer' + observationDiv;
		retrieveLastObservations(poi, observationDiv, footerDiv);
	}

	function fillInfoWindowHeader(poi) {
		const provider = poi.options.id.split('.')[0];
		const name = poi.options.id.split('.')[1];
		const poiIcon = '${iconsPath}/' + poi.options.title + '.png';
		const headerTypeStyle = (poi.options.type.length > 12 ? "min-width:250px" : '');
		let poiTypeName = $("#componentDropdown").find("li#" + poi.options.type).find("a").text().trim();

		if (!poiTypeName) {
			// Workaround for those cases where the componentType has not i18n literal
			poiTypeName = poi.type;
		}

		var content = '<div class="infobox-header">';
		content += '<div class="infobox-header-icon"><img src="' + poiIcon + '"/></div>';
		content += '<div class="infobox-header-name"><p>' + name + '</p></div>';
		content += '<div class="infobox-header-type" style="' + headerTypeStyle + '"><p>' + poiTypeName + '</p></div>';
		content += '<h3>' + provider + '</h3>';
		content += '</div>';

		return content;
	}

	function fillInfoWindowBody(poi, bodyContent, observationDiv, bodyStyle) {
		var content = '<div class="infobox-body" id="' + observationDiv + '" style="' + bodyStyle + '">' + bodyContent + '</div>';
		return content;
	}

	function fillInfoWindowFooter(poi, footerContent, footerDiv) {
		var content = '<div class="infobox-footer" id="' + footerDiv + '">' + footerContent + '</div>';
		return content;
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

<c:choose>
	<c:when test="${provider_map == 'gmaps'}">
		<%@include file="/WEB-INF/jsp/common/include_script_maps_gmaps.jsp" %>
	</c:when>
	<c:otherwise>
		<%@include file="/WEB-INF/jsp/common/include_script_maps_leaflet.jsp" %>        
	</c:otherwise>
</c:choose>


<%@include file="/WEB-INF/jsp/common/include_script_maps_config.jsp"%>

