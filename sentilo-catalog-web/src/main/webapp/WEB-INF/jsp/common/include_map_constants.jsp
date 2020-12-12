<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@ include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<spring:url value="/static/img/icons" var="iconsPath" />
<spring:url value="/component/map" var="componentMap"/>

<script>
    window.messages = {
        boolValues: {
            falseValue: '<spring:message code="false"/>',
            trueValue: '<spring:message code="true"/>'
        }
    };

    // Show detailed modal view on component map
    var useDetailedFrontView = true;

    // the initial map center (Barcelona city)
    var defaultMapCenter = [41.4001221, 2.172839];
    var defaultZoomLevel = 14;
    var defaultInputLocationZoomLevel = 14;

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

    function buildJsonComponentsUrl() {
        var urlToCall = '${componentMap}';
        var mapType = '${mapType}';
        if (mapType != '') {
            urlToCall += '/' + mapType;
        }

        urlToCall += '/json';

        return urlToCall;
    }
</script>
