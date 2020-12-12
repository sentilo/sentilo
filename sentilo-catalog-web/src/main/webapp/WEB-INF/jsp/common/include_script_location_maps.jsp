<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<%@include file="/WEB-INF/jsp/common/include_script_maps.jsp" %>

<script>
    //The locations array contains the set of text locations  making up the polyline.
    var locations = [];

    // The markers array contains the actual Marker objects.
    var markers = [];
    var displayPath;
    var selectedMarker = null;

    function initLocationControls() {
        var lat = $('#txtlatitude');
        var lon = $('#txtlongitude');
        var locaddr = $('#locationaddress');
        var mapCanvas = $('#input_location_map_canvas');
        var mapControls = $('#map_controls');
        var mobile = $('#mobile');

        if (mobile.val() == '0' || mobile.val() == 'false') {
            $('#btnStatic').addClass('active');
            toggleMobile(0);
        } else {
            $('#btnMobile').addClass('active');
            toggleMobile(1);
        }

        mapCanvas.show();
        mapControls.show();
        initializeInputLocationMap();

        lat.focusout(updateAddressAndPosition);
        lon.focusout(updateAddressAndPosition);
    }

    function toggleMobile(value) {
        $('#mobile').val(value);
    }



    /**
     * Adds a new location into the map. Gets the location coordinates from the values filled in the input fields txtlatitude and txtlongitude
     */
    function addLocationFromInput() {
        var lat = $('#txtlatitude').val();
        var pLat = parseFloat(lat);

        if (!$.isNumeric(lat) || pLat < -90 || pLat > 90) {
            alert(latitudeErrorMsg);
            return;
        }

        var lng = $('#txtlongitude').val();
        var pLong = parseFloat(lng);

        if (!$.isNumeric(lng) || pLong < -180 || pLong > 180) {
            alert(longitudeErrorMsg);
            return;
        }

        addLocation(lat, lng);
    }


    /**
     * Returns the index of the marker in the polyline.
     */
    function findMarkerIndex(locationMarker) {
        let index = -1;

        for (let  i = 0; i < markers.length; ++i) {
            if (markers[i] == locationMarker) {
                index = i;
                break;
            }
        }

        return index;
    }

    function initComponentLocation(){
        <c:if test="${mode == 'edit' && fn:length(component.location.coordinates) eq 1}">
	        $('#txtlatitude').val(${component.location.centroid[1]});
	        $('#txtlongitude').val(${component.location.centroid[0]});
	        updateAddressAndPosition();
        </c:if>

        <c:if test="${mode == 'edit' && fn:length(component.location.coordinates) ge 1}">
	        <c:forEach var="lngLat" items="${component.location.coordinates}">
	        	addLocation('${lngLat.latitude}', '${lngLat.longitude}');
	        </c:forEach>
	        // Finally, center map at the centroid
	        centerMap(buildLatLng(${component.location.centroid[1]}, ${component.location.centroid[0]}))
        </c:if>
    }

    function updateAddressAndPosition() {
        const lat = $('#txtlatitude');
        const lon = $('#txtlongitude');
        if (lat.val() && isValidDecimalNumber(lat.val()) && lon.val() && isValidDecimalNumber(lon.val())) {
            const location = buildLatLng(lat.val(), lon.val());
            geocodeAddress(location);
            updateSearchedPosition(location);
        }
    }

    /**
     * Highlights the location specified by index in both the map and the location list.
     */
    function highlight(index) {
        if (selectedMarker == null) {
            selectedMarker = markers[index];
        }
        if (selectedMarker != markers[index]) {
            selectedMarker.setIcon(locationIcon);
        }

        markers[index].setIcon(highlightIcon);
        selectedMarker = markers[index];

        // Mark which location is selected.
        if (index < locations.length) {
            locations.selected = index;
            document.getElementById('locations').options[index].selected = true;
        }
    }

</script>

<c:choose>
    <c:when test="${provider_map == 'gmaps'}">
        <%@include file="/WEB-INF/jsp/common/include_script_location_gmaps.jsp"%>
    </c:when>
    <c:otherwise>
        <%@include file="/WEB-INF/jsp/common/include_script_location_leaflet.jsp"%>
    </c:otherwise>
</c:choose>