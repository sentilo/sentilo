<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<script type="text/javascript">

    // The latlngs array contains the set of LatLng objects.
    var latlngs = new google.maps.MVCArray();

    // The tempMarker is a marker showing a candidate location.
    var tempMarker = new google.maps.Marker({
        draggable: true
    });

    var highlightIcon = {
        url: "https://maps.gstatic.com/mapfiles/ridefinder-images/mm_20_yellow.png",
        size: new google.maps.Size(12,20),
        origin: new google.maps.Point(0,0)
    };

    var tempIcon = {
        url: "https://maps.gstatic.com/mapfiles/ridefinder-images/mm_20_green.png",
        size: new google.maps.Size(12,20),
        origin: new google.maps.Point(0,0)
    };

    var locationIcon = {
        url: "https://maps.gstatic.com/mapfiles/ridefinder-images/mm_20_blue.png",
        size: new google.maps.Size(12,20),
        origin: new google.maps.Point(0,0)
    };

    google.maps.Polyline.prototype.getBounds = function() {
        var bounds = new google.maps.LatLngBounds();
        this.getPath().forEach(function(e) {
            bounds.extend(e);
        });
        return bounds;
    };

    //Creates a location and adds it to the list.
    function addLocationToList(lat, lng) {

        // Add the location to the select element
        var o = new Option('(' + lat + ',' + lng + ')', locations.length);
        /// To fix bug in IE8: jquerify the DOM object 'o' so we can use the html method
        $(o).html('(' + lat + ',' + lng + ')');
        $("#locations").append(o);

        // Create a text location for the array locations
        var newLocation = {Latitude: lat, Longitude: lng};
        locations.push(newLocation);
    }

    // Initialize custom map events like click, dbclick, drag, ..
    function initMapElements() {
        tempMarker.setOptions({icon: tempIcon, draggable: true});
        google.maps.event.addListener(map, "click", showTempMarker);
        displayPath = new google.maps.Polyline({
            map: map,
            strokeColor: "#FF0000",
            strokeOpacity: 1.0,
            strokeWeight: 2,
            path: latlngs
        });
    }

    function showTempMarker(e) {
        tempMarker.setPosition(e.latLng);

        $('#txtlatitude').val(e.latLng.lat());
        $('#txtlongitude').val(e.latLng.lng());
        updateLocationFieldsFromTempMarkerPosition();

        // Update locationaddress, txtlatitude and txtlongitude fields  after tempMarker is draged
        google.maps.event.addListener(tempMarker, "dragend", function() {
            updateLocationFieldsFromTempMarkerPosition();
        });

        tempMarker.setMap(map);
    }

    function updateLocationFieldsFromTempMarkerPosition(){
        $('#txtlatitude').val(tempMarker.getPosition().lat());
        $('#txtlongitude').val(tempMarker.getPosition().lng());
        var location = buildLatLng($('#txtlatitude').val(), $('#txtlongitude').val());
        geocodeAddress(location);
    }

    /**
     * Adds a new location into the map. Gets two params (lat, lng) with the location coordinates
     * @param lat Latitude
     * @param lng Longitude
     */
    function addLocation(lat, lng) {
        var newLocation = buildLatLng(lat, lng);
        markers.push(createLocationMarker(newLocation));
        addLocationToList(lat, lng);
        latlngs.push(newLocation);
        displayPath.setPath(latlngs);
    }

    /**
     * Adds a new marker on the map associated with the position given by param
     * @param location Initial position where marker must be located
     */
    function createLocationMarker(location) {
        if (tempMarker) {
            tempMarker.setMap(null);
        }

        var locationMarker = new google.maps.Marker();

        locationMarker.setOptions({
            icon: locationIcon,
            draggable: true,
            map: map,
            position: location
        });

        google.maps.event.addListener(locationMarker, "click", function() {
            highlight(findMarkerIndex(locationMarker));
        });

        google.maps.event.addListener(locationMarker, "drag", function() {
            var index = findMarkerIndex(locationMarker);

            if (index >= 0) {
                var nLatLng = locationMarker.getPosition();
                latlngs.setAt(index, nLatLng);

                var nLat = nLatLng.lat();
                var nLng = nLatLng.lng();
                var modifiedLocation = {
                    Latitude: nLat,
                    Longitude: nLng
                };

                displayPath.setPath(latlngs);
                locations[index] = modifiedLocation;
                document.getElementById('locations').options[index] = new Option('(' + nLat + ',' + nLng + ')', index);
                highlight(index);
            }
        });

        return locationMarker;
    }

    /**
     * Removes the selected location from the location list
     */
    function deleteLocation() {
        if (locations.length > 0) {
            var locationToRemove = document.getElementById('locations').selectedIndex;

            if (locationToRemove >= 0 && locationToRemove < locations.length) {
                locations.splice(locationToRemove, 1);

                if (selectedMarker == markers[locationToRemove]) {
                    selectedMarker = null;
                }

                markers[locationToRemove].setMap(null);
                markers.splice(locationToRemove, 1);
                document.getElementById('locations').options[locationToRemove] = null;

                latlngs.removeAt(locationToRemove);
                displayPath.setPath(latlngs);
            }

            if (locations.length > 0) {
                if (locationToRemove == 0) {
                    locationToRemove++;
                }
            }
        }
    }

    /**
     * Clear the location list
     */
    function deleteAllLocations() {
        var deleteConfirm = confirm(locationDeleteAllConfirmMsg);

        if (deleteConfirm) {
            document.getElementById('locations').options.length = 0;
            locations = [];
            for(var i = 0; i < markers.length; ++i) {
                var markerToRemove = markers[i];
                markerToRemove.setMap(null);
            }
            markers = [];
            latlngs = new google.maps.MVCArray();
            displayPath.setPath(latlngs);
        }
    }

    /**
     * Move the map to the selected location in the location list.
     */
    function jumpToLocation() {
        var locationList = document.getElementById('locations');
        if (locationList.selectedIndex >= 0) {
            const location = locations[locationList.selectedIndex];
            centerMap(buildLatLng(location.Latitude, location.Longitude));
        }
    }

    $(document).ready(function() {
        initLocationControls();
        initMapElements();
        initComponentLocation();

        $('form#component').submit(function( event ) {
            // Before submit content form, update location hidden field with the new coordinates of the component
            if(displayPath && displayPath.getPath()){
                $('#location').val(displayPath.getPath().getArray().toString());
            }

            return;
        });
    });
</script>
