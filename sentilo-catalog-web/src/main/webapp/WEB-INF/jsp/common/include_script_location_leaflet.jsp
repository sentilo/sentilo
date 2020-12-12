<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<script type="text/javascript">

    // The latlngs array contains the set of LatLng objects.
    var latlngs = [];

    // The tempMarker is a marker showing a candidate location.
    var tempMarker = L.marker([], {draggable: true});

    var highlightIcon = L.icon({
        iconUrl: 'https://maps.gstatic.com/mapfiles/ridefinder-images/mm_20_yellow.png',
        iconSize: [12, 20]
    });

    var tempIcon = L.icon({
        iconUrl: 'https://maps.gstatic.com/mapfiles/ridefinder-images/mm_20_green.png',
        iconSize: [12, 20]
    });

    var locationIcon = L.icon({
        iconUrl: 'https://maps.gstatic.com/mapfiles/ridefinder-images/mm_20_blue.png',
        iconSize: [12, 20]
    });

    //Creates a location and adds it to the list.
    function addLocationToList(lat, lng) {

        // Add the location to the select element
        var o = new Option('(' + lat + ',' + lng + ')', locations.length);
        /// To fix bug in IE8: jquerify the DOM object 'o' so we can use the html method
        $(o).html('(' + lat + ',' + lng + ')');
        $("#locations").append(o);

        // Create a text location for the array locations
        locations.push(L.latLng(lat, lng));
    }

    // Initialize custom map events like click, dbclick, drag, ..
    function initMapElements() {
        tempMarker.setIcon(tempIcon);
        map.on('click', function (e) {
            showTempMarker(e);
        });
        displayPath = L.polyline(latlngs, {
            color: '#FF0000',
            opacity: 1.0,
            weight: 2
        }).addTo(map);
    }

    function showTempMarker(e) {
        const location = buildLatLng(e.latlng.lat, e.latlng.lng);

        tempMarker.setLatLng(location);

        $('#txtlatitude').val(location.lat);
        $('#txtlongitude').val(location.lng);
        updateLocationFieldsFromTempMarkerPosition();

        // Update locationaddress, txtlatitude and txtlongitude fields  after tempMarker is draged
        tempMarker.on('dragend', function (e) {
            updateLocationFieldsFromTempMarkerPosition();
        })

        tempMarker.addTo(map);
    }

    function updateLocationFieldsFromTempMarkerPosition(){
        $('#txtlatitude').val(tempMarker.getLatLng().lat);
        $('#txtlongitude').val(tempMarker.getLatLng().lng);
        const location = buildLatLng($('#txtlatitude').val(), $('#txtlongitude').val());
        geocodeAddress(location);
    }

    /**
     * Adds a new location into the map. Gets two params (lat, lng) with the location coordinates
     * @param lat Latitude
     * @param lng Longitude
     */
    function addLocation(lat, lng) {
        const newLocation = buildLatLng(lat, lng);
        markers.push(createLocationMarker(newLocation));
        addLocationToList(lat, lng);
        latlngs.push(newLocation);
        displayPath.setLatLngs(latlngs);
    }

    /**
     * Adds a new marker on the map associated with the position given by param
     * @param location Initial position where marker must be located
     */
    function createLocationMarker(location) {
        if (tempMarker) {
            map.removeLayer(tempMarker);
        }

        let locationMarker = L.marker(location, {
            draggable: true,
            icon: locationIcon
        }).addTo(map);

        locationMarker.on('click', function () {
            highlight(findMarkerIndex(locationMarker));
        });

        locationMarker.on('drag', function () {
            const index = findMarkerIndex(locationMarker);

            if (index >= 0) {
                const nLatLng = locationMarker.getLatLng();
                latlngs.splice(index, 1, nLatLng);

                const nLat = nLatLng.lat;
                const nLng = nLatLng.lng;
                const modifiedLocation = L.latLng(nLat, nLng);

                displayPath.setLatLngs(latlngs);
                locations[index] = modifiedLocation;
                document.getElementById('locations').options[index] = new Option('(' + nLat + ',' + nLng + ')', index);
                // highlight(index);
            }
        });

        locationMarker.on('dragend', function () {
           console.log(locationMarker.getLatLng());
        });

        return locationMarker;
    }

    /**
     * Removes the selected location from the location list
     */
    function deleteLocation() {
        if (locations.length > 0) {
            let locationToRemove = document.getElementById('locations').selectedIndex;

            if (locationToRemove >= 0 && locationToRemove < locations.length) {
                locations.splice(locationToRemove, 1);

                if (selectedMarker == markers[locationToRemove]) {
                    selectedMarker = null;
                }

                map.removeLayer(markers[locationToRemove]);
                markers.splice(locationToRemove, 1);
                document.getElementById('locations').options[locationToRemove] = null;
                latlngs.splice(locationToRemove, 1);
                displayPath.setLatLngs(latlngs);
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
                map.removeLayer(markers[i]);
            }
            markers = [];
            latlngs = [];
            displayPath.setLatLngs(latlngs);
        }
    }

    /**
     * Move the map to the selected location in the location list.
     */
    function jumpToLocation() {
        var locationList = document.getElementById('locations');
        if (locationList.selectedIndex >= 0) {
            const location = locations[locationList.selectedIndex];
            centerMap(buildLatLng(location.lat, location.lng));
        }
    }

    $(document).ready(function() {
        initLocationControls();
        initMapElements();
        initComponentLocation();

        $('form#component').submit(function( event ) {
            function parsePath(path) {
                return path.getLatLngs().toString().split('LatLng').join('');
            }
            // Before submit content form, update location hidden field with the new coordinates of the component
            if(displayPath && displayPath.getLatLngs()){
                $('#location').val(parsePath(displayPath));
            }

        });
    });
</script>
