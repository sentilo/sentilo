<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>
<%@include file="/WEB-INF/jsp/common/include_script_maps.jsp"%>

<script type="text/javascript">

//The locations array contains the set of text locations  making up the polyline.
var locations = [];
// The latlngs array contains the set of LatLng objects.
var latlngs = new google.maps.MVCArray();

// The markers array contains the actual Marker objects.
var markers = [];
var displayPath;
var selectedMarker = null;

// The tempMarker is a marker showing a candidate location.
var tempMarker = new google.maps.Marker();

var highlightIcon = {
  url: "https://maps.gstatic.com/mapfiles/ridefinder-images/mm_20_yellow.png",
  size: new google.maps.Size(12,20),
  origin: new google.maps.Size(6,20)
};

var tempIcon = {
  url: "https://maps.gstatic.com/mapfiles/ridefinder-images/mm_20_green.png",
  size: new google.maps.Size(12,20),
  origin: new google.maps.Size(6,20)
};

var locationIcon = {
  url: "https://maps.gstatic.com/mapfiles/ridefinder-images/mm_20_blue.png",
  size: new google.maps.Size(12,20),
  origin: new google.maps.Size(6,20)
};

function updateAddressAndPosition() {	
	var lat = $('#txtlatitude');
	var lon = $('#txtlongitude');
	if (lat.val() && isValidDecimalNumber(lat.val()) && lon.val() && isValidDecimalNumber(lon.val())) {
		var location = new google.maps.LatLng(lat.val(), lon.val());
		geocodeAddress(location);
		//updateSearchedPosition(location);
	}
}

function toggleMobile(value) {
	var lat = $('#txtlatitude');
	var lon = $('#txtlongitude');
	var locaddr = $('#locationaddress');
	var mapCanvas = $('#input_location_map_canvas');
	var mapControls = $('#map_controls');
	
	mapCanvas.show();
	mapControls.show();
	initializeInputLocationMap();	
		
	lat.focusout(updateAddressAndPosition);
	lon.focusout(updateAddressAndPosition);

	$('#mobile').val(value);
};

function initializeMobileControls() {
	var mobile = $('#mobile');
	if (mobile.val() == '0' || mobile.val() == 'false') {
		$('#btnStatic').addClass('active');
		toggleMobile(0);
		updateAddressAndPosition();
	} else {
		$('#btnMobile').addClass('active');
		toggleMobile(1);
	}
};

function initializeLocation(){		
	<c:if test="${mode == 'edit' && fn:length(component.location.coordinates) eq 1}">
	 	$('#txtlatitude').val(${component.location.centroid[1]});
	 	$('#txtlongitude').val(${component.location.centroid[0]});
	 //updateAddressAndPosition();
	</c:if>
	
	<c:if test="${mode == 'edit' && fn:length(component.location.coordinates) ge 1}">
		<c:forEach var="lngLat" items="${component.location.coordinates}">
			addLocation('${lngLat.latitude}', '${lngLat.longitude}');			
		</c:forEach>
		// Finally, center map at the centroid
		map.setCenter(new google.maps.LatLng(${component.location.centroid[1]}, ${component.location.centroid[0]}));
	</c:if>
}

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
 
// Diplay component path on Map and initialize custom map events like click, dbclick, drag, ..
function initializeComponentMapElements() {          
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
	var location = new google.maps.LatLng($('#txtlatitude').val(), $('#txtlongitude').val());
	geocodeAddress(location);
}

//Add a location to the location list.
function addLocationFromInput() {
  var lat = $('#txtlatitude').val();
  var pLat = parseFloat(lat);

  if (pLat.toString() != lat || pLat < -90 || pLat > 90) {
    alert('Invalid latitude entered. Must be in range of -90 to 90');
    return;
  }  

  var lng = $('#txtlongitude').val();
  var pLong = parseFloat(lng);

  if (pLong.toString() != lng || pLong < -180 || pLong > 180) {
    alert('Invalid longitude entered. Must be in range of -180 to 180');
    return;
  }    

  addLocation(lat, lng);
}

function addLocation(lat, lng) {
    var newLocation = new google.maps.LatLng(lat, lng);
    markers.push(createLocationMarker(newLocation));
    addLocationToList(lat, lng);
    latlngs.push(newLocation);    
    displayPath.setPath(latlngs);        
 }
 
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

//Returns the index of the marker in the polyline.
function findMarkerIndex(locationMarker) {
  var index = -1;

  for (var  i = 0; i < markers.length; ++i) {
    if (markers[i] == locationMarker) {
      index = i;
      break;
    }
  }

  return index;
}

// Highlights the location specified by index in both the map and the location list.
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

//Delete a location
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

//Delete *all* the locations from the polyline, with confirmation dialog before
// deletion.
function deleteAllLocations() {
  var deleteConfirm = confirm("Are you sure you want to remove all the locations from this component?");

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

google.maps.Polyline.prototype.getBounds = function() {
    var bounds = new google.maps.LatLngBounds();
    this.getPath().forEach(function(e) {
      bounds.extend(e);
    });
    return bounds;
};

  

 // Move the map to the selected location in the location list.
 function jumpToLocation() {
   var locationList = document.getElementById('locations');
   if (locationList.selectedIndex >= 0) {
     var location = locations[locationList.selectedIndex];
     map.setCenter(new google.maps.LatLng(location.Latitude, location.Longitude));
   }
 }

$(document).ready(function() {			
	initializeMobileControls();		
	initializeComponentMapElements();		
	initializeLocation();	
	
	$('form#component').submit(function( event ) {
		// Before submit content form, update location hidden field with the new coordinates of the component		  
		$('#location').val(displayPath.getPath().getArray().toString());
		return;
	});
	
});
</script>

<div class="control-group">
	<div class="controls">
		<div data-toggle="buttons-radio" class="btn-group">
			<button type="button" id="btnStatic" class="btn" onclick="toggleMobile(0);">
				<spring:message code="static" />
			</button>
			<button type="button" id="btnMobile" class="btn" onclick="toggleMobile(1);">
				<spring:message code="mobile" />
			</button>
		</div>
		<form:hidden path="mobile" id="mobile" />
	</div>
</div>

<!--  Coordinates list block  -->
<!--  location list se debe rellenar con el contenido de component.location.coordinates -->

<div class="control-group">
	<label for="locationaddress" class="control-label"> <spring:message code="location.address" /> </label>
	<div class="controls">
		<input type="text" id="locationaddress" />
	</div>
</div>

<div class="control-group">
	<label for="txtlatitude" class="control-label"> <spring:message code="location.latitude" /> </label>	
	<div class="controls">
		<input type="text" id="txtlatitude" maxlength="11"/>
	</div>	
</div>

<div class="control-group">
	<label for="txtlongitude" class="control-label"> <spring:message code="location.longitude" /> </label>	
	<div class="controls">
		<input type="text" id="txtlongitude" maxlength="11"/>
	</div>	
</div>

<div class="control-group">		
	<div class="controls">
		<input type="button" value="Add Location" class="btn" onclick="addLocationFromInput()"/>
	</div>	
</div>

 <div class="control-group">
    <label for="locations" class="control-label"> Locations list </label>
    <div class="controls">
    	<form:hidden path="location" />
    	<select id="locations" size="4" class="input-large" style="width:310px" onchange="highlight(this.selectedIndex)" ondblclick="jumpToLocation()"></select>
    </div>
    <div class="controls">
    	<input type="button" value="Delete Selected Location" class="btn" onclick="deleteLocation()"/>
      	<input type="button" value="Delete All Locations" class="btn" onclick="deleteAllLocations()"/>
    </div>        
</div>

<!-- Map block -->
<div class="control-group">
	<div class="controls">
		<div id="map_controls" class="hide">
			<form class="form-inline row-fluid">
				<button id="locate" class="btn" type="button" onclick="">
					<i class="connecta-icon-location">&nbsp;&nbsp;&nbsp;</i>
				</button>
			</form>
		</div>

		<div id="input_location_map_canvas" class="input_location_map" style="width: 100%;">Map placeholder</div>
	</div>
</div>

