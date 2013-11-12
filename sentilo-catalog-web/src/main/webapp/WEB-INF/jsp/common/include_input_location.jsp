<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>
<%@include file="/WEB-INF/jsp/common/include_script_maps.jsp"%>

<script type="text/javascript">

function updateAddressAndPosition() {
	var lat = $('#latitude');
	var lon = $('#longitude');
	if (lat.val() && isValidDecimalNumber(lat.val()) && lon.val() && isValidDecimalNumber(lon.val())) {
		var location = new google.maps.LatLng(lat.val(), lon.val());
		geocodeAddress(location);
		updateSearchedPosition(location);
	}
}

function toggleMobile(value) {
	var lat = $('#latitude');
	var lon = $('#longitude');
	var locaddr = $('#locationaddress');
	var mapCanvas = $('#input_location_map_canvas');
	var mapControls = $('#map_controls');

	if (value == 0) {
		lat.attr('disabled', false);
		lon.attr('disabled', false);
		locaddr.attr('disabled', false);
		mapCanvas.show();
		mapControls.show();
		initializeInputLocationMap();
	} else {
		lat.attr('disabled', true);
		lon.attr('disabled', true);
		locaddr.attr('disabled', true);
		mapControls.hide();
		mapCanvas.hide();
	}
		
	lat.focusout(updateAddressAndPosition);
	lon.focusout(updateAddressAndPosition);

	$('#mobile').val(value);
};

function initializeMobile() {
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

$(document).ready(function() {
	initializeMobile();
});
</script>

<div class="control-group">
	<div class="controls">
		<div data-toggle="buttons-radio" class="btn-group">
			<button type="button" id="btnStatic" class="btn" onclick="toggleMobile(0);"><spring:message code="static" /></button>
			<button type="button" id="btnMobile" class="btn" onclick="toggleMobile(1);"><spring:message code="mobile" /></button>
		</div>
		<form:hidden path="mobile" id="mobile"/>
	</div>
</div>

<div class="control-group">
	<form:label path="location.latitude" class="control-label">
		<spring:message code="location.latitude" />
	</form:label>
	<div class="controls">
		<form:input path="location.latitude" disabled="true" id="latitude"/>
		<form:errors path="location.latitude" cssClass="text-error" htmlEscape="false" />
	</div>
</div>
<div class="control-group">
	<form:label path="location.longitude" class="control-label">
		<spring:message code="location.longitude" />
	</form:label>
	<div class="controls">
		<form:input path="location.longitude" disabled="true" id="longitude"/>
		<form:errors path="location.longitude" cssClass="text-error" htmlEscape="false" />
	</div>
</div>
<div class="control-group">
	<label for="locationaddress" class="control-label">
		<spring:message code="location.address" />
	</label>
	<div class="controls">
		<input type="text" id="locationaddress" disabled="disabled" />
	</div>
</div>
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

