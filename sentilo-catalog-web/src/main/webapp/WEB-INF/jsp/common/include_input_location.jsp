<%@ page contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<%@include file="/WEB-INF/jsp/common/include_script_location_maps.jsp"%>

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
<div class="control-group">
	<label for="locationaddress" class="control-label"> <spring:message code="location.address" /> </label>
	<div class="controls">
		<input type="text" id="locationaddress"/>
		<a id="locate" class="btn">
			<i class="connecta-icon-location">&nbsp;&nbsp;&nbsp;</i>
		</a>
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
		<input type="button" value="<spring:message code="location.button.addLocation"/>" class="btn" onclick="addLocationFromInput()"/>
	</div>	
</div>

<div class="control-group">
    <label for="locations" class="control-label"> <spring:message code="location.locations.list"/></label>
    <div class="controls">
    	<form:hidden path="location" />
    	<select id="locations" size="4" class="input-large" style="width:310px" onchange="highlight(this.selectedIndex)" ondblclick="jumpToLocation()"></select>
    	<span class="text-error"><form:errors path="location" /></span>
    </div>
</div>
 
<div class="control-group">
    <div class="controls">
    	<input type="button" value="<spring:message code="location.button.delete"/>" class="btn" onclick="deleteLocation()"/>
      	<input type="button" value="<spring:message code="location.button.delete.all"/>" class="btn" onclick="deleteAllLocations()"/>
    </div>        
</div>

<!-- Map block -->
<div class="control-group">
    <div class="controls">
		<div id="input_location_map_canvas" class="input_location_map" style="width: 100%;">Map placeholder</div>
	</div>
</div>
