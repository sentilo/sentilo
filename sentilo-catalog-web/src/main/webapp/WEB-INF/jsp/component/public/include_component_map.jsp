<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<div id="map_controls" class="hide">
	<form class="form-inline row-fluid">		
		<div class="input-prepend input-append" id="geocode">
			<span class="add-on"> <i class="icon-search"></i> </span>
			<spring:message code="component.map.address.search.instructions" var="instructions" />
			<input id="address" class="" placeholder="${instructions}" type="text" data-provide="typeahead" autocomplete="off" />
		</div>
		<button id="locate" class="btn" type="button">
			<i class="connecta-icon-location">&nbsp;&nbsp;&nbsp;</i>
		</button>
	</form>
</div>

<div id="map_canvas_1" class="${mapClass}">
	<c:if test="${includeBlueScreen}">
		<div id="bgHomeScreen"></div>
	</c:if>
</div>
