<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<%@include file="/WEB-INF/jsp/common/messages.jsp"%>

<spring:url value="/static/img/icons" var="iconsPath" />

<script type="text/javascript">

function returnToComponentMap(){
	var componentMapUrl = '${componentMap}';
	var centerLatValue = parseToFloat('<c:out value="${param.lat}" />'); 
	var centerLngValue = parseToFloat('<c:out value="${param.lng}" />'); 
	var zoomValue = parseToInteger('<c:out value="${param.zoom}" />');			
	var filterValue = '<c:out value="${param.filter}" />';
	var alternativeValue = '${alternative}'
	var mapType = '${param.mapType}';

	if(mapType!=''){
		componentMapUrl += '/'+ mapType;
	}
	
	componentMapUrl = addParamToUrl(componentMapUrl, 'zoom', zoomValue);
	componentMapUrl = addParamToUrl(componentMapUrl, 'lat', centerLatValue);
	componentMapUrl = addParamToUrl(componentMapUrl, 'lng', centerLngValue);
	componentMapUrl = addParamToUrl(componentMapUrl, 'filter', filterValue);	
	
	if(alternativeValue){
		componentMapUrl = addParamToUrl(componentMapUrl, 'alternative', 'true');
	}
	
	window.location.href = componentMapUrl;
}

$(document).ready(function() {	
	var latitude = ${component.location.centroid[1]};
	var longitude = ${component.location.centroid[0]} 		
	createComponentHeader(latitude, longitude, '#componentHeader');	
});

</script>



<c:if test="${btnclose ne 'off'}">	
	<div class="pull-right">			
		<a href="javascript: returnToComponentMap();" class="btn btn-danger"><i	class="icon-remove icon-white"></i> Close</a>
	</div>	
</c:if>		
<div class="">
	<div class="pull-left type-icon">
		<img src="${iconsPath}/${componentIcon}.png">
	</div>
	<h1 class="lead">
		<div class="pull-left">
				<span id="componentHeader"></span><br/>
				<small> ${component.name} / ${component.componentType} / ${component.providerId} </small><br/>					 															
		</div>
	</h1>	
</div>
<div class="pull-right tags">
	<c:if test="${not empty component.tagsAsList}">
		<i class="icon-tags"></i>
	</c:if>
	<c:forEach items="${component.tagsAsList}" var="tag">
		<span class="badge">${tag}</span>
	</c:forEach>
</div>
	


