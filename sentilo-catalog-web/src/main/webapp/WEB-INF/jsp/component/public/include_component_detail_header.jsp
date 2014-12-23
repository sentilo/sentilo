<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>
<spring:url value="/static/img/icons" var="iconsPath" />

<script type="text/javascript">

function geocodeAddress(latitude, longitude, target) {
	var geo = new google.maps.Geocoder();
	geo.geocode({
			'latLng' : new google.maps.LatLng(latitude, longitude)
		},
		function (results, status) {
			if (status == google.maps.GeocoderStatus.OK && results[0]) {
				$(target).html(results[0].formatted_address);
			}
		}
	);
}

function returnToComponentMap(){
	var componentMapUrl = '${componentMap}';
	var centerLatValue = '${param.lat}';
	var centerLngValue = '${param.lng}';
	var zoomValue = '${param.zoom}';
	var filterValue = '${param.filter}';
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
	
	geocodeAddress( latitude, longitude,'#componentAddress');
});

</script>

<div class="row-fluid">
	<div class="span12">
		<%@include file="/WEB-INF/jsp/common/messages.jsp"%>
		<br /> <br />
		<div class="pull-right">			
			<a href="javascript: returnToComponentMap();" class="btn btn-danger"><i	class="icon-remove icon-white"></i> Close</a>
		</div>
		<br />
		<div class="">
			<div class="pull-left type-icon">
				<img src="${iconsPath}/${componentIcon}.png">
			</div>
			<h1 class="lead">
				<div class="pull-left">
					${component.providerId} <br />
					${component.componentType} <br /> 
					${component.name} <br />
						 <small> <span id="componentAddress"></span>
					<div class="pull-right">
						<c:if test="${not empty component.tagsAsList}">
							<i class="icon-tags"></i>
						</c:if>
						<c:forEach items="${component.tagsAsList}" var="tag">
							<span class="badge">${tag}</span>
						</c:forEach>
					</div> </small>
			</div>
			</h1>
		</div>
	</div>
</div>


