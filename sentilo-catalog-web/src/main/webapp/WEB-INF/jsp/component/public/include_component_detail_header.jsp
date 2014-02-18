<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

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

$(document).ready(function() {
	geocodeAddress(
			${component.location.latitude},
			${component.location.longitude},
			'#componentAddress'
	);
});

</script>

<div class="row-fluid">
	<div class="span12">
		<%@include file="/WEB-INF/jsp/common/messages.jsp"%>
		<br /> <br />
		<div class="pull-right">
			<spring:url value="/component/map" var="publicMapURL" />
			<a href="${publicMapURL}<c:if test="${alternative eq 'true'}">?alternative=true</c:if>" class="btn btn-danger"><i
				class="icon-remove icon-white"></i> Close</a>
		</div>
		<br />
		<div class="">
			<div class="pull-left type-icon">
				<img src="../../static/img/pins${componentIcon}.png">
			</div>
			<h1 class="lead">
				${component.name} <br /> <small> <span id="componentAddress"></span>
					<div class="pull-right">
						<c:if test="${not empty component.tagsAsList}">
							<i class="icon-tags"></i>
						</c:if>
						<c:forEach items="${component.tagsAsList}" var="tag">
							<span class="badge">${tag}</span>
						</c:forEach>
					</div> </small>
			</h1>
			<div class="pull-right">
				<span id="detailComponentId"><spring:message code="id" /> ${component.id}</span>
			</div>
		</div>
	</div>
</div>
