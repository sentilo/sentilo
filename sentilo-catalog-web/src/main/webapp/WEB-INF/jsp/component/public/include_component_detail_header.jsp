<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>

<%@include file="/WEB-INF/jsp/common/messages.jsp"%>

<spring:url value="/static/img/icons" var="iconsPath" />
<spring:url value="/component/map" var="componentMap" />

<c:set var="extNavButtonClass" value="${extendedDetailUrlParam ? \"bold\" : \"\"} "/>
<c:set var="detailNavButtonClass" value="${extendedDetailUrlParam ? \"\" : \"bold\"} "/>

<script type="text/javascript">

function returnToComponentMap(){
	var componentMapUrl = '${componentMap}';
	var centerLatValue = parseToFloat('<c:out value="${param.lat}" />'); 
	var centerLngValue = parseToFloat('<c:out value="${param.lng}" />'); 
	var zoomValue = parseToInteger('<c:out value="${param.zoom}" />');			
	var ct = '<c:out value="${param.ct}" />';
	var alternativeValue = '${alternative}'
	var mapType = '${param.mapType}';

	if(mapType!=''){
		componentMapUrl += '/'+ mapType;
	}
	
	componentMapUrl = addParamToUrl(componentMapUrl, 'zoom', zoomValue);
	componentMapUrl = addParamToUrl(componentMapUrl, 'lat', centerLatValue);
	componentMapUrl = addParamToUrl(componentMapUrl, 'lng', centerLngValue);
	componentMapUrl = addParamToUrl(componentMapUrl, 'ct', ct);	
	
	if(alternativeValue){
		componentMapUrl = addParamToUrl(componentMapUrl, 'alternative', 'true');
	}
	
	window.location.href = componentMapUrl;
}

function goToDetailWithExtendedDetailURL() {
	let url = new URL(window.location.href);
	let params = new URLSearchParams(url.search);
	params.delete('extendedDetailUrl');
	params.append('extendedDetailUrl', true);
	url.search = params.toString();
	window.location.href = url;	
}


function goToDetailWithoutExtendedDetailURL() {
	let url = new URL(window.location.href);
	let params = new URLSearchParams(url.search);
	params.delete('extendedDetailUrl');
	url.search = params.toString();
	window.location.href = url;	
}


$(document).ready(function() {	
	var latitude = ${component.location.centroid[1]};
	var longitude = ${component.location.centroid[0]} 		
	createComponentHeader(latitude, longitude, '#componentHeader');	

	$('#componentTitleHeader').on("mouseenter", function() {
		var title = $('#componentHeader');
		if (this.offsetWidth < this.scrollWidth) {
			title.attr('title', title.text());
        } else {
        	title.removeAttr('title');
        }
    });
})

</script>

<div class="row header-wrapper">
	<div class="title-wrapper span9">
		<div class="type-icon pull-left">
			<img src="${iconsPath}/${componentIcon}.png">
		</div>
		<h1 class="lead" id="componentTitleHeader">
			<span id="componentHeader"></span><br/>
			<small> ${component.friendlyName} / ${component.componentType} / ${component.providerId} </small><br/>
		</h1>
	</div>
	<div class="buttons-wrapper span3">
		<div class="row">
			<c:if test="${btnclose ne 'off'}">	
				<div class="pull-right button-menu">
					<a href="javascript: returnToComponentMap();" class="btn btn-danger"><i	class="icon-remove icon-white"></i> <spring:message code="button.close" /></a>
				</div>	
			</c:if>
			<c:if test="${not empty component.extendedDetailUrl}">	
				<div class="pull-right button-menu extended-button-menu">
					<a href="javascript: goToDetailWithoutExtendedDetailURL();" class="btn btn-primary ${detailNavButtonClass}"><spring:message code="button.detail" /></a>
				</div>
				<div class="pull-right button-menu extended-button-menu">
					<a href="javascript: goToDetailWithExtendedDetailURL();" class="btn btn-primary ${extNavButtonClass}"><spring:message code="component.button.extendedDetail" /></a>
				</div>
			</c:if>
		</div>
		<div class="row">
			<div class="tags-wrapper pull-right">
				<c:if test="${not empty component.tagsAsList}">
					<i class="icon-tags"></i>
					<c:forEach items="${component.tagsAsList}" var="tag">
						<span class="badge">${tag}</span>
					</c:forEach>
				</c:if>
			</div>
		</div>
	</div>
</div>
