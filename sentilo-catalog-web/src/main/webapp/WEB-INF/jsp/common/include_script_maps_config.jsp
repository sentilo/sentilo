<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<!--   
  This file lets you customize some params from map as defaultCenter, styles, ...
--> 

<c:if test="${(not empty tenantCustomParams) && (not empty tenantCustomParams.mapParams)}">
	<c:set var="mapParams" value="${tenantCustomParams.mapParams}" />
	<script type="text/javascript">
	<!-- // 
		<c:if test="${not empty mapParams.center}">
			var defaultMapCenter = [ ${mapParams.center.latitude}, ${mapParams.center.longitude} ];
		</c:if>
		
		<c:if test="${(not empty mapParams.zoomLevel) && (mapParams.zoomLevel > 0)}">	
			var defaultZoomLevel = ${mapParams.zoomLevel};
		</c:if>
		
		<c:if test="${not empty mapParams.bgColor}">
			var defaultBgHomeMapColor = {"color" : "${mapParams.bgColor}"};
		</c:if>	
	// -->
	</script>
</c:if>