<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<!--   
  This file lets you customize some params from map as defaultCenter, styles, ...
--> 

<c:if test="${not empty tenantCustomParams}">
	<script type="text/javascript">
	<!-- // 
		<c:if test="${not empty tenantCustomParams.mapParams.center}">
			var defaultMapCenter = [ ${tenantCustomParams.mapParams.center.latitude}, ${tenantCustomParams.mapParams.center.longitude} ];
		</c:if>
		
		<c:if test="${not empty tenantCustomParams.mapParams.zoomLevel}">	
			var defaultZoomLevel = ${tenantCustomParams.mapParams.zoomLevel};
		</c:if>
		
		<c:if test="${not empty tenantCustomParams.mapParams.bgColor}">
			var defaultBgHomeMapColor = {"color" : "${tenantCustomParams.mapParams.bgColor}"};
		</c:if>	
	// -->
	</script>
</c:if>