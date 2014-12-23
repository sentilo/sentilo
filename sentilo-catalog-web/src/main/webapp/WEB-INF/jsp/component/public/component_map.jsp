<%@page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<%@include file="/WEB-INF/jsp/common/include_script_maps.jsp"%>

<script type="text/javascript">
$(document).ready(function() {
	<c:choose>
		<c:when test="${not empty param.lat}">
			var centerLatValue = ${param.lat};
			var centerLngValue = ${param.lng};
			var zoomValue = ${param.zoom};
			var filterValue = '${param.filter}';	
			var centerValue = (centerLatValue && centerLngValue ? [centerLatValue, centerLngValue]:[]);
			
			initializeComponentMap("#map_canvas_1", centerValue, zoomValue);
			
			if(filterValue){
				$('#componentDropdown li#'+filterValue+' a').click();
			}	
		</c:when>
		<c:otherwise>
			initializeComponentMap("#map_canvas_1");
		</c:otherwise>
	</c:choose>	
	
});
</script>

<div class="container-fluid">
	<div class="mapcontent">
		<div class="row-fluid">
			<div class="span12">

				<%@include file="/WEB-INF/jsp/common/messages.jsp"%>
				<c:set var="mapClass" value="map2" />
				<%@include file="/WEB-INF/jsp/component/public/include_component_map.jsp"%>
			</div>
		</div>
	</div>
</div>
<%@include file="/WEB-INF/jsp/common/footer.jsp"%>