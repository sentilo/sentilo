<%@ include file="/WEB-INF/jsp/common/taglibs.jsp"%>
<%@ page import="org.springframework.web.servlet.HandlerMapping"%>
<spring:url value="/component/map" var="componentMapURL" />
<spring:url value="/component/map/route" var="routeMapURL" />

<ul class="nav">
	<li class="dropdown">
		<a href="#" class="dropdown-toggle <%=((String)request.getAttribute(HandlerMapping.PATH_WITHIN_HANDLER_MAPPING_ATTRIBUTE)).startsWith("/component/map")?"current":""%>" data-toggle="dropdown">
			<spring:message	code="menu.explore.title" /><b class="caret"></b>
		</a>
		<ul class="dropdown-menu">		
			<li><a href="${componentMapURL}" <%=((String)request.getAttribute(HandlerMapping.PATH_WITHIN_HANDLER_MAPPING_ATTRIBUTE)).startsWith("/component/map")?"class='current'":""%>><spring:message code="menu.explore.universal_viewer.title" /></a></li>
			<li><a href="${routeMapURL}" <%=((String)request.getAttribute(HandlerMapping.PATH_WITHIN_HANDLER_MAPPING_ATTRIBUTE)).startsWith("/component/map/route")?"class='current'":""%>><spring:message code="menu.explore.route_viewer.title" /></a></li>					
		</ul>
	</li>
</ul>