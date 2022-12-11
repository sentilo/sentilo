<%@ include file="/WEB-INF/jsp/common/taglibs.jsp"%>
<%@ page import="org.springframework.web.servlet.HandlerMapping"%>
<spring:url value="/component/map" var="componentMapURL" />
<spring:url value="/component/map/route" var="routeMapURL" />

<%
 String aux = (request.getAttribute(HandlerMapping.PATH_WITHIN_HANDLER_MAPPING_ATTRIBUTE) == null ? "" : (String)request.getAttribute(HandlerMapping.PATH_WITHIN_HANDLER_MAPPING_ATTRIBUTE));
%>

<ul class="nav">
	<li class="dropdown">
		<a href="#" class="dropdown-toggle <%=aux.startsWith("/component/map")?"current":""%>" data-toggle="dropdown">
			<spring:message	code="menu.explore.title" /><b class="caret"></b>
		</a>
		<ul class="dropdown-menu">		
			<li><a href="${componentMapURL}" <%=aux.startsWith("/component/map")?"class='current'":""%>><spring:message code="menu.explore.universal_viewer.title" /></a></li>
			<li><a href="${routeMapURL}" <%=aux.startsWith("/component/map/route")?"class='current'":""%>><spring:message code="menu.explore.route_viewer.title" /></a></li>					
		</ul>
	</li>
</ul>