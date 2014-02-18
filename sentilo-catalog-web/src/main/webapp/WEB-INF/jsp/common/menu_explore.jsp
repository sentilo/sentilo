<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>
<spring:url value="/component/map" var="componentMapURL" />

<ul class="nav">
	<li><a href="${componentMapURL}"> <spring:message code="menu.explore.title" /> </a>
	</li>
</ul>