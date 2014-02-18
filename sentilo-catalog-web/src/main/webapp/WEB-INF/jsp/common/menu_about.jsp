<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<spring:url value="/stats/" var="statsURL" />

<ul class="nav">
	<li><a href="${statsURL}"><spring:message code="menu.about.stats.title" /> </a></li>
</ul>
