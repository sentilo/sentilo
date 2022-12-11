<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>
<spring:url value="/auth/login" var="signinURL" />

<ul class="nav">
	<li><a href="${signinURL}"> <spring:message code="menu.signin.title" /> </a>
	</li>
</ul>