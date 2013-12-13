<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<div class="container-fluid">
<div class="content">
<div class="row-fluid">
<div class="span3">
	<%@include file="/WEB-INF/jsp/common/include_sidebar.jsp" %>
</div>
<div class="span9">

<div class="row-fluid">
<div class="span12">

	<%@include file="/WEB-INF/jsp/common/include_background_logo.jsp" %>	
	
	<div class="alert alert-error">		
		<h2><spring:message code="error.resource.not.found.title" /></h2>
		
		<c:if test="${not empty exception.errorMessageKey}">
			<h4><spring:message code="error.resource.not.found.detail" /></h4>
			<spring:message code="${exception.errorMessageKey}" arguments="${exception.errorMessageArgs}" htmlEscape="false"/>													
		</c:if>
	</div>

</div>
</div>
</div>
</div>
</div>
</div>

<%@include file="/WEB-INF/jsp/common/footer.jsp"%>