<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="taglibs.jsp" %>
<c:if test="${not empty error}">
	<div class="alert alert-error">
		<button type="button" class="close" data-dismiss="alert">&times;</button>
			<c:out value="${error}"></c:out>
	</div>
</c:if>
<c:if test="${not empty success}">
	<div class="alert alert-success">
		<button type="button" class="close" data-dismiss="alert">&times;</button>
			<spring:message code="${success}"/>
	</div>
</c:if>
