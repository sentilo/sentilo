<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<c:choose>
	<c:when test="${not empty backURL}">
		<a href="${backURL}" class="btn"> <spring:message code="button.back" /></a>
	</c:when>
	<c:otherwise>
		<button type="button" onclick="history.back(-1);" class="btn">
			<spring:message code="button.back" />
		</button>
	</c:otherwise>
</c:choose>
