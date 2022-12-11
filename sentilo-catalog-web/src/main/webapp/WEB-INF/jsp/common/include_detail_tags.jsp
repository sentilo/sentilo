<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<div class="row-fluid">
	<div class="span4">
		<strong><spring:message code="tags" /> </strong>
	</div>
	<div class="span8">
		<c:forEach items="${tags}" var="tag">
			<span class="badge">${tag}</span>
		</c:forEach>
	</div>
</div>
