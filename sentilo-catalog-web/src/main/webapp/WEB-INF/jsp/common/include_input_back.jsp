<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>


<c:if test="${(not empty currentBreadCrumb) && (not empty currentBreadCrumb.firstEntry())}"> 
 <c:url value="${currentBreadCrumb.firstEntry()}" var="final_back_url" scope="request">
		<c:param name="sfbr" value="true" />
 </c:url>  
</c:if>


<c:choose>
	<c:when test="${not empty final_back_url}">
		<a href="${final_back_url}" class="btn"> <spring:message code="button.back" /></a>
	</c:when>
	<c:otherwise>
		<a href="#"  onclick="history.back(-1);" class="btn">
			<spring:message code="button.back" />
		</a>
	</c:otherwise>
</c:choose>


