<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<%-- Custom stylesheets here --%>

<%-- If exists a custom style then overrides other tags --%>
<c:if test="${not empty tenantCustomParams and not empty tenantCustomParams.styleClass}">
<spring:url value="/static/${tenantCustomParams.styleClass}" var="tenantCustomCSS" />
<link href="${tenantCustomCSS}" rel="stylesheet" media="all">	
</c:if>