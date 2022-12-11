<%@page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>


<c:set var="extendedDetailUrlParam" value="${param[\"extendedDetailUrl\"]}" />
<c:set var="btnclose" value="${param.btnclose}" />

<spring:eval var="showFooterLegal" expression="@sentiloConfigProperties.getProperty('sentilo.catalog.show.footer_legal','false')"/>

<%@include file="/WEB-INF/jsp/common/include_script_maps.jsp" %>

<c:choose>
	<c:when test="${empty extendedDetailUrlParam}">
		<%@include file="/WEB-INF/jsp/component/public/include_component_detail.jsp" %>
	</c:when>
	<c:otherwise>
		<%@include file="/WEB-INF/jsp/component/public/include_component_extended_detail.jsp" %>        
	</c:otherwise>
</c:choose>