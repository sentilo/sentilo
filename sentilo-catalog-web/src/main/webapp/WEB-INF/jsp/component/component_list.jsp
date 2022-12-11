<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<spring:eval  var="showAdminControls" expression="T(org.sentilo.web.catalog.security.SecurityUtils).showAdminControls('LIST', 'org.sentilo.web.catalog.domain.Component')"/>

<c:set var="componentTable"  value="componentTable" />

<spring:url value="/admin/component/new" var="newComponentLink" />
<spring:url value="/admin/component/list/json" var="sAjaxSourceComp" />
<spring:url value="/admin/component/delete" var="deleteURL" />
<spring:url value="/admin/component/changeAccessType" var="changeAccessTypeURL" />
<spring:url value="/admin/component/list/excel?tableName=${componentTable}" var="componentExcelSource" />

<spring:message code="sure.delete.component" var="deleteComponentConfirmMessage" />
<spring:message code="sure.change.accessType" var="changeAccessTypeConfirmMessage" />

<div class="container-fluid">
	<div class="content">
		<div class="row-fluid">
			<div class="span3">
				<%@include file="/WEB-INF/jsp/common/include_sidebar.jsp"%>
			</div>
			<div class="span9">
				<div class="row-fluid">
					<div class="span12">

						<%@include file="/WEB-INF/jsp/common/include_background_logo.jsp"%>
						<%@include file="/WEB-INF/jsp/common/messages.jsp"%>

						<h1 class="lead">
							<spring:message code="component.list.title" />
							<br />
						</h1>
						
						<%@include file="/WEB-INF/jsp/common/include_component_list.jsp"%>
						
						<c:if test="${showAdminControls}">
						<div class="control-group pull-right">
							<a href="#" onclick="deleteSelected('components','<spring:escapeBody>${deleteComponentConfirmMessage}</spring:escapeBody>');" class="btn btn-danger">
								<spring:message code="component.delete.title" /> 
							</a>
							<a href="#" onclick="changeAccessType('components', '<spring:escapeBody>${changeAccessTypeConfirmMessage}</spring:escapeBody>', 'public', '${changeAccessTypeURL}');" class="btn">							
								<spring:message code="button.accessType.change.toPublic" /> 
							</a>
							<a href="#" onclick="changeAccessType('components', '<spring:escapeBody>${changeAccessTypeConfirmMessage}</spring:escapeBody>', 'private', '${changeAccessTypeURL}');" class="btn">
								<spring:message code="button.accessType.change.toPrivate" /> 
							</a>							
							<a href="#" onclick="window.location.href='${newComponentLink}';" class="btn"> <spring:message code="component.new.title" /> </a>
						</div>
						</c:if>
					</div>
				</div>
			</div>
		</div>
	</div>
</div>
<%@include file="/WEB-INF/jsp/common/footer.jsp"%>
