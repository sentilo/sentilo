<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<c:if test="${mode == 'edit' }">
	<spring:url value="/admin/component/${component.id}/edit" var="actionURL" />
	<spring:message code="component.edit.title" var="pageTitle" />
	<spring:url value="/admin/component/${component.id}/detail" var="backURL" />
	<spring:eval var="showAdminControls" expression="T(org.sentilo.web.catalog.security.SecurityUtils).showAdminControls('EDIT', component)"/>
</c:if>
<c:if test="${mode == 'create' }">
	<spring:url value="/admin/component/create" var="actionURL" />
	<spring:message code="component.new.title" var="pageTitle" />
	<spring:message code="select.empty" var="emptySelectMessage" />
	<spring:url value="/admin/component/list?nameTableRecover=componentTable&fromBack=true" var="backURL" />
	<spring:eval var="showAdminControls" expression="T(org.sentilo.web.catalog.security.SecurityUtils).showAdminControls('CREATE', 'org.sentilo.web.catalog.domain.Component')"/>
</c:if>

<c:set var="editMode" value="${mode == 'edit' }" />


<div class="container-fluid">
	<div class="content">
		<div class="row-fluid">
			<div class="span3">
				<%@include file="/WEB-INF/jsp/common/include_sidebar.jsp"%>
			</div>
			<div class="span9">

				<%@include file="/WEB-INF/jsp/common/include_background_logo.jsp"%>
				<%@include file="/WEB-INF/jsp/common/messages.jsp"%>

				<h1 class="lead">
					${pageTitle}<br />
				</h1>

				<form:form method="post" modelAttribute="component" action="${actionURL}" class="form-horizontal" autocomplete="off">
					<input type="hidden" name="tenantId" id="tenantId" value="${tenantId}"  />
					<fieldset>
						<div class="tabbable">
							<ul class="nav nav-tabs">
								<li class="active"><a href="#tab1" data-toggle="tab"><spring:message code="component.detail.title" /></a></li>
								<li><a href="#tab2" data-toggle="tab"><spring:message code="technicalDetails.tab.label" /></a></li>
								<li><a href="#tab3" data-toggle="tab"><spring:message code="component.additionalInfo" /></a></li>
							</ul>
							<c:if test="${editMode}">
								<form:hidden path="id" />
								<form:hidden path="createdAt" />
								<form:hidden path="createdBy" />
							</c:if>
							<div class="tab-content">
								<div class="tab-pane active" id="tab1">
									<div class="control-group">
										<form:label path="name" class="control-label">
											<spring:message code="component.name" />
										</form:label>
										<div class="controls">
											<form:input path="name" readonly="${editMode}" />
											<form:errors path="name" cssClass="text-error" htmlEscape="false" />
										</div>
									</div>
									<div class="control-group">
										<form:label path="componentType" class="control-label">
											<spring:message code="component.type" />
										</form:label>
										<div class="controls">
											<form:select path="componentType">
												<form:options items="${componentTypes}" itemValue="value" itemLabel="label" />
											</form:select>
											<form:errors path="componentType" cssClass="text-error" htmlEscape="false" />
										</div>
									</div>
									<div class="control-group">
										<form:label path="description" class="control-label">
											<spring:message code="component.description" />
										</form:label>
										<div class="controls">
											<form:textarea path="description" />
											<form:errors path="description" cssClass="text-error" htmlEscape="false" />
										</div>
									</div>
									<c:if test="${empty providerId}">
									<div class="control-group">
										<form:label path="providerId" class="control-label">
											<spring:message code="component.providerId" />
										</form:label>
										<div class="controls">
												<form:select path="providerId" id="providerId" disabled="${editMode}">
													<form:option value="">${emptySelectMessage}</form:option>
													<form:options items="${providers}" itemValue="value" itemLabel="label" />
											</form:select>
											<c:if test="${editMode}">
												<form:hidden path="providerId" />
											</c:if>
												<form:errors path="providerId" cssClass="text-error" htmlEscape="false" />
										</div>
									</div>
													
									</c:if>
									<c:if test="${not empty providerId}">
										<input type="hidden" name="providerId" id="providerId" value="${providerId}" />
										
									</c:if>
									<div class="control-group">
										<form:label path="photoUrl" class="control-label">
											<spring:message code="component.photo" />
										</form:label>
										<div class="controls">
											<form:input path="photoUrl" />
											<form:errors path="photoUrl" cssClass="text-error" htmlEscape="false" />
										</div>
									</div>
									<div class="control-group">
										<form:label path="publicAccess" class="control-label">
											<spring:message code="component.publicAccess" />
										</form:label>
										<div class="controls">
											<form:checkbox path="publicAccess" />
											<form:errors path="publicAccess" cssClass="text-error" htmlEscape="false" />
										</div>
									</div>
									<%@include file="/WEB-INF/jsp/common/include_input_tags.jsp"%>
									<%@include file="/WEB-INF/jsp/common/include_input_location.jsp"%>
								</div>
								<div class="tab-pane" id="tab2">											
									<c:set var="resourceIsComponent"  value="true" />
									<%@include file="/WEB-INF/jsp/common/include_technical_details_new.jsp"%>
								</div>
								<div class="tab-pane" id="tab3">
									<c:set var="additionalInfo"  value="${component.additionalInfo}" />
									<%@include file="/WEB-INF/jsp/common/include_additional_info.jsp"%>
								</div>
							</div>
						</div>
						<div class="control-group">
							<div class="controls">
								<%@include file="/WEB-INF/jsp/common/include_input_back.jsp"%>
								<c:if test="${showAdminControls}">
								<a href="#" onclick="$('form#component').submit();" class="btn btn-success">
									 <spring:message code="button.save" />
								</a>
								</c:if>
							</div>
						</div>
					</fieldset>
				</form:form>
			</div>
		</div>
	</div>
</div>
<%@include file="/WEB-INF/jsp/common/footer.jsp"%>