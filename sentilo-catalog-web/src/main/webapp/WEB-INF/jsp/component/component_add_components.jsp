<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<spring:url value="/admin/component/${componentComponents.componentId}/detail" var="backURL" />

<div class="container-fluid">
	<div class="content">
		<div class="row-fluid">
			<div class="span3">
				<%@include file="/WEB-INF/jsp/common/include_sidebar.jsp"%>
			</div>

			<div class="span9">

				<%@include file="/WEB-INF/jsp/common/include_background_logo.jsp"%>

				<spring:url value="/admin/component/${componentComponents.componentId}/addComponents" var="addComponentsURL" />

				<h1 class="lead">
					<spring:message code="component.assign.title" />
					<br />
				</h1>

				<form:form method="post" modelAttribute="componentComponents" action="${addComponentsURL}" class="form-horizontal">
					<fieldset>
						<div class="control-group">
							<form:label path="componentId" class="control-label">
								<spring:message code="component.id" />
							</form:label>
							<div class="controls">
								<form:input path="componentId" value="${componentId}" readonly="true" />
								<form:errors path="componentId" cssClass="error" htmlEscape="false" />
							</div>
						</div>
						<div class="control-group">
							<form:label path="selectedIds" class="control-label">
								<spring:message code="component.assign.title" />
							</form:label>
							<div class="controls">
								<form:select path="selectedIds">
									<form:options items="${componentComponents.components}" itemValue="id" itemLabel="name" />
								</form:select>
								<form:errors path="selectedIds" cssClass="error" htmlEscape="false" />
							</div>
						</div>
						<div class="control-group">
							<div class="controls">
								<%@include file="/WEB-INF/jsp/common/include_input_back.jsp"%>
								<button type="submit" class="btn">
									<spring:message code="component.assign.title" />
								</button>
							</div>
						</div>
					</fieldset>
				</form:form>

			</div>
		</div>
	</div>
</div>
<%@include file="/WEB-INF/jsp/common/footer.jsp"%>