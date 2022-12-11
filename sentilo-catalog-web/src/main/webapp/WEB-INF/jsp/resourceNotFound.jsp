<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

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

						<div class="alert alert-error">
							<h2>
								<spring:message code="error.resource.not.found.title" />
							</h2>

							<c:if test="${not empty exception}">
								<c:catch var="exceptionErrorMessageKey">${exception.errorMessageKey}</c:catch>
								<c:catch var="exceptionErrorMessageArgs">${exception.errorMessageArgs}</c:catch>
								<c:if test="${empty exceptionErrorMessageKey}">
									<h4>
										<spring:message code="error.resource.not.found.detail" />
									</h4>
									<c:choose>
										<c:when test="${empty exceptionErrorMessageArgs}">
											<spring:message code="${exception.errorMessageKey}" arguments="${exception.errorMessageArgs}" htmlEscape="false" />
										</c:when>
										<c:otherwise>
											<spring:message code="${exception.errorMessageKey}" htmlEscape="false" var="errorMessage" />
										</c:otherwise>
									</c:choose>
								</c:if>
							</c:if>
						</div>

					</div>
				</div>
			</div>
		</div>
	</div>
</div>

<%@include file="/WEB-INF/jsp/common/footer.jsp"%>