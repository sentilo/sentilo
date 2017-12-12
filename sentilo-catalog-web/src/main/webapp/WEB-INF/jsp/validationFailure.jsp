<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<script type="text/javascript">

function toggleErrorTrace() {
	var errorTrace = $('#errorTrace'); 
	if(errorTrace.is(':visible')) {
		$('#errorTrace').hide();
	} else {
		$('#errorTrace').show();
	}
}

$(document).ready(function() {
	$('#errorTrace').hide();
});
</script>

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
								<spring:message code="error.actionExecution.title" />
							</h2>

							<c:if test="${not empty exception}">
								<h4>
									<spring:message code="error.detail.title" />
								</h4>
								<c:choose>
									<c:when test="${not empty exception.errorMessageKey}">
										<spring:message code="${exception.errorMessageKey}" arguments="${exception.errorMessageArgs}"
											htmlEscape="false" />
									</c:when>
									<c:otherwise>
										<c:out value="${exception.localizedMessage}" escapeXml="false" />
									</c:otherwise>
								</c:choose>

								<br />
								<br />
								<button class="btn btn-danger" onclick="toggleErrorTrace();">
									<i class="icon-warning-sign icon-white"></i>
									<spring:message code="error.stacktrace" />
								</button>

								<div id="errorTrace">
								    <!--  Root cause -->
									<c:out value="${exception.cause}" />
									<br />
									<c:forEach items="${exception.stackTrace}" var="trace">
										<c:out value="${trace}" />
										<br />
									</c:forEach>
								</div>
							</c:if>
						</div>
					</div>
				</div>
			</div>
		</div>
	</div>
</div>
<%@include file="/WEB-INF/jsp/common/footer.jsp"%>

