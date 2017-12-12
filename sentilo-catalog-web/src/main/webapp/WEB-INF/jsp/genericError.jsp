<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<script type="text/javascript">
	function toggleErrorTrace() {
		var errorTrace = $('#errorTrace');
		if (errorTrace.is(':visible')) {
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
			<div class="span1"></div>
			<div class="span10">

				<%@include file="/WEB-INF/jsp/common/include_background_logo.jsp"%>

				<div class="alert alert-error">
					<h2>
						<spring:message code="error.information.title" />
					</h2>

					<c:if test="${not empty exception}">
						<h4><spring:message code="error.generic.title" /></h4>
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
			<div class="span1"></div>
		</div>
	</div>
</div>

<%@include file="/WEB-INF/jsp/common/footer.jsp"%>