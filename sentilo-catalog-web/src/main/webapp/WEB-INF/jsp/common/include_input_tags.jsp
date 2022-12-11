<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<script type="text/javascript">
$(document).ready(function() {
	$('#tags').tagsInput({
		height: '30px',
		defaultText:'<spring:message code="tags.add" />',
		placeholderColor:'#aaa'
	});
});
</script>

<div class="control-group">
	<form:label path="tags" class="control-label">
		<spring:message code="tags" />
	</form:label>
	<div class="controls">
		<form:input path="tags" id="tags" />
		<form:errors path="tags" cssClass="text-error" htmlEscape="false" />
	</div>
</div>