<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<%@include file="/WEB-INF/jsp/common/include_script_tables.jsp"%>

<spring:url value="/component/" var="detailPrefix" />
<spring:url value="/component/list/json" var="sAjaxSourceComp" />

<%@include file="/WEB-INF/jsp/common/include_script_tables.jsp"%>

<script type="text/javascript">

	$(document).ready(function() {	
		var firstColumnRenderDelegate = function (data, type, row) {
			return '';
		}; 

		makeTableAsync('${sAjaxSourceComp}', '#componentTable',);
	});
</script>

<div class="container-fluid">
	<div class="content">
		<div class="row-fluid">
			<div class="span12">

				<%@include file="/WEB-INF/jsp/common/include_background_logo.jsp"%>
				<%@include file="/WEB-INF/jsp/common/messages.jsp"%>

				<h1 class="lead">
					<spring:message code="component.list.title" />
					<br />
				</h1>

				<%@include file="/WEB-INF/jsp/common/include_list_component.jsp"%>
			</div>
		</div>
	</div>
</div>
<%@include file="/WEB-INF/jsp/common/footer.jsp"%>