<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>
<%@include file="/WEB-INF/jsp/common/include_script_tables.jsp"%>
<script type="text/javascript">
	var tableUser_${scriptSuffix} = null;
	$(document).ready(function() {	
		tableUser_${scriptSuffix} =	makeTableAsync('${userTable}', '${usersAjaxSource}', '${userDetailPrefix}', undefined, undefined, '${scrollable}');
	});
</script>

<table class="table table-striped" id="${userTable}">
	<thead>
		<tr>
			<td>
				<c:choose>
					<c:when test="${showAdminControls}">
		    			<input type="checkbox" name="selectAllRows#${userTable}"/>
			    	</c:when>
			    	<c:otherwise>&nbsp;</c:otherwise>
			    </c:choose>
			</td>
			<td><strong><spring:message code="user.userName" /> </strong></td>
			<td><strong><spring:message code="user.name" /> </strong></td>
			<td><strong><spring:message code="user.email" /> </strong></td>
		</tr>
	</thead>
	<tbody />
</table>