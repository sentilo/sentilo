<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<spring:eval  var="showAdminControls" expression="T(org.sentilo.web.catalog.security.SecurityUtils).showAdminControls('LIST', 'org.sentilo.web.catalog.domain.Sector')"/>

<c:set value="sectorTable" var="sectorTable" />

<spring:url value="/admin/sector/delete" var="deleteURL" />
<spring:url value="/admin/sector/new" var="newSectorURL" />
<spring:url value="/admin/sector/list/excel?tableName=${sectorTable}" var="excelSource" />
<spring:url value="/admin/sector/" var="sectorDetailPrefix" />
<spring:url value="/admin/sector/list/json" var="sAjaxSource" />


<%@include file="/WEB-INF/jsp/common/include_script_tables.jsp"%>
<script type="text/javascript">
$(document).ready(function() {	
	var tableSector =	makeTableAsync('${sectorTable}', '${sAjaxSource}', '${sectorDetailPrefix}');	
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
						<%@include file="/WEB-INF/jsp/common/messages.jsp"%>

						<h1 class="lead">
							<spring:message code="sector.list.title" />
							<br />
						</h1>


						<form:form method="post" onkeypress="return preventEnterSubmit(event);" modelAttribute="sectors"
							action="${deleteURL}">

							<table class="table table-striped" id="${sectorTable}">
								<thead>
									<tr>
										<td>
											<c:choose>
												<c:when test="${showAdminControls}">
									    			<input type="checkbox" name="selectAllRows#${sectorTable}"/>
										    	</c:when>
										    	<c:otherwise>&nbsp;</c:otherwise>
										    </c:choose>
										</td>
										<td><strong><spring:message code="sector.table.id" /> </strong></td>
										<td><strong><spring:message code="sector.table.name" /> </strong></td>
										<td><strong><spring:message code="sector.table.createdAt" /> </strong></td>
										<td><strong><spring:message code="sector.table.createdBy" /> </strong></td>
									</tr>
								</thead>
								<tbody />
							</table>
							<br />
							<div class="control-group pull-left" id="excel_${sectorTable}">
								<a href="#" type="button" onclick="window.location.href='${excelSource}';" class="btn"> 
									<spring:message code="button.excel" /> 
								</a>
							</div>
							
							<c:if test="${showAdminControls}">
							<div class="control-group pull-right">
								<a href="#" onclick="deleteSelected('sectors');" class="btn btn-danger"> 
									<spring:message code="sector.delete.title" /> 
								</a> 
								<a href="#" type="button" onclick="window.location.href='${newSectorURL}';" class="btn"> 
									<spring:message code="sector.new.title" /> 
								</a>
							</div>
							</c:if>
						</form:form>
					</div>
				</div>
			</div>
		</div>
	</div>
</div>

<%@include file="/WEB-INF/jsp/common/footer.jsp"%>