<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>
<%@include file="/WEB-INF/jsp/common/messages.jsp" %>

<spring:message code="sector.grant.R" var="sectorReadGrantMessage" />
<spring:message code="sector.grant.A" var="sectorAdminGrantMessage" />
<spring:url value="/static/js/sentilo/new_resource_sectorial_table_manage.js" var="newResourceSectorialTableManageJS" />

<script type="text/javascript">
	var sectorReadGrant = '${sectorReadPermission}';
	var sectorAdminGrant = '${sectorAdminPermission}';
	var selectOneErrorMsg = '<spring:message code="sector.select.one" />';
	var okButtonLabelMsg = '<spring:message code="ok" />';
</script>
<script type="text/javascript" src="${newResourceSectorialTableManageJS}"></script>

<fieldset>
	<div class="control-group">
		<div class="table-responsive">
			<table class="table table-striped table-sm">
				<thead>
					<tr>
						<th><input type="checkbox" id="checkAllSectorial" /></th>
						<th><spring:message code="sector.new.resource.table.sector" /></th>
						<th><spring:message code="sector.table.description" /></th>
						<c:if test="${showGrantColumn}">
							<th><spring:message code="sector.provider.add.grant" /></th>
						</c:if>
					</tr>
				</thead>
				<tbody>
					<c:choose>
						<c:when test="${not empty userSectors}">
							<c:forEach items="${userSectors}" var="sector" varStatus="status" >
								<tr>
									<td>
										<input type="checkbox" class="idSectorCheckbox" value="${sector.id}" />
									</td>
									<td>${sector.name}</td>
									<td>${sector.description}</td>
									<c:if test="${showGrantColumn}">
										<td>
											<input type="checkbox" class="convertToToggle" name="grant" value="R" 
												data-off="${sectorReadGrantMessage}" data-on="${sectorAdminGrantMessage}" />
										</td>
									</c:if>
									
								</tr>
							</c:forEach>
						</c:when>
						<c:otherwise>
							<tr>
								<c:if test="${showGrantColumn}">
									<td colspan="4" ><spring:message code="empty.list" /></td>
								</c:if>
								<c:if test="${not showGrantColumn}">
									<td colspan="3" ><spring:message code="empty.list" /></td>
								</c:if>
							</tr>											
						</c:otherwise>
					</c:choose>
				</tbody>
			</table>
			<div class="control-group pull-right">
				<%@include file="/WEB-INF/jsp/common/include_input_back.jsp"%>
					<a href="#" class="btn btn-success"> <spring:message code="button.save" /> </a>
			</div>
		</div>
	</div>
</fieldset>