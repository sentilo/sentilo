<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>

<c:set value="applicationNotInSectorTable" var="applicationTable" />
<c:set value="250" var="scrollable" />
<spring:url value="/admin/application/list/json?sectorId=${sectorId}&tableName=${applicationTable}" var="applicationsAjaxSource" />

<div id="applicationsNotInSectorModal" class="modal hide fade modal-lg customModal" tabindex="-1" role="dialog" aria-labelledby="applicationsNotInSectorModal" aria-hidden="true">
	<div class="modal-dialog modal-lg">
        <div class="modal-header">
            <button type="button" class="close" data-dismiss="modal" aria-hidden="true">×</button>
            <h3 id="confirmModalLabel"><spring:message code="sector.add.applications.to.sector.tittle" arguments="${sector.name}" /></h3>
        </div>
        <div class="modal-body">
        	<div class="span12">
				<div id="error-container" class="alert alert-block alert-error" style="display: none;">
                    <h5><spring:message code="error.check.form.errors" /></h5>
                    <ul id="error-list">
                    </ul>
                </div>
				<div class="tabbable">
					<div class="tab-content">
						<div>
							<script type="text/javascript">
								var tableApplicationNotInSector = null;
								$(document).ready(function() {	
									tableApplicationNotInSector =	makeTableAsync('${applicationTable}', '${applicationsAjaxSource}', undefined, undefined, undefined, ${scrollable}, otherColumnsRender, callBackRowFunction);
								});
							</script>
							<table class="table table-striped" id="${applicationTable}">
								<thead>
									<tr>
										<td>
											<c:choose>
												<c:when test="${showAdminControls}">
									    			<input type="checkbox" name="selectAllRows#${applicationTable}"/>
										    	</c:when>
										    	<c:otherwise>&nbsp;</c:otherwise>
										    </c:choose>
										</td>
										<td><strong><spring:message code="application.name" /> </strong></td>
										<td><strong><spring:message code="application.description" /> </strong></td>
										<td><strong><spring:message code="sector.provider.add.grant" /></strong></td>
									</tr>
								</thead>
								<tbody />
							</table>
						</div>					
						<div class="control-group pull-right" style="margin-bottom: 0px;">
							<c:if test="${showAdminControls}">
								<a href="#" onclick="addElementsToSector('${applicationTable}','${addApplicationToSectorUrl}', [tableApplicationInSector,tableApplicationNotInSector], '#applicationsNotInSectorModal', sectorConfirmAssignedApplications);" class="btn btn-primary">
									<spring:message code="sector.application.add" /> 
								</a> 
							</c:if>
						</div>
					</div>
				</div>
			</div>
       	</div>
     </div>
</div>