<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>

<c:set value="userNotInSectorTable" var="userTable" />
<c:set value="notInSector" var="scriptSuffix" />
<c:set value="250" var="scrollable" />
<spring:url value="" var="userDetailPrefix" />
<spring:url value="/admin/users/list/json?sectorId=${sectorId}" var="usersAjaxSource" />

<div id="usersNotInSectorModal" class="modal hide fade modal-lg customModal" tabindex="-1" role="dialog" aria-labelledby="usersNotInSectorModal" aria-hidden="true">
	<div class="modal-dialog modal-lg">
        <div class="modal-header">
            <button type="button" class="close" data-dismiss="modal" aria-hidden="true">×</button>
            <h3 id="confirmModalLabel"><spring:message code="sector.add.users.to.sector.tittle" arguments="${sector.name}" /></h3>
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
						<form:form method="post" id="addUsersForm">
							<div>
								<%@include file="/WEB-INF/jsp/sector/user/sector_user_list.jsp"%>
							</div>					
							<div class="control-group pull-right" style="margin-bottom: 0px;">
								<c:if test="${showAdminControls}">
									<a href="#" onclick="addToSector('${userTable}','${addUserToSectorUrl}');" class="btn btn-primary">
										<spring:message code="sector.user.add" /> 
									</a> 
								</c:if>
							</div>
						</form:form>
					</div>
				</div>
			</div>
       	</div>
     </div>
</div>