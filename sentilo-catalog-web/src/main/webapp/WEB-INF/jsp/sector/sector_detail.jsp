<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>
<%@include file="/WEB-INF/jsp/common/include_tab_classes.jsp"%>

<spring:eval var="showAdminControls" expression="T(org.sentilo.web.catalog.security.SecurityUtils).showAdminControls('EDIT', sector)"/>
<c:set var="sectorId" scope="request" value="${sector.id}" />

<spring:url value="/static/css/sector-custom-popup.css" var="customPopupCSS" />
<spring:url value="/admin/sector/${sector.id}/edit" var="editSectorLink" />
<spring:url value="/admin/sector/list?nameTableRecover=userTable&fromBack=true" var="backURL" />
<spring:url value="/static/js/sentilo/add_elements_with_grant_to_sector.js" var="addElementsWithGrantToSectorJS" />

<script type="text/javascript">
	var defaultPermission = '${sectorReadPermission}'
	var notDefaultPermission = '${sectorAdminPermission}'
</script>
<script type="text/javascript" src="${addElementsWithGrantToSectorJS}" ></script>
<div class="container-fluid">
	<div class="content">
		<div class="row-fluid">
			<div class="span2">
				<%@include file="/WEB-INF/jsp/common/include_sidebar.jsp"%>
			</div>
			<div class="span10">

				<div class="row-fluid">
					<div class="span12">

						<%@include file="/WEB-INF/jsp/common/include_background_logo.jsp"%>
						<%@include file="/WEB-INF/jsp/common/messages.jsp"%>

						<h1 class="lead">
							${sector.name}<br /> <small><spring:message code="id" /> ${sector.id}</small>
						</h1>

						<div class="tabbable">
							<ul class="nav nav-tabs">
								<li class="${tab1Class}">
									<a href="#tab1" data-toggle="tab">
										<spring:message code="user.detail.title" />
									</a>
								</li>
								<li class="${tab2Class}">
									<a href="#tab2" data-toggle="tab">
										<spring:message code="sector.users.tab.detail.label" />
									</a>
								</li>
								<li class="${tab3Class}">
									<a href="#tab3" data-toggle="tab">
										<spring:message code="sector.providers.tab.detail.label" />
									</a>
								</li>
								<li class="${tab4Class}">
									<a href="#tab4" data-toggle="tab">
										<spring:message code="sector.applications.tab.detail.label" />
									</a>
								</li>
							</ul>
							<div class="tab-content">
								<div class="${tab1PaneClass}" id="tab1">
									<div class="accordion" id="detailAccordion">
										<div class="accordion-group">
											<div class="accordion-heading">
												<a class="accordion-toggle" data-toggle="collapse" data-parent="#detailAccordion"
													href="#detailAccordionCollapse"> <i class="icon-th"></i> <spring:message code="data" /> <i
													class="icon-chevron-down pull-right"></i> </a>
											</div>
											<div id="detailAccordionCollapse" class="accordion-body collapse in">
												<div class="accordion-inner">													
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="sector.name" /> </strong>
														</div>
														<div class="span8">${sector.name}</div>
													</div>
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="sector.description" /> </strong>
														</div>
														<div class="span8">${sector.description}</div>
													</div>
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="sector.createdAt" /> </strong>
														</div>
														<div class="span8">
															<spring:eval expression="sector.createdAt" />
														</div>
													</div>
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="sector.updatedAt" /> </strong>
														</div>
														<div class="span8">
															<spring:eval expression="sector.updatedAt" />
														</div>
													</div>													
												</div>
											</div>
										</div>
									</div>									
									<div class="row-fluid">
										<div class="span12">
											<div class="control-group pull-right">
												<%@include file="/WEB-INF/jsp/common/include_input_back.jsp"%>
												<c:if test="${showAdminControls}">
													<a href="${editSectorLink}" class="btn btn-primary"> <spring:message code="sector.edit.title" /> </a>
												</c:if>
											</div>
										</div>
									</div>
								</div>
								<div class="${tab2PaneClass}" id="tab2">			
									<%@include file="/WEB-INF/jsp/sector/user/users_in_sector_list.jsp" %>						
								</div>
								<div class="${tab3PaneClass}" id="tab3">
									<%@include file="/WEB-INF/jsp/sector/provider/providers_in_sector_list.jsp" %>										
								</div>
								<div class="${tab4PaneClass}" id="tab4">			
									<%@include file="/WEB-INF/jsp/sector/application/applications_in_sector_list.jsp" %>									
								</div>
							</div>
						</div>
					</div>
				</div>
			</div>
		</div>
	</div>
</div>
<%@include file="/WEB-INF/jsp/common/footer.jsp"%>