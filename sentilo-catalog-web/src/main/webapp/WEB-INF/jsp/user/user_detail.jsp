<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<spring:url value="/admin/users/${user.userName}/edit" var="editUserLink" />
<spring:url value="/admin/users/list?nameTableRecover=userTable&fromBack=true" var="backURL" />

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
							${user.userName}<br /> <small><spring:message code="id" /> ${user.id}</small>
						</h1>

						<div class="tabbable">
							<ul class="nav nav-tabs">
								<li class="active"><a href="#tab1" data-toggle="tab"><spring:message code="user.detail.title" /> </a></li>
							</ul>
							<div class="tab-content">
								<div class="tab-pane active" id="tab1">
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
															<strong><spring:message code="user.password" /> </strong>
														</div>
														<div class="span8">${user.password}</div>
													</div>
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="user.name" /> </strong>
														</div>
														<div class="span8">${user.name}</div>
													</div>
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="user.description" /> </strong>
														</div>
														<div class="span8">${user.description}</div>
													</div>
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="user.createdAt" /> </strong>
														</div>
														<div class="span8">
															<spring:eval expression="user.createdAt" />
														</div>
													</div>
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="user.updatedAt" /> </strong>
														</div>
														<div class="span8">
															<spring:eval expression="user.updateAt" />
														</div>
													</div>
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="user.email" /> </strong>
														</div>
														<div class="span8">
															<a href="mailto:${user.email}">${user.email}</a>
														</div>
													</div>
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="user.rols" /> </strong>
														</div>
														<div class="span8">${user.roles}</div>
													</div>
												</div>
											</div>
										</div>
									</div>
									<div class="row-fluid">
										<div class="span12">
											<div class="control-group pull-right">
												<%@include file="/WEB-INF/jsp/common/include_input_back.jsp"%>
												<a href="${editUserLink}" class="btn btn-primary"> <spring:message code="user.edit.title" /> </a>
											</div>
										</div>
									</div>
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