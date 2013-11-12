<%@ page language="java" contentType="text/html; charset=UTF-8"
	pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<%@include file="/WEB-INF/jsp/common/include_script_maps.jsp"%>

<spring:url value="/component/map" var="startURL"/>

<script type="text/javascript">
	$(document).ready(function() {
		initializeHomeMap("#map_canvas_1");		
	});
</script>

<div class="container-fluid">
	<div class="mapcontent">
		<div class="row-fluid">
			<div id="heroBanner" class="hero-unit">
				<br>
				<h1><spring:message code="app.name" htmlEscape="false"/></h1>
				<p>
					<spring:message code="generic.title"/><br/>
				</p>
				<br/>
				<p>
					<a id="exploreButton" class="btn btn-inverse btn-large" href="${startURL}">
						<spring:message code="start.browsing" />
					</a>
				</p>
			</div>
			<c:set var="mapClass" value="maphome"/>
			<%@include file="/WEB-INF/jsp/component/public/include_component_map.jsp" %>
		</div>
		<!-- div class="content" id="home">
			<div class="row-fluid">
				<div class="span4">
					<div class="accordion" id="accordion1">
						<div class="accordion-group">
							<div class="accordion-heading">
								<a class="accordion-toggle in" href="#">
									<i class="icon-question-sign "></i> <span class="divider-vertical"></span>
									<spring:message code="home.howitworks"/>
								</a>
							</div>
							<div class="accordion-body collapse in">
								<div id="banner1"></div>
							</div>
						</div>
					</div>
				</div>
				<div class="span4">
					<div class="accordion" id="accordion1">
						<div class="accordion-group">
							<div class="accordion-heading">
								<a class="accordion-toggle in" href="#">
									<i class="icon-gift "></i> <span class="divider-vertical"></span><spring:message code="home.getmobile"/>
								</a>
							</div>
							<div class="accordion-body collapse in">
								<div id="banner2"></div>

							</div>
						</div>
					</div>
				</div>
				<div class="span4">
					<div class="accordion" id="accordion1">
						<div class="accordion-group">
							<div class="accordion-heading">
								<a class="accordion-toggle in" href="#"> <i
									class="icon-globe "></i> <span class="divider-vertical"></span><spring:message code="home.latestnews"/>
								</a>
							</div>
							<div class="accordion-body collapse in">
								<div id="banner3"></div>
							</div>
						</div>
					</div>
				</div>
			</div>
		</div -->
	</div>
</div>

<%@include file="/WEB-INF/jsp/common/footer.jsp"%>