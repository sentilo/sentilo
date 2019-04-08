<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>


<div id="accordion1" class="accordion">
	<div class="accordion-group">
		<div class="accordion-heading">

			<div class="accordion-toggle in">
				<div class="pull-left">
					<i class="icon-signal "></i> <span class="divider-vertical"></span>Activity
				</div>
				<div class="pull-right">
					<ul class="nav nav-pills">
						<li class="dropdown active">
							<a href="#activity" class="dropdown-toggle" data-toggle="dropdown">
								<spring:message code="component.sensors" /> <b class="caret"></b> 
							</a>
							<ul class="dropdown-menu">
								<c:forEach var="sensor" items="${componentSensors}">
								<li>
									<a href="#activity"
									   onclick="selectSensor({ 
												'id': '${sensor.id}', 
												'sensorId': '${sensor.sensorId}', 
												'label': '${fn:replace(sensor.type,search,replace)}', 
												'dataType': '${sensor.dataType}',
												'unit':'${sensor.unit}'
											}); 
											retrieveChartPanel();
											stopAudioPlayer();
											stopVideoPlayer();"
									data-toggle="pill">${sensor.sensorId} (${sensor.type})</a>
								</li>
								</c:forEach>
							</ul>
						</li>
						<li>
							<a href="#alarms" onclick="changeDataType('alarms');" data-toggle="pill">
								<spring:message code="component.alarms" />
							</a>
						</li>
						<li>
							<a href="#orders" onclick="changeDataType('orders');" data-toggle="pill">
								<spring:message code="component.orders" /> 
							</a>
						</li>
					</ul>

				</div>
			</div>
			<div class="accordion-body">
				<div class="accordion-inner accordion-tall">
					<div class="tab-content">
						<div class="tab-pane active" id="activity">
							<div class="row-fluid">
								<div class="span12">
									<div id="activity_placeholder" style="width: 100%; height: 250px; margin: 0 auto; padding: 0px; position: relative; overflow-y: auto;"></div>
								</div>
							</div>
						</div>
						<div class="tab-pane" id="alarms" style="overflow-y: auto;">
							<div class="row-fluid">
								<div class="span12">
									<div id="alarms_placeholder" style="width: 100%; height: 250px; margin: 0 auto; padding: 0px; position: relative; overflow-y: auto;"></div>
								</div>
							</div>							
						</div>
						<div class="tab-pane" id="orders" style="overflow-y: auto;">
							<div class="row-fluid">
								<div class="span12">
									<div id="orders_placeholder" style="width: 100%; height: 250px; margin: 0 auto; padding: 0px; position: relative; overflow-y: auto;"></div>
								</div>
							</div>
						</div>							
					</div>		
				</div>
			</div>
		</div>
	</div>
</div>
