<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@ include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<script type="text/javascript">
window.messages = {
	boolValues: {
		falseValue: '<spring:message code="false"/>',
		trueValue: '<spring:message code="true"/>'
	},
	chart: {
		iniDate: '<spring:message code="universalMap.details.sensor.chart.iniDate"/>',
		endDate: '<spring:message code="universalMap.details.sensor.chart.endDate"/>'
	},
	players: {
		timestamp: '<spring:message code="universalMap.details.players.timestamp"/>',
		filename: '<spring:message code="universalMap.details.players.filename"/>',
		ellapsed: '<spring:message code="universalMap.details.players.ellapsed"/>',
		download: '<spring:message code="universalMap.details.players.download"/>'
	},
	json: {
		timestamp: '<spring:message code="universalMap.details.json.timestamp"/>',
		expandAll: '<spring:message code="universalMap.details.json.expandAll"/>',
		collapseAll: '<spring:message code="universalMap.details.json.collapseAll"/>',
		expandLevels: '<spring:message code="universalMap.details.json.expandLevels"/>'
	}
};
</script>

<div id="component-detail-modal-info-window" style="display: none;">
	
	<!-- Component last data sections -->
	<div id="component-sections">
		<ul class="nav nav-pills">
	    	<li class="active"><a href="#sensors-section" data-toggle="pill"><spring:message code="universalMap.details.sensors"/></a></li>
	    	<li><a href="#alarms-section" data-toggle="pill"><spring:message code="universalMap.details.alarms"/></a></li>
	    	<li><a href="#orders-section" data-toggle="pill"><spring:message code="universalMap.details.orders"/></a></li>
	    	<li><a href="#" onclick="gotoComponentDetail(); return false;"><i class="icon-chevron-up icon-close-infowindow"></i></a></li>
	    	<li><a href="#" onclick="hideModalLayer(); return false;"><i class="icon-chevron-down icon-close-infowindow"></i></a></li>
	    </ul>
	</div>
	
	<div class="container-fluid">
		<div class="row-fluid">
		
			<!-- Component details -->
	    	<div id="component-details" class="span3">
	    		<div class="component-name"></div>
	    		<div class="component-type"></div>
	    		<div class="component-provider-id"></div>
	    		<div class="component-address"></div>
	    		<div class="component-image"></div>
	    		<div class="component-description"></div>
	    		<div class="component-last-update"></div>
	    		<div class="component-location"></div>
	    	</div>
			
			<!-- Component last data content -->
			<div id="component-data" class="span9">
				<div class="row-fluid">
					<div class="span12">
						<div class="tab-content">
							<p class="no-sensors-message"><spring:message code="universalMap.details.component.noSensors" /></p>
							<ul id="sensors-list-tabs" class="nav nav-tabs" style="display: none;"></ul>
							<div id="sensor-list-tab-content" class="tab-content" style="display: none;">
								<div id="sensors-section" class="active tab-pane">
									<div class="sensor-name"></div>
									<p class="no-sensors-message"><spring:message code="universalMap.details.component.noSensors" /></p>
									<div id="sensor-data-detail">
										<div class="fluid-row">
											<div id="data-wrapper"></div>
											<div id="historic-data-wrapper"></div>
										</div>
									</div>
								</div>
								<div id="alarms-section" class="tab-pane">
									<div class="sensor-name"></div>
									<p class="no-alarms-message"><spring:message code="universalMap.details.component.noAlarms" /></p>
									<div id="alarms-list-tab-content">
										<div class="fluid-row">
											<div id="historic-data-wrapper"></div>
										</div>
									</div>
								</div>
								<div id="orders-section" class="tab-pane">
									<div class="sensor-name"></div>
									<p class="no-orders-message"><spring:message code="universalMap.details.component.noOrders" /></p>
									<div id="orders-list-tab-content">
										<div class="fluid-row">
											<div id="historic-data-wrapper"></div>
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