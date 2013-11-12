<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp" %>

<script>
	
	var d1 = [];
	var xAxisLabels = [];
	var temp = {};
	var iter = 1;
	
	// Graph for text data
	function appendTextObs(key) {

			if (typeof temp[key] == "undefined") {
				temp[key] = 1;
			}  else {
				temp[key]++;
			};

		d1.push([key, 1]);

		$.plot("#plot", [ d1 ], {
			series: {
				color: "#2FABE9",
				bars: {
					show: true,
					align: 'center',
					barWidth: 0.5,
					fill: true,
					fillColor: {
						colors: [ { opacity: 0.08 }, { opacity: 0.1 } ] 
					}
				}
			},
			xaxis: {
				mode: "categories",
				tickLength: 0
			}
		});
	};

	
	// Graph for number data
	function appendNumberObs(key,timestampData) {

		d1.push([iter, key]);
		xAxisLabels.push(formatGraphTimestamp(timestampData));
		
		var xAxisLabelGenerator = function(x) {
			var value = xAxisLabels[x-1];
			if(typeof value == 'undefined'){
				value = "";
			}
	    	return value;
		};
		
		var min_xaxis = iter-10;
		if(min_xaxis < 1){
			min_xaxis = 1;
		}
		var max_xaxis = iter;
		iter++;
		
		
		var options = {
				grid: {
					hoverable: true, 
					clickable: true, 
					tickColor: "#eee",
					borderWidth: 0
				},
				xaxis: {
		        	min: min_xaxis,
		        	max: max_xaxis,
		        	tickFormatter: xAxisLabelGenerator
		        }
			};	
		
		$.plot("#plot", [{
			color: "#2FABE9",
			label: "${sensor.unit}",
			data: d1,
			lines: {
				show: true,
				lineWidth: 3,
				fill: true,
				fillColor: {
					colors: [ { opacity: 0.08 }, { opacity: 0.1 } ] 
				}
			},
			points: {
				show: true
			},
			shadowSize: 2
		}], options);

	};

	// Graph for boolean data
	function appendBooleanObs(key,timestampData) {

			if (key == 'true' || key =='1') {
				d1.push([iter, 1]);
			} else {
				d1.push([iter, 0]);
			}
			
			var min_xaxis = iter-10;
			if(min_xaxis < 0.5){
				min_xaxis = 0.5;
			}
			var max_xaxis = iter+0.5;
			iter++;
						
			var d = new Date();
			xAxisLabels.push(formatGraphTimestamp(timestampData));
			
			var xAxisLabelGenerator = function(x) {
				var value = xAxisLabels[x-1];
				if(typeof value == 'undefined'){
					value = "";
				}
		    	return value;
			};

			
			var yAxisLabelGenerator = function(y) {
				return y == 0 ? 'false' : 'true'; 
			};
			
			var options = {
					grid: {
						hoverable: true, 
						clickable: true, 
						tickColor: "#eee",
						borderWidth: 0
					
					},
					xaxis: {
			        	min: min_xaxis,
			        	max: max_xaxis,
			        	tickFormatter: xAxisLabelGenerator
			        },
			        yaxis: {
			        	min: 0,
			        	max: 1,
			        	tickSize: 1,
			        	tickFormatter: yAxisLabelGenerator
			        }
				};

		$.plot("#plot", [{
			color: "#2FABE9",
			data: d1,
			bars: {
				show: true,
				align: 'center',
				barWidth: 0.5,
				fill: true,
				fillColor: {
					colors: [ { opacity: 0.08 }, { opacity: 0.1 } ] 
				}
			}
		}], options);
	
	};
	</script>