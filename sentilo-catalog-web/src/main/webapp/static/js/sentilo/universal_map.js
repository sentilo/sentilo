
var sensorsLastObsMap = [];
var defaultChartObsLimit = 10;
var maxChartObsLimit = 200;
var componentDetailUrl;
var currentComponentId = null;
var chartNav = null;

window.messages = {
    boolValues: {
        trueValue: 'yes',
        falseValue: 'no'
    }
};

var mapOptions = {
    modalTargetId: null,
    infoboxTargetId: null,
    componentDetailsUrl: null,
    map: null
};

function initUniversalMap(options) {

    mapOptions = options;

    if (options.modalTargetId === null || options.modalTargetId === undefined || options.modalTargetId !== '') {
        mapOptions.modalTargetId = "#component-detail-modal-info-window";
    }

    if (options.infoboxTargetId === null || options.infoboxTargetId === undefined || options.infoboxTargetId === '') {
        mapOptions.infoboxTargetId = "#infobox";
    }


    registerSectionPillChangeEvents();
}

function showModalLayer() {

    var modal = $(mapOptions.modalTargetId);
    if (!modal) {
        return;
    }

    modal.show(100, function () {
        // Set modal layer as opened
        $("body").addClass('modal-layer-opened');
    });

}

function hideModalLayer() {

    var modal = $(mapOptions.modalTargetId);
    if (!modal) {
        return;
    }

    modal.hide(100, function () {
        // Set modal layer as closed
        $("body").removeClass('modal-layer-opened');
    });

}

function isModalLayerVisible() {
    const modal = $(mapOptions.modalTargetId);
    return $(modal).is(":visible");
}

function updateModalLayer(componentId) {

    if (currentComponentId === componentId && isModalLayerVisible()) {
        return;
    }

    // Prevent reload page
    currentComponentId = componentId;

    // Where to put data
    var tabPanel = $("#sensors-list-tabs");
    tabPanel.hide();

    // Sensor observations panle
    var dataPanel = $("#sensor-list-tab-content");
    dataPanel.hide();

    // Alarms list panel
    var alarmsPanel = $("#alarms-list-tab-content #historic-data-wrapper");

    // Orders list panel
    var ordersPanel = $("#orders-list-tab-content #historic-data-wrapper");

    // Show no data
    $(".no-sensors-message").show();
    $(".no-alarms-message").show();
    $(".no-orders-message").show();

    getComponentDetails(componentId, function (data1) {

        if (data1 === null || data1 === undefined) {
            return;
        }

        // Print component details on view
        updateComponentDetails(data1);

        // Look for component last observation details
        var from = null;
        var to = null;
        getComponentLastObservations(componentId, from, to, function (data2) {

            // Uptdate component last update details
            updateComponentLastDataDetails(data2);

            // Update sensor's details
            if (data2.sensors.length === 0) {
                return;
            } else {
                $('.no-sensors-message').hide();
            }

            // Update sensors tabs
            updateSensorsTabs(data2);

            $('a[data-toggle="tab"]').on('shown', function (e) {
                e.target 			// activated tab
                e.relatedTarget 	// previous tab
                // var sensorId = $(this).attr("id").split("-")[1];
                var sensorId = $(this).attr("sensor-id");
                showLastObservations(componentId, sensorId, dataPanel);
                showLastAlarms(componentId, sensorId, alarmsPanel);
                showLastOrders(componentId, sensorId, ordersPanel);

                // Select sensors tab first
                $('#component-sections a[href="#sensors-section"]').tab('show');
            });

            $(document).ready(function () {
                // Missery workaround: first tab is never clicked! :-/
                if ($("#sensors-list-tabs a:first").length) {
                    // var sensorId = $("#sensors-list-tabs a:first").attr("id").split("-")[1];
                    var sensorId = $("#sensors-list-tabs a:first").attr("sensor-id");
                    if (sensorId && sensorId !== null && sensorId !== undefined) {
                        showLastObservations(componentId, sensorId, dataPanel);
                        showLastAlarms(componentId, sensorId, alarmsPanel);
                        showLastOrders(componentId, sensorId, ordersPanel);

                        // Select sensors tab first
                        $('#component-sections a[href="#sensors-section"]').tab('show');
                    } else {
                        // TODO: Tractar no sensors
                        console.log('No sensors available');
                    }
                }
            });

            // Show sensors observations panel
            showSensorsObservationsPanel();

        });

        // Show modal layer if it is down
        showModalLayer();

        // Move map!!!        
        const location = buildLatLng(data1.location[1],data1.location[0]);
        centerMap(location);
    });

}

function setComponentDetailUrl(url) {
    window.componentDetailUrl = url;
}

function gotoComponentDetail() {
    if (window.componentDetailUrl) {
        window.location.href = window.componentDetailUrl;
    }
    return false;
}


/////////////////////////////////////////////////////////////////////////
// SET DATA ON VIEW
/////////////////////////////////////////////////////////////////////////
function showError(error) {
    $(mapOptions.modalTargetId).html('<p style="color: red;"><strong>' + error + '</strong></p>');
}

function getComponentDetails(componentId, callback) {
    var url = mapOptions.componentDetailsUrl + componentId + '/';
    jsonGET(url, [], function (data) {
        callback(data);	// ComponentDTO
    });
}

function updateComponentDetails(data) {
    // data = ComponentDTO
    $(mapOptions.modalTargetId + " .component-id").html(data.id);
    $(mapOptions.modalTargetId + " .component-name").html(data.name);
    $(mapOptions.modalTargetId + " .component-description").html(data.description);
    $(mapOptions.modalTargetId + " .component-provider-id").html(data.providerId);
    $(mapOptions.modalTargetId + " .component-provider-name").html(data.providerName);
    $(mapOptions.modalTargetId + " .component-type").html(data.typeName);
    $(mapOptions.modalTargetId + " .component-location").html('lat: ' + data.location[1] + ', lon: ' + data.location[0]);

    if (data.photoUrl && data.photoUrl !== undefined && data.photoUrl !== '') {
        $(mapOptions.modalTargetId + " .component-image").html(
            '<a href="' + data.photoUrl + '" data-lightbox="component-img">' +
            '	<img class="component-img" src="' + data.photoUrl + '" />' +
            '</a>'
        );
    } else {
        $(mapOptions.modalTargetId + " .component-image").html('');
    }

    // Geolocate component address
    //setComponentAddress(data.location[1], data.location[0], mapOptions.modalTargetId + " .component-address");
}

function updateComponentLastDataDetails(data) {
    $(mapOptions.modalTargetId + " .component-last-update").html(data.lastUpdateTimeMessage);
}

function getComponentLastObservations(componentId, from, to, callback) {

    var url = mapOptions.componentDetailsUrl + componentId + '/lastOb/';

    var filters = {};

    if (to) {
        filters.to = to;
    }

    if (from) {
        filters.from = from;
    }

    jsonGET(url, filters, function (data) {
        callback(data);	// InfoBoxDTO
    });
}

function getComponentLastAlarms(componentId, from, to, callback) {

    var url = mapOptions.componentDetailsUrl + componentId + '/lastOb/';

    var filters = {};

    if (to) {
        filters.to = to;
    }

    if (from) {
        filters.from = from;
    }

    jsonGET(url, filters, function (data) {
        callback(data);	// InfoBoxDTO
    });
}

function getComponentLastOrders(componentId, from, to, callback) {

    var url = mapOptions.componentDetailsUrl + componentId + '/lastOb/';

    var filters = {};

    if (to) {
        filters.to = to;
    }

    if (from) {
        filters.from = from;
    }

    jsonGET(url, filters, function (data) {
        callback(data);	// InfoBoxDTO
    });
}

function setComponentAddress(latitude, longitude, target) {
    var geo = new google.maps.Geocoder();
    var componentDescription = '${component.description}';
    geo.geocode({
            'latLng': new google.maps.LatLng(latitude, longitude)
        },
        function (results, status) {
            if (status == google.maps.GeocoderStatus.OK && results[0]) {
                $(target).html(results[0].formatted_address);
            }
        }
    );
}

function updateSensorsTabs(data) {

    $("#sensors-list-tabs").hide();
    $("#sensors-list-tabs").empty();

    // Get component sensors last observations
    for (var s in data.sensorLastObservations) {
        var sensor = data.sensorLastObservations[s];
        var sensorId = sensor.sensor;

        var cssClass = 'active';
        if (s > 0) {
            cssClass = '';
        }

        var tabStyle = 'display: none;';
        if (sensor.dataType === 'NUMBER' || sensor.dataType === 'BOOLEAN') {
            tabStyle = '';
        }

        var value = '-';
        if (sensor.value !== null && sensor.value !== undefined && sensor.value !== '') {
            if (sensor.dataType === 'BOOLEAN') {
                value = eval(sensor.value) ? messages.boolValues.trueValue.toUpperCase() : messages.boolValues.falseValue.toUpperCase();
            } else {
                value = sensor.value;
            }
        }

        var tabTitle = '<div class="sensor-tab">';
        tabTitle += '	<div class="sensor-last-value">' + value + '</div>';
        tabTitle += '	<div class="sensor-unit" style="' + tabStyle + '">' + sensor.unit + '</div>';
        tabTitle += '	<div class="sensor-id" style="display: none;">' + sensor.id + '</div>';
        tabTitle += '	<div class="sensor-type">' + sensor.sensorType + '</div>';
        tabTitle += '</div>';

        var li =
            '<li class="' + cssClass + '">' +
            '	<a id="sensor-' + sensorId + '-tab" href="#sensor-' + sensorId + '-content" data-toggle="tab" sensor-id="' + sensorId + '">' + tabTitle + '</a>' +
            '</li>';

        $("#sensors-list-tabs").append(li);

        sensorsLastObsMap[sensorId] = sensor;

    }

}

function showSensorsObservationsPanel() {
    // Show tabs
    $("#sensors-list-tabs").show();
    // Show content
    $("#sensor-list-tab-content").show();
}

function registerSectionPillChangeEvents() {

    $('a[data-toggle="pill"]').on('shown', function (e) {
        e.target // activated tab
        e.relatedTarget // previous tab

        var href = $(this).attr('href');
        if (href === '#alarms-section') {
            // Showing alarms
        } else if (href === '#orders-section') {
            // Showing orders
        } else if (href === '#sensors-section') {
            // Showing sensors
        }

    });

}

function showLastObservations(componentId, sensorId, panel) {

    var sensor = sensorsLastObsMap[sensorId];

    if (!sensor || sensor === null || sensor === undefined) {
        return;
    }

    var sensorLastObs = sensorsLastObsMap[sensorId];
    var dataType = sensor.dataType;

    // Clean data wrapper content
    var dataPanel = $("#data-wrapper");
    dataPanel.empty();

    // Clean historic data wrapper content
    var historicDataPanel = $("#historic-data-wrapper");
    historicDataPanel.empty();

    // Stop all media players previously
    stopAllMediaPlayers();

    // Show sensor name
    $(".sensor-name").html(sensor.sensor);

    // Select data template
    if (dataType === 'BOOLEAN') {
        // Show historic data chart panel
        chartNav = [];
        showBooleanDataChart(historicDataPanel, sensor, componentId, sensorId);
    } else if (dataType === 'NUMBER') {
        // Show historic data chart panel
        chartNav = [];
        showNumericDataChart(historicDataPanel, componentId, sensor);
    } else if (dataType === 'TEXT' || dataType === 'LINK') {
        // Show historic data list panel
        showDataHistoryList(historicDataPanel, sensor, componentId, sensorId, dataType);
    } else {
        // Show historic data in accordion mode
        showDataHistoryListAccordion(historicDataPanel, sensor, componentId, sensorId, dataType);
    }
}

function showLastAlarms(componentId, sensorId, panel) {

    $(panel).empty();

    getLastSensorAlarms(componentId, sensorId, null, function (data) {

        // data = List<AlarmMessage>
        // { message, timestamp, sender, time }

        if (data === null || data.length === 0) {
            $("#alarms-list-tab-content").hide();
            $("#alarms-section .no-alarms-message").show();
            return;
        }

        $("#alarms-list-tab-content").show();
        $("#alarms-section .no-alarms-message").hide();

        for (var i = 0; i < data.length; i++) {
            var alarm = data[i];
            $(panel).append(addAlarmLineToHistoryPanel(alarm.message, alarm.timestamp));
        }

    });

}

function addAlarmLineToHistoryPanel(value, timestamp) {
    var line =
        '<div class="activity_text_element">' +
        '	<span class="label label-info">' + timestamp + '</span>&nbsp;' +
        '	<span class="text">' + value + '</span>' +
        '</div>';
    return line;
}

function showLastOrders(componentId, sensorId, panel) {

    $(panel).empty();

    getLastSensorOrders(componentId, sensorId, null, function (data) {

        // data = List<OrderMessage>
        // { order, timestamp, sender, time }

        if (data === null || data.length === 0) {
            $("#orders-list-tab-content").hide();
            $("#orders-section .no-orders-message").show();
            return;
        }

        $("#orders-list-tab-content").show();
        $("#orders-section .no-orders-message").hide();

        for (var i = 0; i < data.length; i++) {
            var order = data[i];
            $(panel).append(addOrderLineToHistoryPanel(order.order, order.timestamp));
        }
    });
}

function addOrderLineToHistoryPanel(value, timestamp) {
    var line =
        '<div class="activity_text_element">' +
        '	<span class="label label-info">' + timestamp + '</span>&nbsp;' +
        '	<span class="text">' + value + '</span>' +
        '</div>';
    return line;
}

function showBooleanDataChart(dataPanel, sensor, componentId, sensorId, filters) {

    getLastSensorObservations(componentId, sensorId, filters, function (data) {
        var chartAnchor = $('<div id="bool-chart" class="ct-chart ct-perfect-fourth ct-chart-centered-labels"></div>');
        dataPanel.html(chartAnchor);
        printBooleanChart(data, '#bool-chart', {height: 220, bottomPadding: 60});

        var fromTimestamp = (data.fromTimestamp === null) ? '' : data.fromTimestamp;
        var toTimestamp = (data.toTimestamp === null) ? '' : data.toTimestamp;

        var iniDateMessage = (messages.chart.iniDate === null) ? 'Initial date:' : messages.chart.iniDate;
        var endDateMessage = (messages.chart.endDate === null) ? 'End date:' : messages.chart.endDate;

        var chartControls =
            '<hr/>' +
            '<div class="chart-controls row-fluid">' +
            '	<div class="chart-controls span3">' +
            '		<div class="btn-group">' +
            '  			<button id="chart-control-prev-btn" class="btn"><i class="icon-chevron-left"></i></button>' +
            '  			<button id="chart-control-refresh-btn" class="btn"><i class="icon-refresh"></i></button>' +
            '  			<button id="chart-control-next-btn" class="btn"><i class="icon-chevron-right"></i></button>' +
            '		</div>' +
            '	</div>' +
            '	<div class="chart-start-date span3"><span class="chart-controls-label">' + iniDateMessage + '&nbsp;</span><span class="chart-controls-value">' + fromTimestamp + '</span></div>' +
            '	<div class="chart-end-date span6"><span class="chart-controls-label">' + endDateMessage + '&nbsp;</span><span class="chart-controls-value">' + toTimestamp + '</span></div>' +
            '</div>';

        dataPanel.append($(chartControls));

        $("#chart-control-prev-btn").unbind('click');
        $("#chart-control-prev-btn").on('click', function (e) {
            filters = {
                to: data.fromTime
            };
            var navActual = {
                fromTimestamp: data.fromTimestamp,
                toTimestamp: data.toTimestamp,
                fromTime: data.fromTime,
                toTime: data.toTime
            };
            chartNav.push(navActual);
            showBooleanDataChart(dataPanel, sensor, componentId, sensorId, filters);
        });

        $("#chart-control-refresh-btn").unbind('click');
        $("#chart-control-refresh-btn").on('click', function (e) {
            chartNav = [];
            filters = {};
            showBooleanDataChart(dataPanel, sensor, componentId, sensorId, filters);
        });

        $("#chart-control-next-btn").unbind('click');
        $("#chart-control-next-btn").on('click', function (e) {
            var prevNav = chartNav.pop();
            if (!prevNav) {
                chartNav = [];
                filters = {};
            } else {
                filters = {
                    to: prevNav.toTime + 1000
                };
            }
            showBooleanDataChart(dataPanel, sensor, componentId, sensorId, filters);
        });

    });

}

function showNumericDataChart(dataPanel, componentId, sensor, filters) {

    getLastSensorObservations(componentId, sensor.sensor, filters, function (data) {
        var chartAnchor = $('<div id="num-chart" class="ct-chart ct-perfect-fourth ct-chart-centered-labels"></div>');
        dataPanel.html(chartAnchor);
        printNumericalChart(data, sensor.sensorType, sensor.unit, '#num-chart', {height: 220, bottomPadding: 60});

        var fromTimestamp = (data.fromTimestamp === null) ? '' : data.fromTimestamp;
        var toTimestamp = (data.toTimestamp === null) ? '' : data.toTimestamp;

        var iniDateMessage = (messages.chart.iniDate === null) ? 'Initial date:' : messages.chart.iniDate;
        var endDateMessage = (messages.chart.endDate === null) ? 'End date:' : messages.chart.endDate;

        var chartControls =
            '<hr/>' +
            '<div class="chart-controls row-fluid">' +
            '	<div class="chart-controls span3">' +
            '		<div class="btn-group">' +
            '  			<button id="chart-control-prev-btn" class="btn"><i class="icon-chevron-left"></i></button>' +
            '  			<button id="chart-control-refresh-btn" class="btn"><i class="icon-refresh"></i></button>' +
            '  			<button id="chart-control-next-btn" class="btn"><i class="icon-chevron-right"></i></button>' +
            '		</div>' +
            '	</div>' +
            '	<div class="chart-start-date span3"><span class="chart-controls-label">' + iniDateMessage + '&nbsp;</span><span class="chart-controls-value">' + fromTimestamp + '</span></div>' +
            '	<div class="chart-end-date span6"><span class="chart-controls-label">' + endDateMessage + '&nbsp;</span><span class="chart-controls-value">' + toTimestamp + '</span></div>' +
            '</div>';

        dataPanel.append($(chartControls));

        $("#chart-control-prev-btn").unbind('click');
        $("#chart-control-prev-btn").on('click', function (e) {
            filters = {
                to: data.fromTime
            };
            var navActual = {
                fromTimestamp: data.fromTimestamp,
                toTimestamp: data.toTimestamp,
                fromTime: data.fromTime,
                toTime: data.toTime
            };
            chartNav.push(navActual);
            showNumericDataChart(dataPanel, componentId, sensor, filters);
        });

        $("#chart-control-refresh-btn").unbind('click');
        $("#chart-control-refresh-btn").on('click', function (e) {
            chartNav = [];
            filters = {};
            showNumericDataChart(dataPanel, componentId, sensor, filters);
        });

        $("#chart-control-next-btn").unbind('click');
        $("#chart-control-next-btn").on('click', function (e) {
            var prevNav = chartNav.pop();
            if (!prevNav) {
                chartNav = [];
                filters = {};
            } else {
                filters = {
                    to: prevNav.toTime + 1000
                };
            }
            showNumericDataChart(dataPanel, componentId, sensor, filters);
        });

    });

}

function getLastSensorObservations(componentId, sensorId, filters, callback) {
    var url = mapOptions.sensorLastObsUrl + componentId + '.' + sensorId + '/';
    jsonGET(url, filters, function (data) {
        callback(data);
    });
}

function getLastSensorAlarms(componentId, sensorId, filters, callback) {
    var url = mapOptions.sensorLastAlarmsUrl + componentId + '.' + sensorId + '/';
    jsonGET(url, filters, function (data) {
        callback(data);
    });
}

function getLastSensorOrders(componentId, sensorId, filters, callback) {
    var url = mapOptions.sensorLastOrdersUrl + componentId + '.' + sensorId + '/';
    jsonGET(url, filters, function (data) {
        callback(data);
    });
}

function calculateFilters(sensor, filters) {
    if (filters === undefined || filters === null) {
        filters = {
            limit: defaultChartObsLimit
        };
    }

    var chartNumberObs = mapOptions.chartObservationsNumber;
    if (sensor !== null || senosr !== undefined) {
        if (sensor.chartNumberObs !== null && sensor.chartNumberObs !== undefined && sensor.chartNumberObs !== '') {
            chartNumberObs = sensor.chartNumberObs;
        }
    }

    filters.limit = chartNumberObs;

    if (filters.limit < defaultChartObsLimit || filters.limit > maxChartObsLimit) {
        filters.limit = defaultChartObsLimit;
    }

    return filters;
}

function stopAllMediaPlayers() {
    // Stop all possible running media players
    stopAudioPlayer();
    stopVideoPlayerUM();
}

///////////////////////////////////////////////////////////////////////////////////////////////////
//CHARTS
///////////////////////////////////////////////////////////////////////////////////////////////////
//Chartist.js
//https://codepen.io/niketmalik/pen/BZjgpQ

function cleanChart(chartPanel) {
    $(chartPanel).empty();
}

function showDataHistoryListAccordion(dataPanel, sensor, componentId, sensorId, dataType) {

    var sensor = sensorsLastObsMap[sensorId];

    tempSensorLastObsMap = {
        audio: [],
        video: [],
        image: [],
        file: [],
        json: []
    };

    var accordionId = 'history-accordion';
    var accordionWrapperId = 'history-accordion-wrapper';

    var accordionWrapper = $('<div>').addClass(accordionWrapperId).attr('id', accordionWrapperId);
    var accordion = $('<div>').addClass('accordion').attr('id', accordionId);

    var audioWrapperIdPrefix = 'audio-data-wrapper-';
    var videoWrapperIdPrefix = 'video-data-wrapper-';
    var imageWrapperIdPrefix = 'image-data-wrapper-';
    var fileWrapperIdPrefix = 'file-data-wrapper-';
    var jsonWrapperIdPrefix = 'json-data-wrapper-';

    getLastSensorObservations(componentId, sensorId, calculateFilters(sensor, null), function (data) {
        for (var e in data.events) {
            var event = data.events[e];
            var value = event.value;
            var formattedValue = event.formattedValue;
            var timestamp = event.timestamp;
            var header = '<span class="label label-info">' + timestamp + '</span>&nbsp;<span class="">' + value + '</span>';

            if (dataType === 'AUDIO_LINK') {
                addAccordionGroup(accordion, accordionId, dataType, header, e, audioWrapperIdPrefix);
                tempSensorLastObsMap.audio.push({
                    value: value,
                    formattedValue: formattedValue,
                    timestamp: timestamp
                });
            } else if (dataType === 'VIDEO_LINK') {
                addAccordionGroup(accordion, accordionId, dataType, header, e, videoWrapperIdPrefix);
                tempSensorLastObsMap.video.push({
                    value: value,
                    formattedValue: formattedValue,
                    timestamp: timestamp
                });
            } else if (dataType === 'IMAGE_LINK') {
                addAccordionGroup(accordion, accordionId, dataType, header, e, imageWrapperIdPrefix);
                tempSensorLastObsMap.image.push({
                    value: value,
                    formattedValue: formattedValue,
                    timestamp: timestamp
                });
            } else if (dataType === 'FILE_LINK') {
                addAccordionGroup(accordion, accordionId, dataType, header, e, fileWrapperIdPrefix);
                tempSensorLastObsMap.file.push({
                    value: value,
                    formattedValue: formattedValue,
                    timestamp: timestamp
                });
            } else if (dataType === 'JSON') {
                addAccordionGroup(accordion, accordionId, dataType, header, e, jsonWrapperIdPrefix);
                tempSensorLastObsMap.json.push({
                    value: value,
                    formattedValue: formattedValue,
                    timestamp: timestamp
                });
            }
        }
    });

    $(accordionWrapper).html(accordion);
    $(dataPanel).append(accordionWrapper);

    $(dataPanel).on('hide', '.collapse', function () {
        stopAllMediaPlayers();
    });

    $(dataPanel).on('show', '.collapse', function () {
        var idx = $(this).attr('idx');
        var dataType = $(this).attr('sensor-data-type');

        if (dataType === 'AUDIO_LINK') {
            showAndLoadAudioPlayerUM($('#' + audioWrapperIdPrefix + idx), tempSensorLastObsMap.audio[idx]);
        } else if (dataType === 'VIDEO_LINK') {
            showAndLoadVideoPlayerUM($('#' + videoWrapperIdPrefix + idx), tempSensorLastObsMap.video[idx]);
        } else if (dataType === 'IMAGE_LINK') {
            showImageUM($('#' + imageWrapperIdPrefix + idx), tempSensorLastObsMap.image[idx]);
        } else if (dataType === 'FILE_LINK') {
            showFileUM($('#' + fileWrapperIdPrefix + idx), tempSensorLastObsMap.file[idx]);
        } else if (dataType === 'JSON') {
            showJsonUM($('#' + jsonWrapperIdPrefix + idx), tempSensorLastObsMap.json[idx]);
        }

    });

    $(dataPanel).on('hidden', '.collapse', function () {
        var idx = $(this).attr('idx');
        var dataType = $(this).attr('sensor-data-type');

        if (dataType === 'AUDIO_LINK') {
            $('#' + audioWrapperIdPrefix + idx).empty();
        } else if (dataType === 'VIDEO_LINK') {
            $('#' + videoWrapperIdPrefix + idx).empty();
        } else if (dataType === 'IMAGE_LINK') {
            $('#' + imageWrapperIdPrefix + idx).empty();
        } else if (dataType === 'FILE_LINK') {
            $('#' + fileWrapperIdPrefix + idx).empty();
        } else if (dataType === 'JSON') {
            $('#' + jsonWrapperIdPrefix + idx).empty();
        }

    });

    setTimeout(function () {
        $('#history-accordion a:first').click();
    }, 500);

}

function addAccordionGroup(accordion, accordionId, dataType, header, idx, dataWrapperPrefix) {
    var accordionGroup = $('<div>').addClass('accordion-group');
    var accordionHeading = $('<div>').addClass('accordion-heading');
    var accordionToggle = $('<a>').addClass('accordion-toggle')
        .attr('data-toggle', 'collapse')
        .attr('data-parent', '#' + accordionId)
        .attr('href', '#collapse' + idx)
        .html(header);
    var accordionBody = $('<div>').addClass('accordion-body collapse audio-collapse')
        .attr('id', 'collapse' + idx)
        .attr('idx', idx)
        .attr('sensor-data-type', dataType);
    var accordionDataWrapper = $('<div>').attr('id', dataWrapperPrefix + idx);
    var accordionInner = $('<div>').addClass('accordion-inner').html(accordionDataWrapper);

    $(accordionBody).append(accordionInner);
    $(accordionHeading).append(accordionToggle);
    $(accordionGroup).append(accordionHeading);
    $(accordionGroup).append(accordionBody);
    $(accordion).append(accordionGroup);
}

function showDataHistoryList(dataPanel, sensor, componentId, sensorId, dataType) {

    getLastSensorObservations(componentId, sensorId, calculateFilters(sensor, null), function (data) {
        for (var e in data.events) {
            var event = data.events[e];
            var value = event.value;
            var timestamp = event.timestamp;
            if (dataType === 'LINK') {
                $(dataPanel).append(generateLinkDataHistoryLine(value, timestamp));
            } else if (dataType === 'TEXT') {
                $(dataPanel).append(generateTextDataHistoryLine(value, timestamp));
            }
        }
    });

}

function generateLinkDataHistoryLine(value, timestamp) {
    var line =
        '<div class="activity_text_element">' +
        '	<span class="label label-info">' + timestamp + '</span>&nbsp;' +
        '	<span class="link"><a href="' + value + '" target="_blank">' + value + '</a></span>' +
        '</div>';
    return line;
}

function generateTextDataHistoryLine(value, timestamp) {
    var line =
        '<div class="activity_text_element">' +
        '	<span class="label label-info">' + timestamp + '</span>&nbsp;' +
        '	<span class="text">' + value + '</span>' +
        '</div>';
    return line;
}

///////////////////////////////////////////////////////////////////////////////////////////////////
//	MEDIA PLAYERS
///////////////////////////////////////////////////////////////////////////////////////////////////
//Audio player object
var wavesurfer;

function showAndLoadAudioPlayerUM(panel, observation) {

    var link = observation.value;
    var s3Link = observation.formattedValue;
    var timestamp = observation.timestamp;
    var filename = getFilenameFromUrl(link);

    var timestampMessage = (messages.players.timestamp === null) ? 'Timestamp:' : messages.players.timestamp;
    var filenameMessage = (messages.players.filename === null) ? 'File name:' : messages.players.filename;
    var ellapsedMessage = (messages.players.ellapsed === null) ? 'Ellapsed time:' : messages.players.ellapsed;

    var html =
        '<div id="waveform-error-wrapper" style="display: none"><p class="error-message"></p></div>' +
        '<div id="audio-player-wrapper">' +
        ' 	<div id="audio-details">' +
        filenameMessage + '&nbsp;<span id="audio-filename">' + filename + '</span>&nbsp;&nbsp;&nbsp;' +
        timestampMessage + '&nbsp;<span id="audio-timestamp">' + timestamp + '</span>' +
        ' 	</div>' +
        '	<div id="waveform"></div>' +
        '	<div id="controller-wrapper">' +
        '		<div id="controller-buttons" class="btn-group">' +
        '			<div id="play-btn" class="btn btn-small"><i class="icon-play"></i></div>' +
        '			<div id="stop-btn" class="btn btn-small"><i class="icon-stop"></i></div>' +
        '		</div>' +
        '		<div id="controller-info">' +
        '			<p><strong>' + ellapsedMessage + '&nbsp;</strong><span id="ellapsed-time"></span></p>' +
        '		</div>' +
        ' 	</div>' +
        '</div>';

    // Add audio player to the panel
    $(panel).html(html);

    // Create the waveform
    wavesurfer = WaveSurfer.create({
        container: '#waveform',
        waveColor: '#1abc9c',
        progressColor: '#12715f'
    });

    // Player controller events
    $("#play-btn").click(function () {
        wavesurfer.playPause();
        updateAudioButtons();
    });

    $("#stop-btn").click(function () {
        wavesurfer.stop();
        updateAudioButtons();
    });

    // Player events
    wavesurfer.on('ready', function () {
        $("#ellapsed-time").html('0.000s / ' + wavesurfer.getDuration().toFixed(3) + 's');
    });

    wavesurfer.on('audioprocess', function () {
        $("#ellapsed-time").html(wavesurfer.getCurrentTime().toFixed(3) + 's / ' + wavesurfer.getDuration().toFixed(3) + 's');
    });

    wavesurfer.on('interaction', function () {
        $("#ellapsed-time").html(wavesurfer.getCurrentTime().toFixed(3) + 's / ' + wavesurfer.getDuration().toFixed(3) + 's');
    });

    wavesurfer.on('error', function (message) {
        $("#waveform-error-wrapper .error-message").html('<strong>ERROR: </strong> ' + message);
        $("#waveform-error-wrapper").show('slow');
    });

    // Load audio file
    loadAudioPlayerUM(panel, observation);
}

function loadAudioPlayerUM(panel, observation) {

    if (!wavesurfer || wavesurfer === null || wavesurfer === undefined) {
        return;
    }

    // Init player & load audio
    wavesurfer.stop();
    wavesurfer.empty();
    wavesurfer.load(observation.formattedValue);

    $("#audio-filename").html(getFilenameFromUrl(observation.value));
    $("#audio-timestamp").html(observation.timestamp);

}

function showAndLoadVideoPlayerUM(panel, observation) {

    // media_players.js overload

    var link = observation.value;
    var s3Link = observation.formattedValue;
    var timestamp = observation.timestamp;
    var filename = getFilenameFromUrl(link);

    var timestampMessage = (messages.players.timestamp === null) ? 'Timestamp:' : messages.players.timestamp;
    var filenameMessage = (messages.players.filename === null) ? 'File name:' : messages.players.filename;

    var html =
        '<div id="video-player-wrapper">' +
        ' 	<div id="video-details">' +
        filenameMessage + '&nbsp;<span id="video-filename">' + filename + '</span>&nbsp;&nbsp;&nbsp;' +
        timestampMessage + '&nbsp;<span id="video-timestamp">' + timestamp + '</span>' +
        ' 	</div>' +
        '	<video id="video-player" playsinline controls>' +
        '		<source id="video-src">' +
        '	</video>' +
        '</div>';

    // Add all
    $(panel).html(html);

    // Load video
    loadVideoPlayerUM(link, s3Link, timestamp);
}

function loadVideoPlayerUM(link, s3Link, timestamp) {

    var videoPlayer = document.getElementById("video-player");
    if (videoPlayer) {
        // Set the video src
        $("#video-src").attr('src', s3Link);
        // Load the video
        $('#video-player').load();
        $("#video-filename").html(getFilenameFromUrl(link));
        $("#video-timestamp").html(timestamp);
    }

}

function stopVideoPlayerUM() {
    var videoPlayer = document.getElementById("video-player");
    if (videoPlayer) {
        $('video').each(function () {
            $(this)[0].pause();
        });
    }
}

function showImageUM(panel, observation) {

    // Overloaded from media_players.js

    var link = observation.value;
    var s3Link = observation.formattedValue;
    var timestamp = observation.timestamp;
    var filename = getFilenameFromUrl(link);

    var timestampMessage = (messages.players.timestamp === null) ? 'Timestamp:' : messages.players.timestamp;
    var filenameMessage = (messages.players.filename === null) ? 'File name:' : messages.players.filename;

    // Create html node
    var html =
        '<div id="image-player-wrapper">' +
        ' 	<div id="image-details">' +
        filenameMessage + '&nbsp;<span id="image-filename">' + filename + '</span>&nbsp;&nbsp;&nbsp;' +
        timestampMessage + '&nbsp;<span id="image-timestamp">' + timestamp + '</span>' +
        ' 	</div>' +
        '	<div id="image-wrapper"></div>' +
        '</div>';

    // Add html
    $(panel).html(html);

    // Load image
    loadImage(link, s3Link, timestamp);
}

function loadImage(link, s3Link, timestamp) {

    $("#image-wrapper").empty();

    var img = $('<img id="link-image" />').attr('src', s3Link)
        .on('load', function () {
            if (!this.complete || typeof this.naturalWidth == "undefined" || this.naturalWidth == 0) {
                img.attr('src', getErrorImage());
                $("#image-wrapper").html(img);
            } else {
                var imageLink = $('<a id="image-link" href="' + s3Link + '" data-lightbox="link-image"></a>');
                $(imageLink).append(img);
                $("#image-wrapper").html(imageLink);
            }
        });

}

function showFileUM(panel, observation) {

    // Overloaded from media_players.js

    var link = observation.value;
    var s3Link = observation.formattedValue;
    var timestamp = observation.timestamp;
    var filename = getFilenameFromUrl(link);

    var timestampMessage = (messages.players.timestamp === null) ? 'Timestamp:' : messages.players.timestamp;
    var filenameMessage = (messages.players.filename === null) ? 'File name:' : messages.players.filename;
    var downloadMessage = (messages.players.download === null) ? 'Download:' : messages.players.download;

    // Create html node
    var html =
        '<div id="file-downloader-wrapper">' +
        ' 	<div id="file-details">' +
        filenameMessage + '&nbsp;<span id="file-filename">' + filename + '</span>&nbsp;&nbsp;&nbsp;' +
        timestampMessage + '&nbsp;<span id="file-timestamp">' + timestamp + '</span>' +
        ' 	</div>' +
        '	<div id="file-wrapper">' +
        ' 		<a id="file-link" class="btn btn-success" href="' + s3Link + '" target="_blank" download="' + filename + '">' + downloadMessage + '&nbsp' + filename + '</a>' +
        '	</div>' +
        '</div>';

    // Add html
    $(panel).html(html);

}

function loadFile(link, s3Link, timestamp) {
    if ($('#file-downloader-wrapper').length) {
        var filename = getFilenameFromUrl(link);
        $("#file-link").attr('href', s3Link);
        $("#file-link").attr('download', filename);
        $("#file-link").html('Dowload ' + filename);
        $("#file-filename").html(filename);
        $("#file-timestamp").html(timestamp);
    }
}

function showJsonUM(panel, observation) {

    var json = observation.value;
    var timestamp = observation.timestamp;

    var timestampMessage = (messages.json.timestamp === null) ? 'Timestamp:' : messages.json.timestamp;
    var expandAllMessage = (messages.json.expandAll === null) ? 'Expand all:' : messages.json.expandAll;
    var collapseAllMessage = (messages.json.collapseAll === null) ? 'Collapse all:' : messages.json.collapseAll;
    var expandLevelsMessage = (messages.json.expandLevels === null) ? 'Expand levels:' : messages.json.expandLevels;

    // See: https://www.jqueryscript.net/other/Render-JSON-Schema-In-HTML-jQuery-JSON-Presenter.html

    var html =
        '<div id="json-presenter-wrapper">' +
        ' 	<div id="json-details">' +
        timestampMessage + '&nbsp;<span id="json-timestamp">' + timestamp + '</span>' +
        ' 	</div>' +
        '	<div id="json-container"></div>' +
        '	<div id="json-controller">' +
        '		<form class="form-inline">' +
        '			<button id="expand-all" class="btn btn-small btn-primary">' + expandAllMessage + '</button>' +
        '			<button id="collapse-all" class="btn btn-small btn-success">' + collapseAllMessage + '</button>' +
        '			<button id="expand-levels" class="btn btn-small btn-danger">' + expandLevelsMessage + '</button>' +
        '			<input id="levels" type="text" class="input-small" value="1" size="5" maxlength="5">' +
        '		</form>' +
        '	</div>' +
        '</div>';

    // Add html json-presenter panel & tools
    panel.html(html);

    var jsonContainer = $('#json-container');

    doJsonPresenterUM(json, timestamp);

    $('#expand-all').on('click', function () {
        jsonContainer.jsonPresenter('expandAll');
    });

    $('#collapse-all').on('click', function () {
        jsonContainer.jsonPresenter('collapseAll');
    });

    $('#expand-levels').on('click', function () {
        var levels = parseInt($('#levels').val());
        jsonContainer.jsonPresenter('expand', levels);
    });

}

function doJsonPresenterUM(json, timestamp) {
    var jsonContainer = $('#json-container');
    var error = false;

    try {
        var json = JSON.parse(json);
    } catch (e) {
        error = true;
        alert(e);
    }

    $('.error').toggleClass('hidden', !error);
    $('#output-container').toggleClass('hidden', error);

    jsonContainer
        .jsonPresenter('destroy')
        .jsonPresenter({json: json})
        .jsonPresenter('expand', 0);

    $('#json-timestamp').html(timestamp);
}

function downloadFile(fileURL, s3FileURL) {
    var filename = fileURL.substring(fileURL.lastIndexOf('/') + 1);
    var link = document.createElement('a');
    link.setAttribute('href', s3FileURL);
    link.setAttribute('download', fileName);
    link.setAttribute('target', '_blank');
    link.style.display = 'none';
    document.body.appendChild(link);
    link.click();
    document.body.removeChild(link);
}

function getFilenameFromUrl(link) {
    return link.substring(link.lastIndexOf('/') + 1);
}

