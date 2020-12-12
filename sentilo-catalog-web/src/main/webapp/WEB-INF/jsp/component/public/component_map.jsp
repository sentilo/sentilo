<%@page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8" %>
<%@include file="/WEB-INF/jsp/common/header.jsp" %>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp" %>

<script type="text/javascript">
    const centerLatValue = parseToFloat('<c:out value="${param.lat}" />');
    const centerLngValue = parseToFloat('<c:out value="${param.lng}" />');
    const zoomValue = parseToInteger('<c:out value="${param.zoom}" />');
    const filterValue = '<c:out value="${param.filter}" />';
    const centerValue = (centerLatValue && centerLngValue ? [centerLatValue, centerLngValue] : null);
    const mapProviderValue = '${provider_map}';
    const selector = "#map_canvas_1";

    const mapProperties = {
        selector: selector,
        provider: mapProviderValue,
        center: centerValue,
        zoom: zoomValue
    };
</script>

<%@include file="/WEB-INF/jsp/common/include_script_maps.jsp" %>

<spring:url value="/component/map/" var="componentMapUrl"/>
<spring:url value="/static/img/icons/" var="iconsPath"/>
<spring:url value="/admin/sensor/lastOb/" var="lastObUrl"/>
<spring:url value="/admin/sensor/lastObs/" var="lastObsUrl"/>
<spring:url value="/admin/sensor/lastOrders/" var="lastOrdersUrl"/>
<spring:url value="/admin/sensor/lastAlarms/" var="lastAlarmsUrl"/>

<script type="text/javascript">
    $(document).ready(function () {

        initializeComponentMap(mapProperties);

        if (filterValue) {
            $('#componentDropdown li#' + filterValue + ' a').click();
        }

        var mapOptions = {
            modalTarget: null,
            infoboxTarget: null,
            componentDetailsUrl: '${componentMapUrl}',
            sensorLastObsUrl: '${lastObsUrl}',
            sensorLastAlarmsUrl: '${lastAlarmsUrl}',
            sensorLastOrdersUrl: '${lastOrdersUrl}',
            map: map,
            provider: mapProperties.provider
        };

        // Init Sentilo Universal Maps Scripts
        // Wait some seconds to best page performace ;-)
        setTimeout(function () { // There is no need to implement a specific function for universalMap
            // Init maps modal info window
            initUniversalMap(mapOptions);
        }, 100);

    });

</script>

<div class="container-fluid">
    <div class="mapcontent">
        <div class="row-fluid">
            <div class="span12">

                <%@include file="/WEB-INF/jsp/common/messages.jsp" %>
                <c:set var="mapClass" value="map2"/>
                <%@include file="/WEB-INF/jsp/component/public/include_component_map.jsp" %>
            </div>
        </div>
    </div>
</div>
<%@include file="/WEB-INF/jsp/component/public/include_component_modal_map_info.jsp" %>
<%@include file="/WEB-INF/jsp/common/footer.jsp" %>
