<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<spring:url value="/static/js/bootstrap-datepicker.js" var="datePickerJS" />
<script type="text/javascript" src="${datePickerJS}"></script>

<spring:url value="/static/js/daterangepicker.js" var="dateRangePickerJS" />
<script type="text/javascript" src="${dateRangePickerJS}"></script>

<spring:url value="/static/js/date.js" var="dateJS" />
<script type="text/javascript" src="${dateJS}"></script>

<script type="text/javascript">

var monthNames = ['<spring:message code="months.january" htmlEscape="false"/>', '<spring:message code="months.february" htmlEscape="false"/>', '<spring:message code="months.march" htmlEscape="false"/>', '<spring:message code="months.april" htmlEscape="false"/>', '<spring:message code="months.may" htmlEscape="false"/>', '<spring:message code="months.june" htmlEscape="false"/>', '<spring:message code="months.july" htmlEscape="false"/>', '<spring:message code="months.august" htmlEscape="false"/>', '<spring:message code="months.september" htmlEscape="false"/>', '<spring:message code="months.october"/>', '<spring:message code="months.november"/>', '<spring:message code="months.december"/>'];
var monthShortNames = ['<spring:message code="months.short.january"/>', '<spring:message code="months.short.february"/>', '<spring:message code="months.short.march"/>', '<spring:message code="months.short.april"/>', '<spring:message code="months.short.may"/>', '<spring:message code="months.short.june"/>', '<spring:message code="months.short.july"/>', '<spring:message code="months.short.august"/>', '<spring:message code="months.short.september"/>', '<spring:message code="months.short.october"/>', '<spring:message code="months.short.november"/>', '<spring:message code="months.short.december"/>'];

var datePickerDays = ['<spring:message code="dow.su"/>', '<spring:message code="dow.mo"/>', '<spring:message code="dow.tu"/>', '<spring:message code="dow.we"/>', '<spring:message code="dow.th"/>', '<spring:message code="dow.fr"/>','<spring:message code="dow.sa"/>', '<spring:message code="dow.su"/>'];

$.fn.datepicker.dates['en'] = {
    days: datePickerDays,
    daysShort: datePickerDays,
    daysMin: datePickerDays,
    months: monthNames,
    monthsShort: monthShortNames,
    today: '<spring:message code="datepicker.today"/>'
};

function makeDateRangePicker(selector) {
    var daterangeLocaleOpts = {
		applyLabel: '<spring:message code="daterange.apply"/>',
		fromLabel: '<spring:message code="daterange.from"/>',
		toLabel: '<spring:message code="daterange.to"/>',
		customRangeLabel: 'Custom Range',
		daysOfWeek: ['<spring:message code="dow.su"/>', '<spring:message code="dow.mo"/>', '<spring:message code="dow.tu"/>', '<spring:message code="dow.we"/>', '<spring:message code="dow.th"/>', '<spring:message code="dow.fr"/>','<spring:message code="dow.sa"/>'],
		monthNames: monthNames,
		firstDay: 1
	};
    
   	var daterangeOpts = {
   		locale: daterangeLocaleOpts,
   		format: 'dd/MM/yyyy'
   	};
   	
   	$(selector).daterangepicker(daterangeOpts);
}

function makeDatePicker(selector) {
	var datepickerOpts = {
		weekStart: 1,
		startDate: '+0d'
	}
	$(selector).datepicker(datepickerOpts);
}
</script>