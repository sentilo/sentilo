<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form" %>
<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8" %>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<spring:url value="/admin/users/${user.userName}/changePassword" var="actionPasswordURL"/>
<spring:message code="user.edit.password" var="pageTitle"/>
<spring:message code="user.password.tooltip" var="passwordTooltip"/>
<spring:eval var="showAdminControls"
             expression="T(org.sentilo.web.catalog.security.SecurityUtils).showAdminControls('EDIT', user)"/>

<script type="text/javascript">

    var actionPasswordURL = '${actionPasswordURL}';

    function showModal() {
        resetErrorList();
        resetFields();
        $('#confirmModal').modal('show');
    }

    function dismissModal() {
        $('#confirmModal').modal('hide');
    }

    function callback(data) {
        resetErrorList();
        if (data.result=='OK') {
            $("#confirmModal").modal('hide');
        } else {
            var errorContainer = $('#error-container');
            var errorList = $('#error-list');
            //Set error messages
            $.each(data.errors,function(key,value) {
                errorList.append('<li class="text-error">' + value + '</li>')
            });
            errorContainer.show();
        }
    }

    function resetErrorList() {
        var errorList = $('#error-list');
        var errorContainer = $('#error-container');
        errorContainer.hide();
        errorList.find('li')
            .remove()
            .end();
    }

    function resetFields() {
        $("#currentPassword").val("");
        $("#newPassword").val("");
        $("#passwordRepeat").val("");
    }

    function submitPassword() {
        var newPassword = $("#newPassword").val();
        var passwordRepeat = $("#passwordRepeat").val();
        var currentPassword = $("#currentPassword").val();
        var userName = "${user.userName}";

        item = {};

        item["userName"] = userName;
        item["newPassword"] = newPassword;
        item["passwordRepeat"] = passwordRepeat;
        item["currentPassword"] = currentPassword;

        var jsonStr = JSON.stringify(item);

        jsonPOST(actionPasswordURL, jsonStr, callback);

    }
</script>

<div id="confirmModal" class="modal hide fade modal-lg" style="width: 700px;" tabindex="-1" role="dialog" aria-labelledby="confirmModalLabel" aria-hidden="true">
    <div class="modal-dialog modal-lg">
        <div class="modal-header">
            <button type="button" class="close" data-dismiss="modal" aria-hidden="true">×</button>
            <h3 id="confirmModalLabel"><spring:message code="user.edit.password"/></h3>
        </div>
        <div class="modal-body">
            <form:form id="passwordFrom" method="post" modelAttribute="password" action="${actionPasswordURL}" class="form-horizontal" >

                <div id="error-container" class="alert alert-block alert-error" style="display: none;">
                    <h5><spring:message code="error.check.form.errors" /></h5>
                    <ul id="error-list">
                    </ul>
                </div>

                <div id="password-container" style="padding-top:50px;">

                    <form:hidden path="userName"/>
                    <c:if test="${not showAdminControls}">
                        <div class="control-group">
                            <form:label path="currentPassword" class="control-label">
                                <spring:message code="user.currentPassword"/>
                            </form:label>
                            <div class="controls">
                                <form:password path="currentPassword" id="currentPassword"/>
                                <form:errors path="currentPassword" cssClass="text-error" htmlEscape="false"/>
                            </div>
                        </div>
                    </c:if>
                    <div class="control-group">
                        <form:label path="newPassword" class="control-label">
                            <spring:message code="user.password"/>
                        </form:label>
                        <div class="controls">
                            <form:password path="newPassword" id="newPassword" tooltip="${passwordTooltip}"/>
                            <form:errors path="newPassword" cssClass="text-error" htmlEscape="false"/>
                        </div>
                    </div>
                    <div class="control-group">
                        <form:label path="passwordRepeat" class="control-label">
                            <spring:message code="user.passwordRepeat"/>
                        </form:label>
                        <div class="controls">
                            <form:password path="passwordRepeat" id="passwordRepeat"
                            />
                            <form:errors path="passwordRepeat" cssClass="text-error"
                                         htmlEscape="false"/>
                        </div>
                    </div>
                </div>
            </form:form>
        </div>
        <div class="modal-footer">
            <a href="#confirmModal" role="button" class="btn" onclick="dismissModal()">
                <spring:message code="button.back"/>
            </a>
            <a href="#confirmModal" role="button" class="btn btn-primary" onclick="submitPassword()">
                <spring:message code="button.save"/>
            </a>
        </div>
    </div>
</div>

