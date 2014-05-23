<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<ul class="nav">
        <li id="locale-menu" class="dropdown">
                <a id="drop4" class="dropdown-toggle" data-toggle="dropdown" role="button" href="#">
                        <spring:message code="menu.locale.title" />
                </a>
                <ul class="dropdown-menu" aria-labelledby="drop4" role="menu">
                        <li><a href="javascript:;" onclick="changeLocale('ca');">Catal&agrave;</a></li>
                        <li><a href="javascript:;" onclick="changeLocale('en');">English</a></li>
                        <li><a href="javascript:;" onclick="changeLocale('it');">Italiano</a></li>
                </ul>
        </li>
</ul>

