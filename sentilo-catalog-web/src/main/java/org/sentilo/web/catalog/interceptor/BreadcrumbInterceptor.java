/*
 * Sentilo
 *
 * Original version 1.4 Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de
 * Barcelona. Modified by Opentrends adding support for multitenant deployments and SaaS.
 * Modifications on version 1.5 Copyright (C) 2015 Opentrends Solucions i Sistemes, S.L.
 *
 *
 * This program is licensed and may be used, modified and redistributed under the terms of the
 * European Public License (EUPL), either version 1.1 or (at your option) any later version as soon
 * as they are approved by the European Commission.
 *
 * Alternatively, you may redistribute and/or modify this program under the terms of the GNU Lesser
 * General Public License as published by the Free Software Foundation; either version 3 of the
 * License, or (at your option) any later version.
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied.
 *
 * See the licenses for the specific language governing permissions, limitations and more details.
 *
 * You should have received a copy of the EUPL1.1 and the LGPLv3 licenses along with this program;
 * if not, you may find them at:
 *
 * https://joinup.ec.europa.eu/software/page/eupl/licence-eupl http://www.gnu.org/licenses/ and
 * https://www.gnu.org/licenses/lgpl.txt
 */
package org.sentilo.web.catalog.interceptor;

import java.lang.annotation.Annotation;
import java.util.Arrays;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.sentilo.web.catalog.breadcrumb.BreadcrumbsTrail;
import org.springframework.http.MediaType;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.method.HandlerMethod;
import org.springframework.web.servlet.handler.HandlerInterceptorAdapter;

public class BreadcrumbInterceptor extends HandlerInterceptorAdapter {

  private static final String BACK_ACTION_PARAM = "sfbr";
  private static final String FROM_MENU_ACTION_PARAM = "sfamr";
  private static final String BREADCRUMB_KEY = "currentBreadCrumb";
  private static final String REFERER_HEADER = "referer";

  public boolean preHandle(final HttpServletRequest request, final HttpServletResponse response, final Object handler) throws Exception {
    final Annotation annotation = getMethodAnnotation(handler);
    if (annotation != null) {
      processAnnotation(request, (RequestMapping) annotation);
    }

    return true;
  }

  private void processAnnotation(final HttpServletRequest request, final RequestMapping annotation) {

    final boolean fromBackRequest = Boolean.parseBoolean(request.getParameter(BACK_ACTION_PARAM));
    final boolean fromAdminMenuRequest = Boolean.parseBoolean(request.getParameter(FROM_MENU_ACTION_PARAM));
    final String refererUrl = request.getHeader(REFERER_HEADER);

    if (fromAdminMenuRequest || !StringUtils.hasText(refererUrl)) {
      emptyCurrentBreadCrumb(request);
    } else if (fromBackRequest) {
      pollBreadcrumb(request);
    } else if (isBreadcrumbRequestCandidate(request, annotation)) {
      pushBreadcrumb(request, refererUrl);
    }

  }

  private BreadcrumbsTrail getBreadCrumbsTrail(final HttpServletRequest request) {
    BreadcrumbsTrail obj = (BreadcrumbsTrail) request.getSession().getAttribute(BREADCRUMB_KEY);
    if (obj == null) {
      obj = new BreadcrumbsTrail();
      request.getSession().setAttribute(BREADCRUMB_KEY, obj);
    }

    return obj;
  }

  private boolean isBreadcrumbRequestCandidate(final HttpServletRequest request, final RequestMapping annotation) {
    // Only /admin GET requests which reload page are candidates to be stored into breadcrumb list
    return isAdminRequest(request) && annotation.method().length == 1 && RequestMethod.GET == annotation.method()[0]
        && !Arrays.asList(annotation.produces()).contains(MediaType.APPLICATION_JSON_VALUE);
  }

  private boolean isAdminRequest(final HttpServletRequest request) {
    return request.getServletPath().startsWith("/admin");
  }

  private void emptyCurrentBreadCrumb(final HttpServletRequest request) {
    getBreadCrumbsTrail(request).clear();
  }

  private void pollBreadcrumb(final HttpServletRequest request) {
    getBreadCrumbsTrail(request).poll();
  }

  private void pushBreadcrumb(final HttpServletRequest request, final String linkUrl) {
    // Before to add linkUrl to breadcrumb queue, the navigation url params should be deleted
    final String cleanLinkUrl = filterControlBreadcrumbParams(linkUrl);
    getBreadCrumbsTrail(request).push(cleanLinkUrl);
  }

  private String filterControlBreadcrumbParams(final String linkUrl) {
    String filteredLinkUrl = linkUrl;

    if (linkUrl.contains("?") && (linkUrl.contains(BACK_ACTION_PARAM) || linkUrl.contains(FROM_MENU_ACTION_PARAM))) {
      final int pos = linkUrl.indexOf('?');
      final String[] queryParameters = linkUrl.substring(pos + 1).split("&");
      final StringBuilder filteredQueryParameters = new StringBuilder("");
      for (final String queryParam : queryParameters) {
        if (queryParam.startsWith(BACK_ACTION_PARAM) || queryParam.startsWith(FROM_MENU_ACTION_PARAM)) {
          continue;
        }

        if (filteredQueryParameters.length() > 0) {
          filteredQueryParameters.append("&");
        }
        filteredQueryParameters.append(queryParam);
      }

      filteredLinkUrl = linkUrl.substring(0, pos + 1) + filteredQueryParameters.toString();
    }

    return filteredLinkUrl;
  }

  private Annotation getMethodAnnotation(final Object handler) {
    if (handler instanceof HandlerMethod) {
      final HandlerMethod handlerMethod = (HandlerMethod) handler;
      return handlerMethod.getMethodAnnotation(RequestMapping.class);
    } else {
      return null;
    }
  }

}
