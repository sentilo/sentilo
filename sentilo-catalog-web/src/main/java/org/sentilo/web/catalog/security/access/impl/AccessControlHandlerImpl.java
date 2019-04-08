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
package org.sentilo.web.catalog.security.access.impl;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Collection;
import java.util.List;

import org.sentilo.web.catalog.controller.CrudController;
import org.sentilo.web.catalog.domain.CatalogDocument;
import org.sentilo.web.catalog.exception.NotAllowedActionException;
import org.sentilo.web.catalog.security.access.AccessControlContext;
import org.sentilo.web.catalog.security.access.AccessControlHandler;
import org.sentilo.web.catalog.security.access.AccessControlService;
import org.sentilo.web.catalog.security.enums.ActionType;
import org.sentilo.web.catalog.service.CrudService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.annotation.AnnotationUtils;
import org.springframework.stereotype.Component;
import org.springframework.util.ReflectionUtils;
import org.springframework.web.bind.annotation.RequestMapping;

@Component("accessControlHandler")
public class AccessControlHandlerImpl implements AccessControlHandler {

  private static final Logger LOGGER = LoggerFactory.getLogger(AccessControlHandlerImpl.class);

  @Autowired
  private AccessControlService accessControlService;

  @Override
  public boolean checkAccess(final CrudController<CatalogDocument> controller, final ActionType action) {
    return checkAccess(controller, action, null);

  }

  @SuppressWarnings("unchecked")
  @Override
  public boolean checkAccess(final CrudController<CatalogDocument> controller, final ActionType action, final Object targetParam) {
    if (requestMustBeChecked(controller)) {
      try {
        final CrudService<CatalogDocument> service = getService(controller);
        final Object target = getTarget(controller, targetParam);

        if (target instanceof CatalogDocument || target == null) {
          accessControlService.checkAccess(new AccessControlContext((CatalogDocument) target, action, service));
        } else if (target instanceof Collection<?>) {
          for (final CatalogDocument resource : (Collection<CatalogDocument>) target) {
            accessControlService.checkAccess(new AccessControlContext(resource, action, service));
          }
        }
      } catch (final NotAllowedActionException e) {
        LOGGER.warn("Access to controller denied: {}", e.getMessage());
        return false;
      } catch (final Exception ex) {
        LOGGER.warn("An error has happened while checking access control.", ex);
        return false;
      }
    }

    return true;
  }

  private boolean requestMustBeChecked(final CrudController<CatalogDocument> controller) {
    // request must be checked if and only if it is a restricted request, i.e, it is handled by an
    // admin controller
    final RequestMapping requestMapping = AnnotationUtils.findAnnotation(controller.getClass(), RequestMapping.class);
    return requestMapping.value()[0].startsWith("/admin");
  }

  private Object getTarget(final CrudController<CatalogDocument> controller, final Object targetParam) throws Exception {
    final Object resource = null;

    if (targetParam instanceof CatalogDocument) {
      return targetParam;
    } else if (targetParam instanceof String[]) {
      return buildNewEntities(controller, (String[]) targetParam);
    } else if (targetParam instanceof String || targetParam == null) {
      return buildNewEntity(controller, (String) (targetParam == null ? "-1" : targetParam));
    }

    return resource;
  }

  @SuppressWarnings("unchecked")
  private CrudService<CatalogDocument> getService(final CrudController<CatalogDocument> controller)
      throws IllegalAccessException, IllegalArgumentException, InvocationTargetException {
    return (CrudService<CatalogDocument>) invokeMethod(controller, "getService", null, new Class<?>[] {});
  }

  private CatalogDocument buildNewEntity(final CrudController<CatalogDocument> controller, final String id)
      throws IllegalAccessException, IllegalArgumentException, InvocationTargetException {
    return (CatalogDocument) invokeMethod(controller, "buildNewEntity", id, new Class<?>[] {String.class});
  }

  @SuppressWarnings("unchecked")
  private List<CatalogDocument> buildNewEntities(final CrudController<CatalogDocument> controller, final String[] ids)
      throws IllegalAccessException, IllegalArgumentException, InvocationTargetException {
    return (List<CatalogDocument>) invokeMethod(controller, "buildResourceListFromIds", ids, new Class<?>[] {String[].class});
  }

  private Object invokeMethod(final Object obj, final String methodName, final Object args, final Class<?>... paramTypes)
      throws IllegalAccessException, IllegalArgumentException, InvocationTargetException {
    final Method m = ReflectionUtils.findMethod(obj.getClass(), methodName, paramTypes);
    ReflectionUtils.makeAccessible(m);
    return args == null ? m.invoke(obj, new Object[] {}) : m.invoke(obj, new Object[] {args});
  }
}
