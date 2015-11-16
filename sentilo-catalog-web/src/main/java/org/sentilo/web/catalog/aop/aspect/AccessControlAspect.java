/*
 * Sentilo
 *  
 * Original version 1.4 Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de Barcelona.
 * Modified by Opentrends adding support for multitenant deployments and SaaS. Modifications on version 1.5 Copyright (C) 2015 Opentrends Solucions i Sistemes, S.L.
 * 
 *   
 * This program is licensed and may be used, modified and redistributed under the
 * terms  of the European Public License (EUPL), either version 1.1 or (at your 
 * option) any later version as soon as they are approved by the European 
 * Commission.
 *   
 * Alternatively, you may redistribute and/or modify this program under the terms
 * of the GNU Lesser General Public License as published by the Free Software 
 * Foundation; either  version 3 of the License, or (at your option) any later 
 * version. 
 *   
 * Unless required by applicable law or agreed to in writing, software distributed
 * under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR 
 * CONDITIONS OF ANY KIND, either express or implied. 
 *   
 * See the licenses for the specific language governing permissions, limitations 
 * and more details.
 *   
 * You should have received a copy of the EUPL1.1 and the LGPLv3 licenses along 
 * with this program; if not, you may find them at: 
 *   
 *   https://joinup.ec.europa.eu/software/page/eupl/licence-eupl
 *   http://www.gnu.org/licenses/ 
 *   and 
 *   https://www.gnu.org/licenses/lgpl.txt
 */
package org.sentilo.web.catalog.aop.aspect;

import java.lang.reflect.Method;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.annotation.Before;
import org.aspectj.lang.annotation.Pointcut;
import org.sentilo.web.catalog.controller.CrudController;
import org.sentilo.web.catalog.domain.CatalogDocument;
import org.sentilo.web.catalog.security.access.AccessControlContext;
import org.sentilo.web.catalog.security.access.AccessControlService;
import org.sentilo.web.catalog.security.enums.ActionType;
import org.sentilo.web.catalog.service.CrudService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.AnnotationUtils;
import org.springframework.stereotype.Component;
import org.springframework.util.ReflectionUtils;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

/**
 * 
 * @deprecated Deprecated in favor of @PreAuthorize annotation to mark a method as secured
 * 
 */
@Component("accessControl")
// @Aspect
@Deprecated
public class AccessControlAspect implements Ordered {

  private static final int ORDER = 1;

  private final Map<String, ActionType> getActionMapping = new HashMap<String, ActionType>();
  private final Map<String, ActionType> postActionMapping = new HashMap<String, ActionType>();

  @Autowired
  private AccessControlService accessControlService;

  public AccessControlAspect() {
    super();
    init();
  }

  @Pointcut("execution(* org.sentilo.web.catalog.controller.CrudController.*(..))")
  public void crudMethod() {
    // Do nothing. Pointcut definition
  }

  @Pointcut("execution(* org.sentilo.web.catalog.controller.SearchController.*(..))")
  public void searchMethod() {
    // Do nothing. Pointcut definition
  }

  @SuppressWarnings("unchecked")
  @Before("(crudMethod() || searchMethod()) && @annotation(requestMapping) && !@annotation(org.springframework.web.bind.annotation.ResponseBody)")
  public void checkControlAccess(final JoinPoint jp, final RequestMapping requestMapping) throws Exception {
    final Object[] args = jp.getArgs();

    final CrudController<CatalogDocument> controller = (CrudController<CatalogDocument>) jp.getTarget();

    if (requestMustBeChecked(controller)) {

      final CrudService<CatalogDocument> service = getService(controller);
      final Object target = getTarget(controller, args, requestMapping);
      final ActionType action = getAction(requestMapping);

      if (target instanceof CatalogDocument || target == null) {
        accessControlService.checkAccess(new AccessControlContext((CatalogDocument) target, action, service));
      } else if (target instanceof Collection<?>) {
        for (final CatalogDocument resource : (Collection<CatalogDocument>) target) {
          accessControlService.checkAccess(new AccessControlContext(resource, action, service));
        }
      }
    }

    return;
  }

  private boolean requestMustBeChecked(final CrudController<CatalogDocument> controller) {
    // request must be checked if and only if it is a restricted request, i.e, it is handled by an
    // admin controller
    final RequestMapping requestMapping = AnnotationUtils.findAnnotation(controller.getClass(), RequestMapping.class);
    return requestMapping.value()[0].startsWith("/admin");
  }

  private ActionType getAction(final RequestMapping requestMapping) {
    // Get the first element of the array: only is necessary to get the method action.
    final String value = requestMapping.value()[0];
    return (requestMapping.method()[0] == RequestMethod.GET ? getActionMapping.get(value) : postActionMapping.get(value));
  }

  private void init() {
    getActionMapping.put("/", ActionType.LIST);
    getActionMapping.put("/list", ActionType.LIST);
    getActionMapping.put("/list/excel", ActionType.LIST);
    getActionMapping.put("/list/json", ActionType.LIST);
    getActionMapping.put("/new", ActionType.CREATE);
    getActionMapping.put("/{id}/edit", ActionType.EDIT);
    getActionMapping.put("/{id}/detail", ActionType.READ);
    postActionMapping.put("/{id}/delete", ActionType.DELETE);
    postActionMapping.put("/create", ActionType.SAVE_NEW);
    postActionMapping.put("/{id}/edit", ActionType.SAVE);
    postActionMapping.put("/delete", ActionType.DELETE);
  }

  private Object getTarget(final CrudController<CatalogDocument> controller, final Object[] args, final RequestMapping requestMapping)
      throws Exception {
    Object resource = null;

    if (requestMapping.method()[0] == RequestMethod.POST && requestMapping.value()[0].equals("/delete")) {
      return buildNewEntities(controller, (String[]) args[0]);
    }

    if (requestMapping.method()[0] == RequestMethod.GET) {
      final String entityId = (args.length == 3 && !isListRequest(requestMapping) ? (String) args[0] : "-1");
      return buildNewEntity(controller, entityId);
    }

    for (final Object arg : args) {
      if (arg instanceof CatalogDocument) {
        resource = arg;
        return resource;
      }
    }

    return resource;
  }

  private boolean isListRequest(final RequestMapping requestMapping) {
    for (final String value : requestMapping.value()) {
      if (value.startsWith("/list")) {
        return true;
      }
    }

    return false;
  }

  @SuppressWarnings("unchecked")
  private CrudService<CatalogDocument> getService(final CrudController<CatalogDocument> controller) throws Exception {
    return (CrudService<CatalogDocument>) invokeMethod(controller, "getService", null, new Class<?>[] {});
  }

  private CatalogDocument buildNewEntity(final CrudController<CatalogDocument> controller, final String id) throws Exception {
    return (CatalogDocument) invokeMethod(controller, "buildNewEntity", id, new Class<?>[] {String.class});
  }

  @SuppressWarnings("unchecked")
  private List<CatalogDocument> buildNewEntities(final CrudController<CatalogDocument> controller, final String[] ids) throws Exception {
    return (List<CatalogDocument>) invokeMethod(controller, "buildResourceListFromIds", ids, new Class<?>[] {String[].class});
  }

  private Object invokeMethod(final Object obj, final String methodName, final Object args, final Class<?>... paramTypes) throws Exception {
    final Method m = ReflectionUtils.findMethod(obj.getClass(), methodName, paramTypes);
    ReflectionUtils.makeAccessible(m);
    return (args == null ? m.invoke(obj, new Object[] {}) : m.invoke(obj, new Object[] {args}));
  }

  @Override
  public int getOrder() {
    return ORDER;
  }

}
