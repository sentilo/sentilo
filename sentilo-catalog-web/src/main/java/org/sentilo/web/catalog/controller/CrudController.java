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
package org.sentilo.web.catalog.controller;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.validation.Valid;

import org.sentilo.web.catalog.domain.CatalogDocument;
import org.sentilo.web.catalog.exception.BusinessValidationException;
import org.sentilo.web.catalog.exception.CatalogException;
import org.sentilo.web.catalog.utils.ModelUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.MessageSource;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.servlet.ModelAndView;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

/**
 * Base controller for CRUD use cases.
 */
public abstract class CrudController<T extends CatalogDocument> extends SearchController<T> {

  protected static final String DETAIL_ACTION = "detail";
  protected static final String NEW_ACTION = "new";

  protected static final String RESOURCE_EDITED = "edited";
  protected static final String RESOURCE_CREATED = "created";
  protected static final String RESOURCE_DELETED = "deleted";

  protected abstract T buildNewEntity(String id);

  protected abstract String getEntityModelKey();

  @Autowired
  protected MessageSource messageSource;

  @ExceptionHandler(CatalogException.class)
  public ModelAndView handleCustomException(final CatalogException ex) {
    final String viewName = ex instanceof BusinessValidationException ? "validationFailure" : "catalogFailure";
    final ModelAndView model = new ModelAndView(viewName);
    model.addObject("exception", ex);
    model.addObject("tenantCustomParams", getTenantCustomParams());
    return model;
  }

  protected T addResourceToModel(final String id, final Model model) {
    final T entityToAdd = getService().findAndThrowErrorIfNotExist(buildNewEntity(id));
    model.addAttribute(getEntityModelKey(), entityToAdd);
    return entityToAdd;
  }

  protected void addConfirmationMessageToModel(final String actionFinished, final Model model) {
    final String messageKey = getTypeShortName() + "." + actionFinished;
    ModelUtils.addConfirmationMessageTo(model, messageKey);
  }

  @RequestMapping(value = "/new", method = RequestMethod.GET)
  @PreAuthorize("@accessControlHandler.checkAccess(this, 'CREATE')")
  public String newResource(final HttpServletRequest request, final Model model) {
    doBeforeNewResource(request, model);
    model.addAttribute(getEntityModelKey(), buildNewEntity(null));
    doAfterNewResource(model);
    ModelUtils.setCreateMode(model);
    return getNameOfViewToReturn(NEW_ACTION);
  }

  @RequestMapping(value = "/{id}/edit", method = RequestMethod.GET)
  @PreAuthorize("@accessControlHandler.checkAccess(this, 'EDIT', #id)")
  public String editResource(@PathVariable final String id, final HttpServletRequest request, final Model model) {
    doBeforeEditResource(id, model);
    addResourceToModel(id, model);
    doAfterEditResource(model);
    ModelUtils.setEditMode(model);
    return getNameOfViewToReturn(NEW_ACTION);
  }

  @RequestMapping(value = "/{id}/detail", method = RequestMethod.GET)
  @PreAuthorize("@accessControlHandler.checkAccess(this, 'READ', #id)")
  public String viewResource(@PathVariable final String id, final HttpServletRequest request, final Model model) {
    doBeforeViewResource(id, model);
    addResourceToModel(id, model);
    doAfterViewResource(model);
    ModelUtils.setDetailMode(model);
    return getNameOfViewToReturn(DETAIL_ACTION);
  }

  @RequestMapping(value = "/delete", method = RequestMethod.POST)
  @PreAuthorize("@accessControlHandler.checkAccess(this, 'DELETE', #selectedIds)")
  public String deleteResource(@RequestParam final String[] selectedIds, final HttpServletRequest request,
      final RedirectAttributes redirectAttributes, final Model model) {
    final List<T> resourcesList = buildResourceListFromIds(selectedIds);

    doBeforeDeleteResources(resourcesList, request, model);
    getService().delete(resourcesList);
    addConfirmationMessageToModel(RESOURCE_DELETED, redirectAttributes);
    doAfterDeleteResources(resourcesList, request, model);

    return redirectToList(model, request, redirectAttributes);
  }

  @RequestMapping(value = "/create", method = RequestMethod.POST)
  @PreAuthorize("@accessControlHandler.checkAccess(this, 'SAVE_NEW', #resource)")
  public String createResource(@Valid final T resource, final BindingResult result, final Model model, final RedirectAttributes redirectAttributes,
      final HttpServletRequest request) {

    if (result.hasErrors()) {
      doBeforeNewResource(request, model);
      ModelUtils.setCreateMode(model);
      return getNameOfViewToReturn(NEW_ACTION);
    }

    doBeforeCreateResource(resource, model);
    getService().create(resource);
    addConfirmationMessageToModel(RESOURCE_CREATED, redirectAttributes);
    doAfterCreateResource(resource, redirectAttributes, model);

    // After create a new resource, resources list is show in its initial state
    redirectAttributes.addAttribute("sfbr", "false");

    return redirectToList(model, request, redirectAttributes);
  }

  @RequestMapping(value = "/{id}/edit", method = RequestMethod.POST)
  @PreAuthorize("@accessControlHandler.checkAccess(this, 'SAVE', #resource)")
  public String updateResource(@Valid final T resource, final BindingResult result, @PathVariable final String id, final Model model,
      final RedirectAttributes redirectAttributes, final HttpServletRequest request) {

    if (result.hasErrors()) {
      doBeforeEditResource(id, model);
      ModelUtils.setEditMode(model);
      return getNameOfViewToReturn(NEW_ACTION);
    }

    doBeforeUpdateResource(resource, model);
    getService().update(resource);
    addConfirmationMessageToModel(RESOURCE_EDITED, redirectAttributes);
    doAfterUpdateResource(resource, redirectAttributes, model);

    return redirect(model, request, redirectAttributes);
  }

  protected Class<? extends CatalogDocument> getRowClass() {
    return buildNewEntity("1").getClass();
  }

  protected List<T> buildResourceListFromIds(final String[] resourceIds) {
    final List<T> resources = new ArrayList<T>();
    for (final String id : resourceIds) {
      resources.add(buildNewEntity(id));
    }
    return resources;
  }

  protected void doBeforeNewResource(final HttpServletRequest request, final Model model) {
    // To override by subclasses.
  }

  protected void doBeforeEditResource(final String id, final Model model) {
    // To override by subclasses.
  }

  protected void doBeforeViewResource(final String id, final Model model) {
    // To override by subclasses.
  }

  protected void doBeforeDeleteResources(final Collection<T> resources, final HttpServletRequest request, final Model model) {
    // To override by subclasses.
  }

  protected void doBeforeCreateResource(final T resource, final Model model) {
    // To override by subclasses.
  }

  protected void doBeforeUpdateResource(final T resource, final Model model) {
    // To override by subclasses.
  }

  protected void doAfterDeleteResources(final Collection<T> resources, final HttpServletRequest request, final Model model) {
    // To override by subclasses.
  }

  protected void doAfterCreateResource(final T resource, final RedirectAttributes redirectAttributes, final Model model) {
    // To override by subclasses.
  }

  protected void doAfterUpdateResource(final T resource, final RedirectAttributes redirectAttributes, final Model model) {
    // To override by subclasses.
  }

  protected void doAfterViewResource(final Model model) {
    // To override by subclasses.
  }

  protected void doAfterNewResource(final Model model) {
    // To override by subclasses.
  }

  protected void doAfterEditResource(final Model model) {
    // To override by subclasses.
  }

}
