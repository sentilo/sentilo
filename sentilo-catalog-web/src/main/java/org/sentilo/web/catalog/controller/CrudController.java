/*
 * Sentilo
 * 
 * Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de Barcelona.
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
import java.util.Date;
import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.validation.Valid;

import org.sentilo.web.catalog.domain.CatalogDocument;
import org.sentilo.web.catalog.format.datetime.LocalDateFormatter;
import org.sentilo.web.catalog.utils.ModelUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.MessageSource;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

/**
 * Base controller for CRUD use cases.
 */
public abstract class CrudController<T extends CatalogDocument> extends SearchController<T> {

  protected static final String DETAIL_ACTION = "detail";
  protected static final String NEW_ACTION = "new";

  protected static final String RESOURCE_EDITED = "edited";
  protected static final String RESOURCE_CREATED = "created";
  protected static final String RESOURCE_DELETED = "deleted";

  private String typeShortName;

  protected abstract T buildNewEntity(String id);

  protected abstract String getEntityModelKey();

  @Autowired
  private LocalDateFormatter localDateFormat;

  @Autowired
  protected MessageSource messageSource;

  protected T addResourceToModel(final String id, final Model model) {
    final T entityToAdd = getService().findAndThrowErrorIfNotExist(buildNewEntity(id));
    model.addAttribute(getEntityModelKey(), entityToAdd);
    return entityToAdd;
  }

  protected void addConfirmationMessageToModel(final String actionFinished, final Model model) {
    final String messageKey = getTypeShortNameName() + "." + actionFinished;
    ModelUtils.addConfirmationMessageTo(model, messageKey);
  }

  private String getTypeShortNameName() {
    if (typeShortName == null) {
      // Para obtener el nombre debemos primero tener una instancia del recurso sobre el cual se
      // hace el CRUD.
      // Para ello invocamos al metodo buildNewEntity con un valor por defecto
      final T resource = buildNewEntity("1");
      final String fullName = resource.getClass().getName();
      final int lastPos = fullName.lastIndexOf('.');
      typeShortName = fullName.substring(lastPos + 1).toLowerCase();
    }
    return typeShortName;
  }

  @RequestMapping(value = "/new", method = RequestMethod.GET)
  public String newResource(final HttpServletRequest request, final Model model) {
    doBeforeNewResource(request, model);
    model.addAttribute(getEntityModelKey(), buildNewEntity(null));
    ModelUtils.setCreateMode(model);
    return getNameOfViewToReturn(NEW_ACTION);
  }

  @RequestMapping(value = "/{id}/edit", method = RequestMethod.GET)
  public String editResource(@PathVariable final String id, final HttpServletRequest request, final Model model) {
    doBeforeEditResource(model);
    addResourceToModel(id, model);
    ModelUtils.setEditMode(model);
    return getNameOfViewToReturn(NEW_ACTION);
  }

  @RequestMapping(value = "/{id}/detail", method = RequestMethod.GET)
  public String viewResource(@PathVariable final String id, final HttpServletRequest request, final Model model) {
    doBeforeViewResource(id, model);
    addResourceToModel(id, model);
    doAfterViewResource(model);
    ModelUtils.setDetailMode(model);
    return getNameOfViewToReturn(DETAIL_ACTION);
  }

  @RequestMapping(value = "/delete", method = RequestMethod.POST)
  public String deleteResource(@RequestParam final String[] selectedIds, final HttpServletRequest request, final Model model) {
    doBeforeDeleteResource(selectedIds, request, model);
    getService().delete(buildResourceListFromIds(selectedIds));
    addConfirmationMessageToModel(RESOURCE_DELETED, model);
    doAfterDeleteResource(request, model);
    return getNameOfViewToReturn(LIST_ACTION);
  }

  @RequestMapping(value = "/create", method = RequestMethod.POST)
  public String createResource(@Valid final T resource, final BindingResult result, final Model model) {
    if (result.hasErrors()) {
      doBeforeEditResource(model);
      ModelUtils.setCreateMode(model);
      return getNameOfViewToReturn(NEW_ACTION);
    }

    doBeforeCreateResource(resource, model);
    resource.setCreatedAt(new Date());
    resource.setUpdateAt(new Date());
    getService().create(resource);
    addConfirmationMessageToModel(RESOURCE_CREATED, model);
    doAfterCreateResource(model);
    return getNameOfViewToReturn(LIST_ACTION);
  }

  @RequestMapping(value = "/{id}/edit", method = RequestMethod.POST)
  public String updateResource(@Valid final T resource, final BindingResult result, @PathVariable final String id, final Model model) {
    if (result.hasErrors()) {
      doBeforeEditResource(model);
      ModelUtils.setEditMode(model);
      return getNameOfViewToReturn(NEW_ACTION);
    }
    doBeforeUpdateResource(resource, model);
    resource.setUpdateAt(new Date());
    getService().update(resource);
    addConfirmationMessageToModel(RESOURCE_EDITED, model);
    doAfterUpdateResource(model);
    return getNameOfViewToReturn(LIST_ACTION);
  }

  protected List<T> buildResourceListFromIds(final String[] resourceIds) {
    final List<T> resources = new ArrayList<T>();
    for (final String id : resourceIds) {
      resources.add(buildNewEntity(id));
    }
    return resources;
  }

  public LocalDateFormatter getLocalDateFormat() {
    return localDateFormat;
  }

  protected void doBeforeNewResource(final HttpServletRequest request, final Model model) {
    // To override by subclasses.
  }

  protected void doBeforeEditResource(final Model model) {
    // To override by subclasses.
  }

  protected void doBeforeViewResource(final String id, final Model model) {
    // To override by subclasses.
  }

  protected void doBeforeDeleteResource(final String[] selectedIds, final HttpServletRequest request, final Model model) {
    // To override by subclasses.
  }

  protected void doBeforeCreateResource(final T resource, final Model model) {
    // To override by subclasses.
  }

  protected void doBeforeUpdateResource(final T resource, final Model model) {
    // To override by subclasses.
  }

  protected void doAfterDeleteResource(final HttpServletRequest request, final Model model) {
    // To override by subclasses.
  }

  protected void doAfterCreateResource(final Model model) {
    // To override by subclasses.
  }

  protected void doAfterUpdateResource(final Model model) {
    // To override by subclasses.
  }

  protected void doAfterViewResource(final Model model) {
    // TODO Auto-generated method stub
  }
}
