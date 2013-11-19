/*
 * Sentilo
 *   
 * Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de  Barcelona.
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
package org.sentilo.web.catalog.controller;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.validation.Valid;

import org.sentilo.web.catalog.domain.CatalogDocument;
import org.sentilo.web.catalog.utils.ModelUtils;
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

	protected T addResourceToModel(String id, Model model) {
		T entityToAdd = getService().findAndThrowErrorIfNotExist(buildNewEntity(id));
		model.addAttribute(getEntityModelKey(), entityToAdd);
		return entityToAdd;
	}

	protected void addConfirmationMessageToModel(String actionFinished, Model model) {
		String messageKey = getTypeShortNameName() + "." + actionFinished;
		ModelUtils.addConfirmationMessageTo(model, messageKey);
	}

	private String getTypeShortNameName() {
		if (typeShortName == null) {
			// Para obtener el nombre debemos primero tener una instancia del recurso sobre el cual se hace el CRUD.
			// Para ello invocamos al metodo buildNewEntity con un valor por defecto
			T resource = buildNewEntity("1");
			String fullName = resource.getClass().getName();
			int lastPos = fullName.lastIndexOf(".");
			typeShortName = fullName.substring(lastPos + 1).toLowerCase();
		}
		return typeShortName;
	}

	@RequestMapping(value = "/new", method = RequestMethod.GET)
	public String newResource(HttpServletRequest request, Model model) {
		doBeforeNewResource(request, model);
		model.addAttribute(getEntityModelKey(), buildNewEntity(null));
		ModelUtils.setCreateMode(model);
		return getNameOfViewToReturn(NEW_ACTION);
	}

	@RequestMapping(value = "/{id}/edit", method = RequestMethod.GET)
	public String editResource(@PathVariable String id, HttpServletRequest request, Model model) {
		doBeforeEditResource(model);
		addResourceToModel(id, model);
		ModelUtils.setEditMode(model);
		return getNameOfViewToReturn(NEW_ACTION);
	}

	@RequestMapping(value = "/{id}/detail", method = RequestMethod.GET)
	public String viewResource(@PathVariable String id, HttpServletRequest request, Model model) {
		doBeforeViewResource(id, model);
		addResourceToModel(id, model);
		ModelUtils.setDetailMode(model);
		return getNameOfViewToReturn(DETAIL_ACTION);
	}

	@RequestMapping(value = "/delete", method = RequestMethod.POST)
	public String deleteResource(@RequestParam String[] selectedIds, HttpServletRequest request, Model model) {		
		doBeforeDeleteResource(selectedIds, request, model);
		getService().delete(buildResourceListFromIds(selectedIds));
		addConfirmationMessageToModel(RESOURCE_DELETED, model);
		doAfterDeleteResource(request, model);
		return getNameOfViewToReturn(LIST_ACTION);
	}

	@RequestMapping(value = "/create", method = RequestMethod.POST)
	public String createResource(@Valid T resource, BindingResult result, Model model) {
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
	public String updateResource(@Valid T resource, BindingResult result, @PathVariable String id, Model model) {
		if (result.hasErrors()) {
			doBeforeEditResource(model);
			ModelUtils.setEditMode(model);
			return getNameOfViewToReturn(NEW_ACTION);
		}
		resource.setUpdateAt(new Date());
		getService().update(resource);
		addConfirmationMessageToModel(RESOURCE_EDITED, model);
		doAfterUpdateResource(model);
		return getNameOfViewToReturn(LIST_ACTION);
	}

	protected List<T> buildResourceListFromIds(String[] resourceIds) {
		List<T> resources = new ArrayList<T>();
		for (String id : resourceIds) {
			resources.add(buildNewEntity(id));
		}
		return resources;
	}

	protected void doBeforeNewResource(HttpServletRequest request, Model model) {
		//To override by subclasses.
	}

	protected void doBeforeEditResource(Model model) {
		//To override by subclasses.
	}

	protected void doBeforeViewResource(String id, Model model) {
		//To override by subclasses.
	}

	protected void doBeforeDeleteResource(String[] selectedIds, HttpServletRequest request, Model model) {
		//To override by subclasses.
	}

	protected void doBeforeCreateResource(T resource, Model model) {
		//To override by subclasses.
	}

	protected void doAfterDeleteResource(HttpServletRequest request, Model model) {
		//To override by subclasses.
	}

	protected void doAfterCreateResource(Model model) {
		//To override by subclasses.
	}

	protected void doAfterUpdateResource(Model model) {
		//To override by subclasses.
	}
}
