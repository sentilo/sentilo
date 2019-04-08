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
package org.sentilo.web.catalog.controller.admin;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.sentilo.web.catalog.context.TenantContextHolder;
import org.sentilo.web.catalog.controller.CrudController;
import org.sentilo.web.catalog.domain.CatalogDocument;
import org.sentilo.web.catalog.domain.DocumentFile;
import org.sentilo.web.catalog.domain.Provider;
import org.sentilo.web.catalog.exception.DocumentFileException;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.service.CrudService;
import org.sentilo.web.catalog.service.DocumentFilesService;
import org.sentilo.web.catalog.service.ProviderService;
import org.sentilo.web.catalog.utils.Constants;
import org.sentilo.web.catalog.utils.TenantUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.util.FileCopyUtils;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

@Controller
@RequestMapping("/admin/documents")
public class DocumentFilesController extends CrudController<DocumentFile> {

  @Autowired
  private DocumentFilesService documentFilesService;

  @Autowired
  private ProviderService providerService;

  @ModelAttribute(Constants.MODEL_ACTIVE_MENU)
  public String getActiveMenu() {
    return Constants.MENU_PROVIDER;
  }

  @RequestMapping(value = "/doc/{id}/detail", method = RequestMethod.GET)
  @PreAuthorize("@accessControlHandler.checkAccess(this, 'READ', #id)")
  public String downloadDocumentFile(@PathVariable final String id, final HttpServletRequest request, final Model model,
      final HttpServletResponse response) {

    final DocumentFile documentFile = documentFilesService.find(new DocumentFile(id));

    try {
      response.setContentType(documentFile.getContentType());
      response.setContentLength(Long.valueOf(documentFile.getFilesize()).intValue());
      response.setHeader("Content-Description", documentFile.getName());
      response.setHeader("Content-Disposition", String.format("attachment; filename=\"%s\"", documentFile.getFilename()));

      FileCopyUtils.copy(documentFile.getContent(), response.getOutputStream());

      response.flushBuffer();
    } catch (final IOException ex) {
      LOGGER.info("Error writing file to output stream. Filename was '{}'", documentFile.getName(), ex);
      throw new RuntimeException("IOError writing file " + documentFile.getName() + " to output stream");
    }

    return null;
  }

  protected void doBeforeCreateResource(final DocumentFile resource, final Model model) {
    // Filled in the document derived attributes .
    try {
      resource.setContentType(resource.getFile().getContentType());
      resource.setContent(resource.getFile().getBytes());
      resource.setFilesize(resource.getFile().getSize());
      resource.setFilename(resource.getFile().getOriginalFilename());
    } catch (final IOException ioe) {
      throw new DocumentFileException(ioe);
    }
  }

  protected void doBeforeNewResource(final HttpServletRequest request, final Model model) {
    final String entityId = request.getParameter(Constants.MODEL_ENTITY_ID);
    if (StringUtils.hasText(entityId)) {
      model.addAttribute(Constants.MODEL_ENTITY_ID, entityId);
      // For now, only providers could have documents attached.
      model.addAttribute(Constants.MODEL_ENTITY, providerService.find(new Provider(entityId)));
    }
  }

  @Override
  protected void doBeforeSearchPage(final HttpServletRequest request, final SearchFilter filter) {
    final String entityId = request.getParameter(Constants.MODEL_ENTITY_ID);
    if (StringUtils.hasText(entityId)) {
      filter.addAndParam("entityId", entityId);
    }

    // Include / Exclude fields from documentFile collection
    filter.excludeField("content");
  }

  protected String redirectToList(final Model model, final HttpServletRequest request, final RedirectAttributes redirectAttributes) {
    // redirect must be to the provider detail page, but showing by default the document files tab
    redirectAttributes.addFlashAttribute(Constants.MODEL_OPENED_TAB, Constants.TAB_5);

    final String entityId = request.getParameter(Constants.MODEL_ENTITY_ID);
    final StringBuilder sb = new StringBuilder("/");
    if (TenantContextHolder.hasContext()) {
      sb.append(TenantUtils.getCurrentTenant());
      sb.append("/");
    }

    sb.append("admin/provider/" + entityId + "/detail");

    final String redirectUrl = redirectStrategy.buildRedirectUrl(request, sb.toString());
    return "redirect:" + redirectUrl;
  }

  @Override
  protected void initViewNames() {
    getViewNames().put(LIST_ACTION, Constants.VIEW_SENSOR_LIST);
    getViewNames().put(NEW_ACTION, Constants.VIEW_PROVIDER_ADD_DOCUMENT_FILE);
  }

  @Override
  protected DocumentFile buildNewEntity(final String id) {
    return new DocumentFile(id);
  }

  @Override
  protected CrudService<DocumentFile> getService() {
    return documentFilesService;
  }

  @Override
  protected List<String> toRow(final DocumentFile resource) {
    final List<String> row = new ArrayList<String>();
    row.add(resource.getId()); // checkbox
    row.add(resource.getName());
    row.add(resource.getDescription());
    row.add(resource.getContentType());
    row.add(resource.getCreatedBy());
    row.add(getLocalDateFormat().printAsLocalTime(resource.getCreatedAt(), Constants.DATETIME_FORMAT));
    return row;
  }

  @Override
  protected Class<? extends CatalogDocument> getRowClass() {
    return DocumentFile.class;
  }

  @Override
  protected String getEntityModelKey() {
    return Constants.MODEL_DOCUMENT_FILE;
  }

  @Override
  protected void doBeforeExcelBuilder(final Model model) {
    // Nothing to do
  }
}
