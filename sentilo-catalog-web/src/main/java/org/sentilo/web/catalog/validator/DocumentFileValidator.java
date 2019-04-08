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
package org.sentilo.web.catalog.validator;

import java.util.regex.Pattern;

import org.sentilo.web.catalog.domain.DocumentFile;
import org.sentilo.web.catalog.service.DocumentFilesService;
import org.sentilo.web.catalog.utils.Constants;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

@SentiloValidator
public class DocumentFileValidator implements Validator {

  private static final long MAX_FILE_SIZE = 4194304; // 4MB

  public static final String ERROR_CODE_EMPTY_FILE = "document.file.validation.error.file.empty";
  public static final String ERROR_CODE_MAX_FILESIZE = "document.file.validation.error.file.size";
  public static final String ERROR_CODE_FILE_ALREADY_EXISTS = "document.file.validation.error.file.exists";
  public static final String ERROR_CODE_FILENAME_FORMAT = "document.file.validation.error.file.nameFormat";

  @Autowired
  private DocumentFilesService documentFilesService;

  @Override
  public boolean supports(final Class<?> clazz) {
    return DocumentFile.class.equals(clazz);
  }

  @Override
  public void validate(final Object target, final Errors errors) {
    final DocumentFile document = (DocumentFile) target;

    // Test file exists
    if (document.getFile() == null || document.getFile().isEmpty()) {
      errors.rejectValue("file", ERROR_CODE_EMPTY_FILE);
      return;
    }

    // Test file size
    if (document.getFile().getSize() > MAX_FILE_SIZE) {
      errors.rejectValue("file", ERROR_CODE_MAX_FILESIZE);
      return;
    }

    // Check file existence, so we'll inform user with a related message
    // The document id is formed by: entityId.physicDocumentName
    // In this way, we can't have two files with same physic file name
    if (documentFilesService.checkDuplicateFilename(document.getEntityId(), document.getFile().getOriginalFilename())) {
      errors.rejectValue("file", ERROR_CODE_FILE_ALREADY_EXISTS);
      return;
    }

    // Check filename characters format
    final Pattern filenamePattern = Pattern.compile(Constants.VALIDATION_FILENAME_REGEXP);
    if (!filenamePattern.matcher(document.getFile().getOriginalFilename()).matches()) {
      errors.rejectValue("file", ERROR_CODE_FILENAME_FORMAT);
      return;
    }
  }
}
