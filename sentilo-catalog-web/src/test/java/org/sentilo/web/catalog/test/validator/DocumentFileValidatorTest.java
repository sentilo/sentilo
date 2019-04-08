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
package org.sentilo.web.catalog.test.validator;

import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.web.catalog.domain.DocumentFile;
import org.sentilo.web.catalog.service.DocumentFilesService;
import org.sentilo.web.catalog.validator.DocumentFileValidator;
import org.springframework.validation.Errors;
import org.springframework.web.multipart.MultipartFile;

public class DocumentFileValidatorTest {

  @InjectMocks
  private DocumentFileValidator validator;

  @Mock
  private DocumentFilesService documentFileService;

  @Mock
  private Errors errors;

  @Mock
  private DocumentFile document;

  @Mock
  private MultipartFile file;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void supports() {
    Assert.assertTrue(validator.supports(DocumentFile.class));
  }

  @Test
  public void fileFileFieldMandatoryTest() {
    when(document.getFile()).thenReturn(file);
    when(file.isEmpty()).thenReturn(true);

    validator.validate(document, errors);

    verify(errors, times(1)).rejectValue(eq("file"), eq(DocumentFileValidator.ERROR_CODE_EMPTY_FILE));
  }

  @Test
  public void fileFileSizeTest() {
    when(file.getSize()).thenReturn(new Long(16000000));
    when(document.getFile()).thenReturn(file);
    when(file.isEmpty()).thenReturn(false);

    validator.validate(document, errors);

    verify(errors, times(1)).rejectValue(eq("file"), eq(DocumentFileValidator.ERROR_CODE_MAX_FILESIZE));
  }

  @Test
  public void fileFileExistsTest() {
    when(file.getSize()).thenReturn(new Long(15000));
    when(document.getName()).thenReturn("name");
    when(document.getDescription()).thenReturn("description");
    when(document.getFile()).thenReturn(file);
    when(document.getEntityId()).thenReturn("providerId");
    when(file.isEmpty()).thenReturn(false);
    when(file.getOriginalFilename()).thenReturn("originalFileName");
    when(documentFileService.checkDuplicateFilename("providerId", "originalFileName")).thenReturn(true);

    validator.validate(document, errors);

    verify(errors, times(1)).rejectValue(eq("file"), eq(DocumentFileValidator.ERROR_CODE_FILE_ALREADY_EXISTS));
  }

  @Test
  public void fileFilenamePatternTest() {
    when(file.getSize()).thenReturn(new Long(15000));
    when(document.getName()).thenReturn("name");
    when(document.getDescription()).thenReturn("description");
    when(document.getFile()).thenReturn(file);
    when(document.getEntityId()).thenReturn("providerId");
    when(file.isEmpty()).thenReturn(false);
    when(file.getOriginalFilename()).thenReturn("originalFileName with errors in name");
    when(documentFileService.checkDuplicateFilename("providerId", "originalFileName")).thenReturn(false);

    validator.validate(document, errors);

    verify(errors, times(1)).rejectValue(eq("file"), eq(DocumentFileValidator.ERROR_CODE_FILENAME_FORMAT));
  }

}
