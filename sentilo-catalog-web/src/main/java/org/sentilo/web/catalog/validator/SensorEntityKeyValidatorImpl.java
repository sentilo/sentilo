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
package org.sentilo.web.catalog.validator;

import org.sentilo.web.catalog.exception.builder.DuplicateKeyExceptionBuilder;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.service.SensorService;
import org.sentilo.web.catalog.utils.CatalogUtils;
import org.sentilo.web.catalog.utils.Constants;
import org.springframework.dao.DuplicateKeyException;
import org.springframework.util.Assert;
import org.springframework.util.CollectionUtils;

/**
 * Sensor specific key validation constraint: one provider cannot have two sensors with same name
 */
public class SensorEntityKeyValidatorImpl implements EntityKeyValidator {

  private final SensorService sensorService;
  private final DuplicateKeyExceptionBuilder duplicateKeyExceptionBuilder;

  public SensorEntityKeyValidatorImpl(final SensorService sensorService, final DuplicateKeyExceptionBuilder duplicateKeyExceptionBuilder) {
    Assert.notNull(sensorService);
    this.sensorService = sensorService;
    this.duplicateKeyExceptionBuilder = duplicateKeyExceptionBuilder;
  }

  @Override
  public void checkIntegrityKey(final String idToCheck) throws DuplicateKeyException {
    // idToCheck have the format: providerId.componentId.sensorId
    final String[] tokens = idToCheck.split(CatalogUtils.escapeRegexCharacter(Constants.DEFAULT_KEY_TOKEN_SPLITTER));
    final SearchFilter filter = new SearchFilter();
    filter.addAndParam("providerId", tokens[0]);
    filter.addAndParam("sensorId", tokens[2]);
    if (!CollectionUtils.isEmpty(sensorService.search(filter).getContent())) {
      duplicateKeyExceptionBuilder.buildDuplicateKeyException(idToCheck);
    }

  }

}
