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

import org.sentilo.web.catalog.domain.CatalogDocument;
import org.sentilo.web.catalog.exception.builder.DefaultCatalogBuilderExceptionImpl;
import org.sentilo.web.catalog.exception.builder.DuplicateKeyExceptionBuilder;
import org.springframework.dao.DuplicateKeyException;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.util.Assert;

public class DefaultEntityKeyValidatorImpl implements EntityKeyValidator {

  private final MongoRepository<? extends CatalogDocument, String> repository;
  private final DuplicateKeyExceptionBuilder duplicateKeyExceptionBuilder;

  public DefaultEntityKeyValidatorImpl(final Class<? extends CatalogDocument> type,
      final MongoRepository<? extends CatalogDocument, String> repository) {
    Assert.notNull(repository);
    this.repository = repository;
    duplicateKeyExceptionBuilder = new DefaultCatalogBuilderExceptionImpl(type);
  }

  public DefaultEntityKeyValidatorImpl(final MongoRepository<? extends CatalogDocument, String> repository,
      final DuplicateKeyExceptionBuilder duplicateKeyExceptionBuilder) {
    Assert.notNull(repository);
    this.repository = repository;
    this.duplicateKeyExceptionBuilder = duplicateKeyExceptionBuilder;
  }

  @Override
  public void checkIntegrityKey(final String idToCheck) throws DuplicateKeyException {
    if (repository.findOne(idToCheck) != null) {
      duplicateKeyExceptionBuilder.buildDuplicateKeyException(idToCheck);
    }

  }

}
