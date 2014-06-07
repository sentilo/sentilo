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
package org.sentilo.web.catalog.service.impl;

import java.util.Collection;

import org.sentilo.web.catalog.domain.Application;
import org.sentilo.web.catalog.repository.ApplicationRepository;
import org.sentilo.web.catalog.service.ApplicationService;
import org.sentilo.web.catalog.service.PermissionService;
import org.sentilo.web.catalog.utils.IdentityKeyGenerator;
import org.sentilo.web.catalog.validator.EntityKeyValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

@Service
public class ApplicationServiceImpl extends AbstractBaseServiceImpl<Application> implements ApplicationService {

  @Autowired
  private ApplicationRepository repository;

  @Autowired
  private PermissionService permissionService;

  @Autowired
  @Qualifier("appsAndProvidersKeyValidator")
  private EntityKeyValidator customEntityValidator;

  public ApplicationServiceImpl() {
    super(Application.class);
  }

  @Override
  protected void doAfterInit() {
    setEntityKeyValidator(customEntityValidator);
    super.doAfterInit();
  }

  @Override
  public ApplicationRepository getRepository() {
    return repository;
  }

  @Override
  public String getEntityId(final Application entity) {
    return entity.getId();
  }

  @Override
  public Application create(final Application application) {
    // El identificador se informa por pantalla (es obligatorio). El nombre, en caso de no estar
    // informado, se rellena con el valor del identificador.
    if (!StringUtils.hasText(application.getName())) {
      application.setName(application.getId());
    }

    // Validamos la unicidad del identificador: no puede existir otra entidad (app o provider) con
    // el mismo identificador.
    checkIntegrityKey(application.getId());

    application.setToken(IdentityKeyGenerator.generateNewToken(application.getId()));

    permissionService.createRelated(application);
    return getRepository().save(application);
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.sentilo.web.catalog.service.impl.AbstractBaseServiceImpl#delete(java.lang.Object)
   */
  public void delete(final Application application) {
    super.delete(application);
    permissionService.deleteRelated(application);
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.sentilo.web.catalog.service.impl.AbstractBaseServiceImpl#delete(java.util.Collection)
   */
  public void delete(final Collection<Application> applications) {
    super.delete(applications);
    for (final Application application : applications) {
      permissionService.deleteRelated(application);
    }
  }
}
