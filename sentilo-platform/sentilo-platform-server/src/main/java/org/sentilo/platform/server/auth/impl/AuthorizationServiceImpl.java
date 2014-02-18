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
package org.sentilo.platform.server.auth.impl;

import java.util.HashSet;
import java.util.Set;

import org.sentilo.platform.common.domain.PermissionMessage;
import org.sentilo.platform.common.domain.PermissionsMessage;
import org.sentilo.platform.common.exception.CatalogAccessException;
import org.sentilo.platform.common.service.CatalogService;
import org.sentilo.platform.server.auth.AuthorizationService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import org.springframework.util.Assert;
import org.springframework.util.CollectionUtils;

@Service
public class AuthorizationServiceImpl implements AuthorizationService {

  private final Logger logger = LoggerFactory.getLogger(AuthorizationServiceImpl.class);

  private final Set<String> writesAccessSet = new HashSet<String>();
  private final Set<String> readsAccessSet = new HashSet<String>();

  @Autowired
  private CatalogService catalogService;

  public AuthorizationServiceImpl() {
    super();
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.sentilo.platform.server.auth.AuthorizationService#hasAccessToRead(java.lang.String,
   * java.lang.String)
   */
  public boolean hasAccessToRead(final String source, final String target) {
    final String keyAccess = buildKeyAccess(source, target);
    return readsAccessSet.contains(keyAccess) || writesAccessSet.contains(keyAccess);
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.sentilo.platform.server.auth.AuthorizationService#hasAccessToWrite(java.lang.String,
   * java.lang.String)
   */
  public boolean hasAccessToWrite(final String source, final String target) {
    return writesAccessSet.contains(buildKeyAccess(source, target));
  }

  private String buildKeyAccess(final String source, final String target) {
    Assert.notNull(source);
    Assert.notNull(target);

    return source + "#@#" + target;
  }

  @Scheduled(initialDelay = 1000, fixedRate = 300000)
  public void loadActivePermissions() {
    try {
      logger.debug("Actualizando cache de permisos");
      final PermissionsMessage permissions = catalogService.getPermissions();
      final Set<String> auxWritesAccessSet = new HashSet<String>();
      final Set<String> auxReadsAccessSet = new HashSet<String>();

      if (permissions != null && !CollectionUtils.isEmpty(permissions.getPermissions())) {
        for (final PermissionMessage permission : permissions.getPermissions()) {
          final String key = buildKeyAccess(permission.getSource(), permission.getTarget());
          if (permission.isWritePermission()) {
            auxWritesAccessSet.add(key);
          } else {
            auxReadsAccessSet.add(key);
          }
        }
      }

      replaceActivePermissions(auxWritesAccessSet, auxReadsAccessSet);

    } catch (final CatalogAccessException e) {
      logger.warn("Error al llamar al catalogo para recuperar la lista de autorizaciones", e);
    }
  }

  private void replaceActivePermissions(final Set<String> updateWritesAccesSet, final Set<String> updateReadsAccesSet) {
    writesAccessSet.clear();
    readsAccessSet.clear();
    writesAccessSet.addAll(updateWritesAccesSet);
    readsAccessSet.addAll(updateReadsAccesSet);
  }

}
