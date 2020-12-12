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
package org.sentilo.platform.server.auth.impl;

import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import org.sentilo.common.utils.SentiloConstants;
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

  private static final Logger LOGGER = LoggerFactory.getLogger(AuthorizationServiceImpl.class);

  private final Set<String> adminAccessSet = new HashSet<String>();
  private final Set<String> writesAccessSet = new HashSet<String>();
  private final Set<String> readsAccessSet = new HashSet<String>();

  private final Lock lock = new ReentrantLock();

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
    lock.lock();
    try {
      final String keyAccess = buildKeyAccess(source, target);
      return readsAccessSet.contains(keyAccess) || writesAccessSet.contains(keyAccess) || adminAccessSet.contains(keyAccess);
    } finally {
      lock.unlock();
    }
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.platform.server.auth.AuthorizationService#hasAccessToWrite(java.lang.String,
   * java.lang.String)
   */
  public boolean hasAccessToWrite(final String source, final String target) {
    lock.lock();
    try {
      final String keyAccess = buildKeyAccess(source, target);
      return writesAccessSet.contains(keyAccess) || adminAccessSet.contains(keyAccess);
    } finally {
      lock.unlock();
    }
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.platform.server.auth.AuthorizationService#hasAccessToAdmin(java.lang.String,
   * java.lang.String)
   */
  public boolean hasAccessToAdmin(final String source, final String target) {
    lock.lock();
    try {
      return adminAccessSet.contains(buildKeyAccess(source, target));
    } finally {
      lock.unlock();
    }
  }

  private String buildKeyAccess(final String source, final String target) {
    Assert.notNull(source, "source argument is required; it must not be null.");
    Assert.notNull(target, "target argument is required; it must not be null.");

    return source + SentiloConstants.SENTILO_INTERNAL_TOKEN + target;
  }

  @Scheduled(initialDelay = 1000, fixedRate = 300000)
  public void loadActivePermissions() {
    try {
      LOGGER.info("Updating permissions cache");
      final PermissionsMessage permissions = catalogService.getPermissions();
      final Set<String> auxAdminAccessSet = new HashSet<String>();
      final Set<String> auxWritesAccessSet = new HashSet<String>();
      final Set<String> auxReadsAccessSet = new HashSet<String>();

      if (permissions != null && !CollectionUtils.isEmpty(permissions.getPermissions())) {
        for (final PermissionMessage permission : permissions.getPermissions()) {
          final String key = buildKeyAccess(permission.getSource(), permission.getTarget());
          switch (permission.getType()) {
            case ADMIN:
              auxAdminAccessSet.add(key);
              break;
            case WRITE:
              auxWritesAccessSet.add(key);
              break;
            case READ:
              auxReadsAccessSet.add(key);
              break;
            default:
              break;
          }
        }
      }

      replaceActivePermissions(auxAdminAccessSet, auxWritesAccessSet, auxReadsAccessSet);
      LOGGER.info("Process finished successfully");

    } catch (final CatalogAccessException e) {
      LOGGER.warn("Error updating permissions cache", e);
    }
  }

  private void replaceActivePermissions(final Set<String> updateAdminsAccesSet, final Set<String> updateWritesAccesSet,
      final Set<String> updateReadsAccesSet) {

    lock.lock();
    try {
      adminAccessSet.clear();
      writesAccessSet.clear();
      readsAccessSet.clear();
      adminAccessSet.addAll(updateAdminsAccesSet);
      writesAccessSet.addAll(updateWritesAccesSet);
      readsAccessSet.addAll(updateReadsAccesSet);
    } finally {
      lock.unlock();
    }
  }

}
