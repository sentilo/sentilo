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
package org.sentilo.web.catalog.admin.upgrade;

import java.util.List;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import org.sentilo.web.catalog.domain.User;
import org.sentilo.web.catalog.security.crypto.SentiloDelegatingPasswordEncoder;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.stereotype.Component;

@Component
public class UpgradeDatabaseHook implements InitializingBean {

  protected static final Logger LOGGER = LoggerFactory.getLogger(UpgradeDatabaseHook.class);

  @Autowired
  private MongoTemplate mongoTemplate;

  @Autowired
  private SentiloDelegatingPasswordEncoder passwordEncoder;

  private final static String COLLECTION_NAME = "sentiloUpgradeControl";

  /** See BCryptPasswordEncoder#BCRYPT_PATTERN */
  private final Pattern BCRYPT_PATTERN = Pattern.compile("\\A\\$2a?\\$\\d\\d\\$[./0-9A-Za-z]{53}");

  public enum ItemState {
    FINISHED, RUNNING, ERROR
  }

  @Override
  public void afterPropertiesSet() throws Exception {
    final long startTime = System.currentTimeMillis();
    LOGGER.info("*******************  Starting UpgradeDatabase process ***************** ");
    final boolean exists = mongoTemplate.collectionExists(COLLECTION_NAME);
    if (!exists) {
      mongoTemplate.createCollection(COLLECTION_NAME);
    }

    apply_v18_Changes();

    LOGGER.info("*******************  Finished UpgradeDatabase process in {} ms. ***************** ", System.currentTimeMillis() - startTime);
  }

  private void apply_v18_Changes() throws Exception {
    if (passwordEncoder.encodePasswords()) {
      encodePasswords();
    } else {
      LOGGER.warn("Catalog is configured to use plain text passwords!!. It is a bad practice to run this configuration in a "
          + "production environment. Instead configure catalog to use Bcrypt encoder");
    }
  }

  private void encodePasswords() {
    final String v18_passwords_item_key = "encode_user_passwords.v18.sentilo";

    // Check if changes associated with 1.8 version have already been applied in a previous
    // execution

    final UpgradeDatabaseItem v18_passwords_item = getItem(v18_passwords_item_key);
    if (!(ItemState.FINISHED.equals(v18_passwords_item.getState()) || ItemState.RUNNING.equals(v18_passwords_item.getState()))) {
      try {
        v18_passwords_item.changeState(ItemState.RUNNING);
        mongoTemplate.save(v18_passwords_item, COLLECTION_NAME);

        final List<User> users = mongoTemplate.findAll(User.class);
        final List<User> usersToSave = users.parallelStream().filter(user -> !isPasswordBCryptEncoded(user))
            .peek(user -> user.setPassword(passwordEncoder.encode(user.getPassword()))).collect(Collectors.toList());

        LOGGER.info(" {} user passwords transformed. ", usersToSave.size());

        usersToSave.forEach(user -> mongoTemplate.save(user));

        v18_passwords_item.changeState(ItemState.FINISHED);
        mongoTemplate.save(v18_passwords_item, COLLECTION_NAME);
      } catch (final Exception e) {
        LOGGER.error("An error has been raised while encoding user passwords", e);
        v18_passwords_item.changeState(ItemState.ERROR, e.getMessage());
        mongoTemplate.save(v18_passwords_item, COLLECTION_NAME);
        throw e;
      }
    }
  }

  private boolean isPasswordBCryptEncoded(final User user) {
    boolean encoded = true;

    if (!BCRYPT_PATTERN.matcher(user.getPassword()).matches()) {
      LOGGER.trace("Password for user {} will be encoded with BCrypt algorithm", user.getUserName());
      encoded = false;
    }

    return encoded;
  }

  private UpgradeDatabaseItem getItem(final String key) {
    final UpgradeDatabaseItem item = mongoTemplate.findById(key, UpgradeDatabaseItem.class, COLLECTION_NAME);
    return item == null ? new UpgradeDatabaseItem(key) : item;
  }

}
