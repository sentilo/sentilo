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
package org.sentilo.web.catalog.security;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.sentilo.web.catalog.domain.User;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;

public class CatalogUserDetails implements UserDetails {

  private static final long serialVersionUID = 1L;

  private final User user;
  private final String tenantId;
  private final List<GrantedAuthority> authorities;

  public CatalogUserDetails(final User user) {
    this.user = user;
    tenantId = user.getTenantId();
    authorities = new ArrayList<GrantedAuthority>();
    for (final Role role : user.getRoles()) {
      authorities.add(new SimpleGrantedAuthority(role.toString()));
    }
  }

  @Override
  public Collection<? extends GrantedAuthority> getAuthorities() {
    return authorities;
  }

  @Override
  public String getPassword() {
    return user.getPassword();
  }

  @Override
  public String getUsername() {
    return user.getUserName();
  }

  @Override
  public boolean isAccountNonExpired() {
    return user.isActive();
  }

  @Override
  public boolean isAccountNonLocked() {
    return user.isActive();
  }

  @Override
  public boolean isCredentialsNonExpired() {
    return user.isActive();
  }

  @Override
  public boolean isEnabled() {
    return user.isActive();
  }

  public String getTenantId() {
    return tenantId;
  }

  public boolean isSuperAdminUser() {
    return authorities.contains(new SimpleGrantedAuthority(Role.SUPER_ADMIN.toString()));
  }

  public boolean isAdminUser() {
    return authorities.contains(new SimpleGrantedAuthority(Role.ADMIN.toString()));
  }

  public boolean isUser() {
    return authorities.contains(new SimpleGrantedAuthority(Role.USER.toString()));
  }

  public boolean isPlatformUser() {
    return authorities.contains(new SimpleGrantedAuthority(Role.PLATFORM.toString()));
  }
}
