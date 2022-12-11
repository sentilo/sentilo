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
package org.sentilo.web.catalog.test.security.service;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.powermock.api.mockito.PowerMockito.mockStatic;

import java.util.Arrays;
import java.util.Collection;
import java.util.concurrent.atomic.AtomicInteger;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.web.catalog.context.TenantContextHolder;
import org.sentilo.web.catalog.context.TenantContextImpl;
import org.sentilo.web.catalog.domain.User;
import org.sentilo.web.catalog.exception.UserLoginNotAllowedException;
import org.sentilo.web.catalog.security.CatalogUserDetails;
import org.sentilo.web.catalog.security.Role;
import org.sentilo.web.catalog.security.SecurityUtils;
import org.sentilo.web.catalog.security.service.impl.CatalogUserDetailsServiceImpl;
import org.sentilo.web.catalog.service.UserService;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UsernameNotFoundException;

@RunWith(PowerMockRunner.class)
@PrepareForTest(SecurityUtils.class)
public class CatalogUserDetailsServiceImplTest {

  static AtomicInteger usersCounter = new AtomicInteger(1);
  private final String userName = "mockName";
  private final String mockTenant = "mockTenant";

  @Mock
  private UserService userService;

  @Mock
  private User user;

  @Mock
  private Authentication authentication;

  @Mock
  private CatalogUserDetails mockUserDetails;

  @InjectMocks
  private CatalogUserDetailsServiceImpl service;

  @Before
  public void setUp() {
    TenantContextHolder.clearContext();
    SecurityContextHolder.clearContext();

    MockitoAnnotations.initMocks(this);
    mockStatic(SecurityUtils.class);
    when(user.isActive()).thenReturn(true);
  }

  @After
  public void tearDown() {
    System.clearProperty(SentiloConstants.SENTILO_MULTITENANT_PROP_KEY);
    TenantContextHolder.clearContext();
    SecurityContextHolder.clearContext();
  }

  @Test
  public void loadUserByUsername() {
    final String userNameKey = getMockUserId();
    System.setProperty(SentiloConstants.SENTILO_MULTITENANT_PROP_KEY, Boolean.FALSE.toString());
    when(userService.find(new User(userNameKey))).thenReturn(user);

    service.loadUserByUsername(userNameKey);

    verify(userService).find(new User(userNameKey));
  }

  @Test(expected = UsernameNotFoundException.class)
  public void loadNoValidUser() {
    final String userNameKey = getMockUserId();
    when(userService.find(new User(userNameKey))).thenReturn(null);

    service.loadUserByUsername(userNameKey);

    verify(userService).find(new User(userNameKey));
  }

  @Test(expected = UserLoginNotAllowedException.class)
  public void loadSuperAdminUserAndMultitenantDisabled() {
    final String userNameKey = getMockUserId();
    System.setProperty(SentiloConstants.SENTILO_MULTITENANT_PROP_KEY, Boolean.FALSE.toString());
    when(userService.find(new User(userNameKey))).thenReturn(user);
    when(user.getRoles()).thenReturn(Arrays.asList(new Role[] {Role.SUPER_ADMIN}));

    service.loadUserByUsername(userNameKey);

    verify(userService).find(new User(userNameKey));
  }

  @Test
  public void loadSuperAdminUser() {
    final String userNameKey = getMockUserId();
    System.setProperty(SentiloConstants.SENTILO_MULTITENANT_PROP_KEY, Boolean.TRUE.toString());
    TenantContextHolder.clearContext();
    when(userService.find(new User(userNameKey))).thenReturn(user);
    when(user.getRoles()).thenReturn(Arrays.asList(new Role[] {Role.SUPER_ADMIN}));

    final CatalogUserDetails userDetails = (CatalogUserDetails) service.loadUserByUsername(userNameKey);

    verify(userService).find(new User(userNameKey));
    Assert.assertTrue(userDetails.isSuperAdminUser());
  }

  @Test(expected = UserLoginNotAllowedException.class)
  public void rejectSuperAdminAccessToTenantSite() {
    final String userNameKey = getMockUserId();
    System.setProperty(SentiloConstants.SENTILO_MULTITENANT_PROP_KEY, Boolean.TRUE.toString());
    TenantContextHolder.setContext(new TenantContextImpl(mockTenant));
    when(userService.find(new User(userNameKey))).thenReturn(user);
    when(user.getRoles()).thenReturn(Arrays.asList(new Role[] {Role.SUPER_ADMIN}));

    final CatalogUserDetails userDetails = (CatalogUserDetails) service.loadUserByUsername(userNameKey);

    verify(userService).find(new User(userNameKey));
    Assert.assertTrue(userDetails.isSuperAdminUser());
  }

  @Test(expected = UserLoginNotAllowedException.class)
  public void loadUserFromDiffTenant() {
    final String userNameKey = getMockUserId();
    System.setProperty(SentiloConstants.SENTILO_MULTITENANT_PROP_KEY, Boolean.TRUE.toString());
    TenantContextHolder.setContext(new TenantContextImpl(mockTenant));
    when(userService.find(new User(userNameKey))).thenReturn(user);
    when(user.getTenantId()).thenReturn("tenantUser");
    when(user.getRoles()).thenReturn(Arrays.asList(new Role[] {Role.ADMIN}));

    service.loadUserByUsername(userNameKey);

    verify(userService).find(new User(userNameKey));
  }

  @Test
  public void loadUserFromTenant() {
    final String userNameKey = getMockUserId();
    System.setProperty(SentiloConstants.SENTILO_MULTITENANT_PROP_KEY, Boolean.TRUE.toString());
    TenantContextHolder.setContext(new TenantContextImpl(mockTenant));
    when(userService.find(new User(userNameKey))).thenReturn(user);
    when(user.getTenantId()).thenReturn(mockTenant);
    when(user.getRoles()).thenReturn(Arrays.asList(new Role[] {Role.ADMIN}));

    service.loadUserByUsername(userNameKey);

    verify(userService).find(new User(userNameKey));
  }

  @Test
  public void getCatalogUserDetails() {
    SecurityContextHolder.getContext().setAuthentication(authentication);
    when(authentication.getPrincipal()).thenReturn(mockUserDetails);

    final CatalogUserDetails userDetails = service.getCatalogUserDetails();

    Assert.assertEquals(mockUserDetails, userDetails);
  }

  @Test
  public void getNullCatalogUserDetails() {
    SecurityContextHolder.getContext().setAuthentication(authentication);
    when(authentication.getPrincipal()).thenReturn(new UserDetails() {

      /**
       *
       */
      private static final long serialVersionUID = 1L;

      @Override
      public boolean isEnabled() {
        return true;
      }

      @Override
      public boolean isCredentialsNonExpired() {
        return true;
      }

      @Override
      public boolean isAccountNonLocked() {
        return true;
      }

      @Override
      public boolean isAccountNonExpired() {
        return true;
      }

      @Override
      public String getUsername() {
        return null;
      }

      @Override
      public String getPassword() {
        return null;
      }

      @Override
      public Collection<? extends GrantedAuthority> getAuthorities() {
        return null;
      }
    });

    Assert.assertNull(service.getCatalogUserDetails());
  }

  private String getMockUserId() {
    return userName + "_" + usersCounter.getAndIncrement();
  }

}
