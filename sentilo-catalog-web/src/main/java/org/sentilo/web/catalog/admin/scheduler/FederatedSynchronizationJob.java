package org.sentilo.web.catalog.admin.scheduler;

import java.util.Arrays;

import org.sentilo.web.catalog.admin.service.FederatedSynchronizationService;
import org.sentilo.web.catalog.domain.User;
import org.sentilo.web.catalog.security.CatalogUserDetails;
import org.sentilo.web.catalog.security.Role;
import org.sentilo.web.catalog.security.SecurityUtils;
import org.sentilo.web.catalog.utils.Constants;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;

/**
 * This job defines the process to import and synchronize into the catalog the resources from
 * federated Sentilo services.
 */
@Component
public class FederatedSynchronizationJob {

  final static int ONE_MINUTE = 60 * 1000; // 1 minute
  final static int FIXED_DELAY = 10 * ONE_MINUTE; // 10 minutes

  @Autowired
  private FederatedSynchronizationService service;

  @Scheduled(initialDelay = ONE_MINUTE, fixedDelay = FIXED_DELAY)
  public void syncFederatedServices() {
    if (SecurityUtils.isFederationEnabled()) {
      setUpContext();
      service.syncCatalogs();
    }
  }

  private void setUpContext() {
    registerAuthenticateUser();
  }

  /**
   * As some internal methods rely on fetching the SecurityContext and check that user is
   * authenticated (for audit actions purposes), we set a custom user to identify a batch process
   */
  private void registerAuthenticateUser() {
    final Role[] roles = {Role.ADMIN};
    final User user = new User(Constants.BATCH_USER);
    user.setActive(true);
    user.setRoles(Arrays.asList(roles));
    final CatalogUserDetails cud = new CatalogUserDetails(user);

    final UsernamePasswordAuthenticationToken result = new UsernamePasswordAuthenticationToken(cud, cud.getPassword(), cud.getAuthorities());
    SecurityContextHolder.getContext().setAuthentication(result);
  }

}
