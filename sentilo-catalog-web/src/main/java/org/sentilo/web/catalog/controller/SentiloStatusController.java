package org.sentilo.web.catalog.controller;

import java.util.Collections;

import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.web.catalog.domain.User;
import org.sentilo.web.catalog.dto.StatusDTO;
import org.sentilo.web.catalog.dto.StatusItemDTO;
import org.sentilo.web.catalog.exception.CatalogException;
import org.sentilo.web.catalog.service.PlatformService;
import org.sentilo.web.catalog.utils.Constants;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.MessageSource;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.data.mongodb.core.MongoOperations;
import org.springframework.stereotype.Controller;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

@Controller
@RequestMapping("/status")
public class SentiloStatusController extends CatalogBaseController {

  @Autowired
  private MongoOperations mongoOps;

  @Autowired
  private PlatformService platformService;

  @Autowired
  private MessageSource messageSource;

  @RequestMapping(method = RequestMethod.GET)
  public String getStatusPage() {
    // If status page flag is activated, returns the status page path. Otherwise returns a page
    // that describe that status page is disabled
    return statusPageDisabled() ? Constants.VIEW_STATE_DISABLED : Constants.VIEW_STATE_ENABLED;
  }

  @RequestMapping(value = "/json", method = RequestMethod.GET)
  @ResponseBody
  public StatusDTO getPlatformStatus() {
    final StatusDTO sentiloStatus = new StatusDTO();

    if (!statusPageDisabled()) {
      sentiloStatus.addItem(checkMongoDBStatus());
      sentiloStatus.addItem(checkPubSubStatus());
      sentiloStatus.addItem(checkRedisStatus());

      Collections.sort(sentiloStatus.getItems());
    }

    return sentiloStatus;
  }

  public static boolean statusPageDisabled() {
    final String propValue = System.getProperty(SentiloConstants.SENTILO_STATE_PAGE_ENABLED_PROP_KEY);
    return StringUtils.hasText(propValue) && !Boolean.valueOf(propValue);
  }

  private StatusItemDTO checkMongoDBStatus() {
    boolean isOk = true;

    final String description = messageSource.getMessage("status.mongo.desc", new Object[] {}, LocaleContextHolder.getLocale());
    final String errorDesc = messageSource.getMessage("status.mongo.desc.error", new Object[] {}, LocaleContextHolder.getLocale());
    final String successDesc = messageSource.getMessage("status.mongo.desc.success", new Object[] {}, LocaleContextHolder.getLocale());

    try {
      mongoOps.collectionExists(User.class);
    } catch (final Exception e) {
      isOk = false;
    }

    return new StatusItemDTO(1, "MongoDB database", description, isOk, isOk ? successDesc : errorDesc);
  }

  private StatusItemDTO checkRedisStatus() {
    boolean isOk = true;

    try {
      platformService.getPlatformPerformance();
    } catch (final CatalogException e) {
      isOk = false;
    }

    final String description = messageSource.getMessage("status.redis.desc", new Object[] {}, LocaleContextHolder.getLocale());
    final String errorDesc = messageSource.getMessage("status.redis.desc.error", new Object[] {}, LocaleContextHolder.getLocale());
    final String successDesc = messageSource.getMessage("status.redis.desc.success", new Object[] {}, LocaleContextHolder.getLocale());

    return new StatusItemDTO(3, "Redis database", description, isOk, isOk ? successDesc : errorDesc);
  }

  private StatusItemDTO checkPubSubStatus() {
    final boolean isOk = platformService.isPlatformRunning();

    final String description = messageSource.getMessage("status.pubsub.desc", new Object[] {}, LocaleContextHolder.getLocale());
    final String errorDesc = messageSource.getMessage("status.pubsub.desc.error", new Object[] {}, LocaleContextHolder.getLocale());
    final String successDesc = messageSource.getMessage("status.pubsub.desc.success", new Object[] {}, LocaleContextHolder.getLocale());

    return new StatusItemDTO(2, "Sentilo API REST server", description, isOk, isOk ? successDesc : errorDesc);
  }
}
