package org.sentilo.web.catalog.controller.admin;

import java.util.ArrayList;
import java.util.List;

import org.sentilo.web.catalog.controller.CrudController;
import org.sentilo.web.catalog.domain.ActiveSubscription;
import org.sentilo.web.catalog.domain.CatalogDocument;
import org.sentilo.web.catalog.service.ActiveSubscriptionsService;
import org.sentilo.web.catalog.service.CrudService;
import org.sentilo.web.catalog.utils.Constants;
import org.sentilo.web.catalog.utils.ExcelGeneratorUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestMapping;

@Controller
@RequestMapping("/admin/activesubscriptions")
public class ActiveSubscriptionsController extends CrudController<ActiveSubscription> {

  @Autowired
  private ActiveSubscriptionsService activeSubscriptionsService;

  @ModelAttribute(Constants.MODEL_ACTIVE_MENU)
  public String getActiveMenu() {
    return Constants.MENU_ACTIVE_SUBSCRIPTIONS;
  }

  @Override
  protected CrudService<ActiveSubscription> getService() {
    return activeSubscriptionsService;
  }

  @Override
  protected List<String> toRow(final ActiveSubscription resource) {
    final List<String> row = new ArrayList<String>();
    row.add(""); // First column
    row.add(resource.getEntityId());
    row.add(resource.getEntityType().name());
    row.add(resource.getSubscriptionType());
    if (resource.getProvider() != null && resource.getProvider().endsWith("*")) {
      row.add(resource.getProvider().substring(0, resource.getProvider().length() - 1));
    } else {
      row.add(resource.getProvider());
    }
    row.add(resource.getSensor());
    row.add(resource.getEndpoint());
    row.add(String.valueOf(resource.getMaxRetries()));
    row.add(String.valueOf(resource.getRetryDelay()));
    return row;
  }

  @Override
  protected List<String> toExcelRow(final ActiveSubscription activeSubscription) {
    return ExcelGeneratorUtils.getActiveSubscriptionsExcelRowsData(activeSubscription, getLocalDateFormat(),
        userDetailsService.getCatalogUserDetails());
  }

  @Override
  protected void initViewNames() {
    getViewNames().put(LIST_ACTION, Constants.VIEW_ACTIVE_SUBSCRIPTIONS_LIST);
  }

  @Override
  protected Class<? extends CatalogDocument> getRowClass() {
    return ActiveSubscription.class;
  }

  @Override
  protected ActiveSubscription buildNewEntity(final String id) {
    return new ActiveSubscription(id);
  }

  @Override
  protected String getEntityModelKey() {
    return Constants.MODEL_ACTIVE_SUBSCRIPTIONS;
  }

}
