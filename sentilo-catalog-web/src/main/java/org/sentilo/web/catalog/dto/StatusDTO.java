package org.sentilo.web.catalog.dto;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

public class StatusDTO {

  @JsonInclude(value = Include.NON_EMPTY)
  private List<StatusItemDTO> items;

  @JsonInclude(value = Include.NON_NULL)
  private Boolean isPlatformRunningProperly;

  public StatusDTO() {
    super();
    items = new ArrayList<StatusItemDTO>();
  }

  public void addItem(final StatusItemDTO item) {
    items.add(item);
    checkGlobalState(item);
  }

  public List<StatusItemDTO> getItems() {
    return items;
  }

  private void checkGlobalState(final StatusItemDTO item) {
    isPlatformRunningProperly = isPlatformRunningProperly == null ? item.getStatus() : isPlatformRunningProperly & item.getStatus();
  }

}
