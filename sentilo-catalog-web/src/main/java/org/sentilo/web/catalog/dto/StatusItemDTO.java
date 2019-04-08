package org.sentilo.web.catalog.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

public class StatusItemDTO implements Comparable<StatusItemDTO> {

  private transient int order;
  @JsonInclude(value = Include.NON_NULL)
  private String id;
  @JsonInclude(value = Include.NON_NULL)
  private String name;
  @JsonInclude(value = Include.NON_NULL)
  private String description;
  @JsonInclude(value = Include.NON_NULL)
  private boolean status;
  @JsonInclude(value = Include.NON_NULL)
  private String stateDesc;

  public StatusItemDTO(final int order, final String name, final String description) {
    this.order = order;
    id = "sentilo-component-" + order;
    this.name = name;
    this.description = description;
  }

  public StatusItemDTO(final int order, final String name, final String description, final boolean status) {
    this(order, name, description);
    this.status = status;
  }

  public StatusItemDTO(final int order, final String name, final String description, final boolean status, final String stateDesc) {
    this(order, name, description, status);
    this.stateDesc = stateDesc;
  }

  @Override
  public int compareTo(final StatusItemDTO obj) {
    return Integer.valueOf(order).compareTo(Integer.valueOf(obj.getOrder()));
  }

  public String getName() {
    return name;
  }

  public void setName(final String name) {
    this.name = name;
  }

  public boolean getStatus() {
    return status;
  }

  public void setStatus(final boolean status) {
    this.status = status;
  }

  public String getStateDesc() {
    return stateDesc;
  }

  public void setStateDesc(final String stateDesc) {
    this.stateDesc = stateDesc;
  }

  public int getOrder() {
    return order;
  }

  public void setOrder(final int order) {
    this.order = order;
  }

  public String getDescription() {
    return description;
  }

  public void setDescription(final String description) {
    this.description = description;
  }

}
