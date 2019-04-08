package org.sentilo.agent.activity.monitor.repository.batch.support;

import java.util.Map;

import org.springframework.util.CollectionUtils;

public class Item {

  /**
   * For ES v2, response for the op_type "index" is create and in newer versions it's index
   */
  private Map<String, Object> create;
  private Map<String, Object> index;

  public Item() {
    super();
  }

  public Integer getStatus() {
    Integer status = null;
    if (!CollectionUtils.isEmpty(create) || !CollectionUtils.isEmpty(index)) {
      status = CollectionUtils.isEmpty(create) ? (Integer) getIndex().get("status") : (Integer) getCreate().get("status");
    }
    return status;
  }

  public Map<String, Object> getIndex() {
    return index;
  }

  public void setIndex(final Map<String, Object> index) {
    this.index = index;
  }

  public Map<String, Object> getCreate() {
    return create;
  }

  public void setCreate(final Map<String, Object> create) {
    this.create = create;
  }
}
